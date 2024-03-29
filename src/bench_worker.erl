-module(bench_worker).
-behaviour(gen_server).

%% API.
-export([start_link/2, close/1]).
-export([start_test/3, stop_test/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-define(VERSION, 1).
-define(TIMEOUT, 10000).

-record(state, {
	server = undefined :: pid(),
	id = undefined :: non_neg_integer(),
	socket = undefined :: inet:socket(),
	nextmid = undefined :: non_neg_integer(),
	enable = undefined :: boolean(),
	req = undefined :: undefined | ecoap_message:coap_message(),
	ep_addr = undefined :: undefined| {inet:ip_address(), inet:port_number()},
	sent = undefined :: non_neg_integer(),
	rec = undefined :: non_neg_integer(),
	timeout = undefined :: non_neg_integer(),
	timer = undefined :: undefined | reference(),
	timestamp = undefined :: undefined | integer(),
	hdr_ref = undefined :: binary(),
	worker_ref = undefined :: undefined | reference()
}).

%% API.

-spec start_link(inet:socket(), non_neg_integer()) -> {ok, pid()}.
start_link(Server, ID) ->
	gen_server:start_link(?MODULE, [Server, ID], []).

-spec close(pid()) -> ok.
close(Pid) ->
	gen_server:cast(Pid, shutdown).

-spec start_test(pid(), reference(), {string(), ecoap_message:coap_method(), binary()}) -> ok.
start_test(Pid, Ref, {Uri, Method, Payload}) ->
	gen_server:cast(Pid, {start_test, Ref, {Uri, Method, Payload}}).

-spec stop_test(pid()) -> ok.
stop_test(Pid) ->
	gen_server:cast(Pid, stop_test).

%% gen_server.

init([Server, ID]) ->
	% trap exit so that terminate/2 will be called when supervisor order us to terminate
	process_flag(trap_exit, true),
	{ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
	{ok, HDR_Ref} = hdr_histogram:open(3600000000, 3),
	{ok, #state{server=Server, socket=Socket, id=ID, nextmid=ecoap_message_id:first_mid(), enable=false, sent=0, rec=0, timeout=0, hdr_ref=HDR_Ref}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({start_test, Ref, {Uri, Method, Payload}}, State=#state{id=_ID, socket=Socket, nextmid=MsgId}) ->
	#{ip:=PeerIP, port:=PeerPortNo, path:=Path, 'query':=Query} = ecoap_uri:decode_uri(Uri),
	Options = ecoap_message:add_option('Uri-Query', Query, ecoap_message:add_option('Uri-Path', Path, #{})),
	Request = ecoap_message:set_id(MsgId, ecoap_request:request('CON', Method, Options, Payload)),
	ok = inet_udp:send(Socket, PeerIP, PeerPortNo, ecoap_message:encode(Request)),
	Timer = erlang:start_timer(?TIMEOUT, self(), req_timeout),
	{noreply, State#state{enable=true, req=Request, ep_addr={PeerIP, PeerPortNo}, sent=1, timer=Timer, timestamp=erlang:monotonic_time(), worker_ref=Ref}};

handle_cast(stop_test, State=#state{id=_ID, server=Server, sent=Sent, rec=Rec, timeout=TimeOut, hdr_ref=HDR_Ref, worker_ref=Ref}) ->
	gen_server:cast(Server, {result, self(), Ref, #{sent=>Sent, rec=>Rec, timeout=>TimeOut}, HDR_Ref}),
	% set enable to false so that we do not process incomming messages recevied from socket any more
	{noreply, State#state{enable=false}};

handle_cast(shutdown, State) ->
	{stop, normal, State};

handle_cast(_Msg, State=#state{id=ID}) ->
	io:fwrite("unexpected cast in bench_worker ~p: ~p~n", [ID, _Msg]),
	{noreply, State}.

% incoming ACK(2) response to a request with code {ok, _}
handle_info({udp, Socket, PeerIP, PeerPortNo, <<?VERSION:2, 2:2, _TKL:4, 2:3, _:5, MsgId:16, _/bytes>>}, 
	State=#state{enable=true, socket=Socket, nextmid=MsgId, req=Request, sent=Sent, rec=Rec, timer=Timer, timestamp=Timestamp, hdr_ref=HDR_Ref}) ->
	_ = erlang:cancel_timer(Timer, [{async, true}, {info, false}]),
	ok = hdr_histogram:record(HDR_Ref, erlang:convert_time_unit(erlang:monotonic_time() - Timestamp, native, micro_seconds)),
	NextMsgId = ecoap_message_id:next_mid(MsgId),
	ok = inet_udp:send(Socket, PeerIP, PeerPortNo, ecoap_message:encode(ecoap_message:set_id(NextMsgId, Request))),
	NewTimer = erlang:start_timer(?TIMEOUT, self(), req_timeout),
	{noreply, State#state{rec=Rec+1, sent=Sent+1, nextmid=NextMsgId, timer=NewTimer, timestamp=erlang:monotonic_time()}};

handle_info({udp, Socket, _PeerIP, _PeerPortNo, <<?VERSION:2, T:2, _TKL:4, Class:3, DetailedCode:5, MsgId:16, _/bytes>>}, 
	State=#state{enable=true, socket=Socket, nextmid=ExpectedMsgId}) ->
	% unexpected response, e.g. late response or unknown response code
	io:fwrite("Recv wrong msg, expect type:~p id:~p code:~p but recevied type:~p id:~p code:~p~n", 
	['ACK', ExpectedMsgId, '{2,xx}', ecoap_iana:decode_type(T), MsgId, {Class, DetailedCode}]),
	{noreply, State};

handle_info({timeout, Timer, req_timeout}, 
	State=#state{enable=true, ep_addr={PeerIP, PeerPortNo}, socket=Socket, req=Request, nextmid=MsgId, sent=Sent, timeout=TimeOut, timer=Timer}) ->
	NextMsgId = ecoap_message_id:next_mid(MsgId),
	ok = inet_udp:send(Socket, PeerIP, PeerPortNo, ecoap_message:encode(ecoap_message:set_id(NextMsgId, Request))),
	NewTimer = erlang:start_timer(?TIMEOUT, self(), req_timeout),
	{noreply, State#state{timeout=TimeOut+1, sent=Sent+1, nextmid=NextMsgId, timer=NewTimer, timestamp=erlang:monotonic_time()}};

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State=#state{socket=Socket, hdr_ref=HDR_Ref}) ->
	ok = hdr_histogram:close(HDR_Ref),
	ok = gen_udp:close(Socket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal
