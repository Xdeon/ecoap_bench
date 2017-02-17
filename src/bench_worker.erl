-module(bench_worker).
-behaviour(gen_server).

-include_lib("ecoap_common/include/coap_def.hrl").

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
-define(MAX_MESSAGE_ID, 65535). % 16-bit number
-define(TIMEOUT, 10000).

-record(state, {
	server = undefined :: pid(),
	id = undefined :: non_neg_integer(),
	socket = undefined :: inet:socket(),
	% msgid_store = undefined :: map(),
	nextmid = undefined :: non_neg_integer(),
	req = undefined :: undefined | coap_message(),
	ep_id = undefined :: undefined| {inet:ip_address(), inet:port_number()},
	sent = undefined :: non_neg_integer(),
	rec = undefined :: non_neg_integer(),
	timeout = undefined :: non_neg_integer(),
	timer = undefined :: undefined | reference(),
	timestamp = undefined :: undefined | integer(),
	hdr_ref = undefined :: undefined | binary(),
	worker_ref = undefined :: undefined | reference()
}).

%% API.

-spec start_link(inet:socket(), non_neg_integer()) -> {ok, pid()}.
start_link(Server, ID) ->
	gen_server:start_link(?MODULE, [Server, ID], []).

-spec close(pid()) -> ok.
close(Pid) ->
	gen_server:cast(Pid, shutdown).

start_test(Pid, Ref, {Method, Uri, Content}) ->
	gen_server:cast(Pid, {start_test, Ref, {Method, Uri, Content}}).

stop_test(Pid) ->
	gen_server:cast(Pid, stop_test).

%% gen_server.

init([Server, ID]) ->
	{ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
	{ok, #state{server=Server, socket=Socket, id=ID, nextmid=first_mid(), sent=0, rec=0, timeout=0}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({start_test, Ref, {Method, Uri, Content}}, State=#state{id=_ID, socket=Socket, nextmid=MsgId}) ->
	% io:format("worker ~p start_test~n", [_ID]),
	% io:format("start_test at ~p~n", [erlang:monotonic_time()]),
    {ok, HDR_Ref} = hdr_histogram:open(3600000000, 3),
	{EpID={PeerIP, PeerPortNo}, Path, Query} = resolve_uri(Uri),
	Options = coap_message_utils:append_option('Uri-Query', Query, coap_message_utils:append_option('Uri-Path', Path, [])),
	Request0 = coap_message_utils:request('CON', Method, Content, Options),
	Request1 = Request0#coap_message{id=MsgId},
	ok = inet_udp:send(Socket, PeerIP, PeerPortNo, coap_message:encode(Request1)),
	Timer = erlang:start_timer(?TIMEOUT, self(), req_timeout),
	{noreply, State#state{req=Request1, ep_id=EpID, sent=1, timer=Timer, timestamp=erlang:monotonic_time(), hdr_ref=HDR_Ref, worker_ref=Ref}};

handle_cast(stop_test, State=#state{id=_ID, server=Server, sent=Sent, rec=Rec, timeout=TimeOut, hdr_ref=HDR_Ref, worker_ref=Ref}) ->
	% io:format("worker ~p stop_test~n", [ID]),
	% io:format("stop_test at ~p~n", [erlang:monotonic_time()]),
	gen_server:cast(Server, {result, self(), Ref, #{sent=>Sent, rec=>Rec, timeout=>TimeOut}, HDR_Ref}),
	{stop, normal, State};

handle_cast(shutdown, State) ->
	{stop, normal, State};

handle_cast(_Msg, State=#state{id=ID}) ->
	io:format("unexpected cast in bench_worker ~p: ~p~n", [ID, _Msg]),
	{noreply, State}.

% incoming ACK(2) response to a request with code {ok, _}
handle_info({udp, Socket, PeerIP, PeerPortNo, <<?VERSION:2, 2:2, _TKL:4, 2:3, _:5, MsgId:16, _/bytes>>}, 
	State=#state{socket=Socket, req=Request, nextmid=MsgId, sent=Sent, rec=Rec, timer=Timer, timestamp=Timestamp, hdr_ref=HDR_Ref}) ->
	_ = erlang:cancel_timer(Timer),
	NextMsgId = next_mid(MsgId),
	ok = hdr_histogram:record(HDR_Ref, erlang:convert_time_unit(erlang:monotonic_time() - Timestamp, native, micro_seconds)),
	ok = inet_udp:send(Socket, PeerIP, PeerPortNo, coap_message:encode(Request#coap_message{id=NextMsgId})),
	NewTimer = erlang:start_timer(?TIMEOUT, self(), req_timeout),
	{noreply, State#state{rec=Rec+1, sent=Sent+1, nextmid=NextMsgId, timer=NewTimer, timestamp=erlang:monotonic_time()}};

handle_info({udp, Socket, _PeerIP, _PeerPortNo, <<?VERSION:2, T:2, _TKL:4, Class:3, DetailedCode:5, MsgId:16, _/bytes>>}, 
	State=#state{socket=Socket, nextmid=ExpectedMsgId}) ->
	io:format("Recv msg with type:~p id:~p code:~p, expect type:~p id:~p code:~p~n", 
		[coap_iana:decode_type(T), MsgId, {Class, DetailedCode}, 'ACK', ExpectedMsgId, '{2,xx}']),
	{stop, unexpected_response, State};

handle_info({timeout, Timer, req_timeout}, 
	State=#state{ep_id={PeerIP, PeerPortNo}, socket=Socket, req=Request, nextmid=MsgId, sent=Sent, timeout=TimeOut, timer=Timer}) ->
	NextMsgId = next_mid(MsgId),
	ok = inet_udp:send(Socket, PeerIP, PeerPortNo, coap_message:encode(Request#coap_message{id=NextMsgId})),
	NewTimer = erlang:start_timer(?TIMEOUT, self(), req_timeout),
	{noreply, State#state{timeout=TimeOut+1, sent=Sent+1, nextmid=NextMsgId, timer=NewTimer, timestamp=erlang:monotonic_time()}};

handle_info(_Info, State=#state{id=_ID}) ->
	% io:format("unexpected info in bench_worker ~p: ~p~n", [ID, _Info]),
	{noreply, State}.

terminate(_Reason, _State=#state{socket=Socket}) ->
	ok = gen_udp:close(Socket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal

first_mid() ->
    _ = rand:seed(exs1024),
    rand:uniform(?MAX_MESSAGE_ID).

next_mid(MsgId) ->
    if
        MsgId < ?MAX_MESSAGE_ID -> MsgId + 1;
        true -> 1 % or 0?
    end.

resolve_uri(Uri) ->
    {ok, {_Scheme, _UserInfo, Host, PortNo, Path, Query}} =
        http_uri:parse(Uri, [{scheme_defaults, [{coap, 5683}]}]),
    {ok, PeerIP} = inet:getaddr(Host, inet),
    {{PeerIP, PortNo}, split_path(Path), split_query(Query)}.

split_path([]) -> [];
split_path([$/]) -> [];
split_path([$/ | Path]) -> split_segments(Path, $/, []).

split_query([]) -> [];
split_query([$? | Path]) -> split_segments(Path, $&, []).

split_segments(Path, Char, Acc) ->
    case string:rchr(Path, Char) of
        0 ->
            [make_segment(Path) | Acc];
        N when N > 0 ->
            split_segments(string:substr(Path, 1, N-1), Char,
                [make_segment(string:substr(Path, N+1)) | Acc])
    end.

make_segment(Seg) ->
    list_to_binary(http_uri:decode(Seg)).

