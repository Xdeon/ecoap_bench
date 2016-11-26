-module(bench_worker).
-behaviour(gen_server).

%% API.
-export([start_link/2, close/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	server = undefined :: pid(),
	id = undefined :: non_neg_integer(),
	socket = undefined :: inet:socket()
}).

%% API.

-spec start_link(inet:socket(), non_neg_integer()) -> {ok, pid()}.
start_link(Server, ID) ->
	gen_server:start_link(?MODULE, [Server, ID], []).

-spec close(pid()) -> ok.
close(Pid) ->
	gen_server:cast(Pid, shutdown).

%% gen_server.

init([Server, ID]) ->
	{ok, Socket} = gen_udp:open(0, [binary, {active, true}]),
	{ok, #state{server=Server, socket=Socket, id=ID}}.

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(shutdown, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State=#state{socket=Socket}) ->
	ok = gen_udp:close(Socket),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
