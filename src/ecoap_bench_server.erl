-module(ecoap_bench_server).
-behaviour(gen_server).

%% API.
-export([start_link/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	worker_sup = undefined :: pid()
}).

%% API.

-spec start_link(pid()) -> {ok, pid()}.
start_link(SupPid) ->
	proc_lib:start_link(?MODULE, init, [SupPid]).

%% gen_server.

init(SupPid) ->
	register(?MODULE, self()),
	ok = proc_lib:init_ack({ok, self()}),
	{ok, Pid} = supervisor:start_child(SupPid, 
		#{id => bench_worker_sup, 
		start => {bench_worker_sup, start_link, []}, 
		restart => temporary, 
		shutdown => infinity, 
		type => supervisor,
		modules => [bench_worker_sup]}),
    link(Pid),
    gen_server:enter_loop(?MODULE, [], #state{worker_sup=Pid}, {local, ?MODULE}).

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
