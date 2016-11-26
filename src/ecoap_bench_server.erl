-module(ecoap_bench_server).
-behaviour(gen_server).

%% API.
-export([start_link/1, start_workers/1, shutdown_workers/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	worker_sup = undefined :: pid(),
	worker_pids = [] :: [pid()],
	worker_refs = [] :: [reference()]
}).

%% API.

-spec start_link(pid()) -> {ok, pid()}.
start_link(SupPid) ->
	proc_lib:start_link(?MODULE, init, [SupPid]).

start_workers(N) ->
	gen_server:call(?MODULE, {start_workers, N}).

shutdown_workers() ->
	gen_server:call(?MODULE, shutdown_workers).

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

handle_call({start_workers, N}, _From, State=#state{worker_sup=WorkerSup, worker_pids=WorkerPids, worker_refs=Refs}) ->
	Pids = [begin {ok, Pid} = bench_worker_sup:start_worker(WorkerSup, [self(), ID]), Pid end || ID <- lists:seq(1, N)],
	Refs2 = lists:foldl(fun(Pid, Acc) -> Ref = erlang:monitor(process, Pid), [Ref|Acc] end, Refs, Pids),
	{reply, ok, State#state{worker_pids=lists:append(WorkerPids, Pids), worker_refs=Refs2}};

handle_call(shutdown_workers, _From, State=#state{worker_pids=WorkerPids}) ->
	[bench_worker:close(Pid) || Pid <- WorkerPids],
	{reply, ok, State};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State=#state{worker_pids=WorkerPids, worker_refs=Refs}) ->
	case lists:member(Ref, Refs) of
		true -> 
			_ = report_worker_error(Pid, Reason),
			{noreply, State#state{worker_pids=lists:delete(Pid, WorkerPids), worker_refs=lists:delete(Ref, Refs)}};
		false -> 
			{noreply, State}
	end;

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal
report_worker_error(Pid, Reason) ->
	case Reason of
		normal -> ok;
		Else ->
			error_logger:error_msg("Worker ~p carshed with reason ~p~n", [Pid, Else])
	end.
