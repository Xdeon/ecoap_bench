-module(ecoap_bench_server).
-behaviour(gen_server).

%% API.
-export([start_link/1, start_workers/1, start_test/3, start_test/2]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	worker_sup = undefined :: pid(),
	start_worker_id = 1 :: non_neg_integer(),
	worker_pids = [] :: [pid()],
	worker_refs = [] :: [reference()],
	worker_counter = 0 :: non_neg_integer(),
	start_time = undefined :: undefined | integer(),
	test_time = undefined :: undefined | non_neg_integer(),
	result = #{sent=>0, rec=>0, timeout=>0, throughput=>0}
}).

%% API.

-spec start_link(pid()) -> {ok, pid()}.
start_link(SupPid) ->
	proc_lib:start_link(?MODULE, init, [SupPid]).

start_workers(N) ->
	gen_server:call(?MODULE, {start_workers, N}, 10000).

start_test(N, Time, Uri) ->
	start_workers(N),
	start_test(Time, Uri).

start_test(Time, Uri) ->
	gen_server:cast(?MODULE, {start_test, Time, Uri}).

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

handle_call({start_workers, N}, _From, 
	State=#state{start_worker_id=StartID, worker_sup=WorkerSup, worker_pids=WorkerPids, worker_refs=Refs, worker_counter=Cnt}) ->
	Pids = [begin {ok, Pid} = bench_worker_sup:start_worker(WorkerSup, [self(), ID]), Pid end || ID <- lists:seq(StartID, StartID+N-1)],
	Refs2 = lists:foldl(fun(Pid, Acc) -> Ref = erlang:monitor(process, Pid), [Ref|Acc] end, Refs, Pids),
	{reply, ok, State#state{start_worker_id=StartID+N, worker_pids=lists:append(WorkerPids, Pids), worker_refs=Refs2, worker_counter=Cnt+N}};

% handle_call(shutdown_workers, _From, State=#state{worker_pids=WorkerPids}) ->
% 	[bench_worker:close(Pid) || Pid <- WorkerPids],
% 	{reply, ok, State#state{worker_pids=[]}};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast({start_test, Time, Uri}, State=#state{worker_pids=WorkerPids}) ->
	io:format("start ~p clients for ~pms~n", [length(WorkerPids), Time*1000]),
	[bench_worker:start_test(Pid, Uri) || Pid <- WorkerPids],
	StartTime = erlang:monotonic_time(),
	{noreply, State#state{start_time=StartTime}, Time*1000};

handle_cast({result, _Pid, _R=#{sent:=WSent, rec:=WRec, timeout:=WTimeOut}}, 
	State=#state{result=Result=#{sent:=ASent, rec:=ARec, timeout:=ATimeOut}, worker_counter=Cnt}) ->
	% io:format("worker result: ~p~n", [_R]),
	NewResult = Result#{sent:=ASent+WSent, rec:=ARec+WRec, timeout:=ATimeOut+WTimeOut},
	case Cnt - 1 of
		0 -> 
			gen_server:cast(self(), test_complete);
		_Else ->
			ok
	end,
	{noreply, State#state{result=NewResult, worker_counter=Cnt-1}};

handle_cast(test_complete, State=#state{result=Result, test_time=TestTime, worker_pids=_WorkerPids}) ->
	#{rec:=Rec} = Result,
	Result2 = Result#{throughput:=Rec/TestTime*1000},
	io:format("~p~n", [Result2]),
	% shutdown_workers(WorkerPids),
	{noreply, State#state{start_worker_id=1, result=Result#{sent:=0, rec:=0, timeout:=0, throughput:=0}, worker_counter=0}};

handle_cast(_Msg, State) ->
	io:format("unexpected cast in ecoap_bench_server: ~p~n", [_Msg]),
	{noreply, State}.

handle_info(timeout, State=#state{worker_pids=WorkerPids, start_time=StartTime}) ->
	[bench_worker:stop_test(Pid) || Pid <- WorkerPids],
	TestTime = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, milli_seconds),
	io:format("TestTime: ~ps~n", [TestTime/1000]),
	{noreply, State#state{test_time=TestTime}};

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
	handle_down_worker(Ref, Pid, Reason, State);

handle_info(_Info, State) ->
	io:format("unexpected info in ecoap_bench_server: ~p~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State=#state{worker_pids=WorkerPids}) ->
	_ = shutdown_workers(WorkerPids),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal
handle_down_worker(Ref, Pid, Reason, State=#state{worker_pids=WorkerPids, worker_refs=Refs}) ->
	case lists:member(Ref, Refs) of
		true -> 
			case Reason of
				normal -> 
					NewWorkerPids = lists:delete(Pid, WorkerPids),
					NewWorkerRefs = lists:delete(Ref, Refs),
					{noreply, State#state{worker_pids=NewWorkerPids, worker_refs=NewWorkerRefs}};
				Else ->
					error_logger:error_msg("Worker ~p carshed with reason ~p~n", [Pid, Else]),
					{stop, normal, State}
			end;
		false ->
			{noreply, State}
	end.

shutdown_workers(WorkerPids) ->
	[bench_worker:close(Pid) || Pid <- WorkerPids].
