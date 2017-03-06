-module(ecoap_bench_server).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([start_workers/1, go_test/2]).
-export([start_test/3, start_test/4, start_test/5]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	worker_sup = undefined :: pid(),
	% start_worker_id = undefined :: non_neg_integer(),
	worker_pids = undefined :: [pid()],
	worker_counter = undefined :: non_neg_integer(),
	worker_refs = undefined :: gb_sets:set(reference()),
	start_time = undefined :: undefined | integer(),
	test_time = undefined :: undefined | non_neg_integer(),
	result = #{sent=>0, rec=>0, timeout=>0, throughput=>0, min=>0, max=>0, mean=>0, median=>0, stddev=> 0, ptile95=>0} :: map(),
	hdr_ref = undefined :: undefined | binary(),
	client = undefined :: undefined | pid()
}).

-include_lib("ecoap_common/include/coap_def.hrl").

%% API.

-spec start_link(pid()) -> {ok, pid()}.
start_link(SupPid) ->
	proc_lib:start_link(?MODULE, init, [SupPid]).

-spec start_test(non_neg_integer(), non_neg_integer(), list()) -> ok | {error, any()}.
start_test(N, Time, Uri) ->
	start_test(N, Time, Uri, 'GET').

-spec start_test(non_neg_integer(), non_neg_integer(), list(), coap_method()) -> ok | {error, any()}.
start_test(N, Time, Uri, Method) ->
	start_test(N, Time, Uri, Method, <<>>).

-spec start_test(non_neg_integer(), non_neg_integer(), list(), coap_method(), binary()) -> map() | {error, any()}.
start_test(N, Time, Uri, Method, Content) ->
	start_workers(N),
	go_test(Time, {Method, Uri, Content}),
	Ref = erlang:monitor(process, whereis(?MODULE)),
	receive
		{test_result, _TestTime, Result2} ->
			erlang:demonitor(Ref, [flush]),
			% io:fwrite("Test complete~nTest Time: ~p~n", [TestTime/1000]),
			Result2;
		{'DOWN', Ref, process, _Pid, Reason} ->
			{error, Reason}
	end.

-spec start_workers(non_neg_integer()) -> ok.
start_workers(N) ->
	gen_server:cast(?MODULE, {start_workers, N}).

-spec go_test(non_neg_integer(), {coap_method(), list(), coap_content() | binary() | list()}) -> ok.
go_test(Time, {Method, Uri, Content}) ->
	gen_server:cast(?MODULE, {start_test, Time, {Method, Uri, Content}, self()}).

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
    gen_server:enter_loop(?MODULE, [], 
    	#state{worker_sup=Pid, worker_pids=[], worker_counter=0, worker_refs=gb_sets:new(), result=new_result()}, {local, ?MODULE}).

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({start_workers, N}, State=#state{worker_sup=WorkerSup}) -> 
	Pids = [begin {ok, Pid} = bench_worker_sup:start_worker(WorkerSup, [self(), ID]), link(Pid), Pid end || ID <- lists:seq(1, N)],
	{noreply, State#state{worker_pids=Pids, worker_counter=N}};

handle_cast({start_test, Time, {Method, Uri, Content}, Client}, State=#state{worker_pids=WorkerPids}) ->
	% io:fwrite("start ~p clients for ~pms~n", [length(WorkerPids), Time*1000]),
	{ok, Main_HDR_Ref} = hdr_histogram:open(3600000000, 3),
	WorkerRefs = lists:foldl(fun(Pid, Acc) -> 
									Ref = make_ref(), 
									bench_worker:start_test(Pid, Ref, {Method, Uri, Content}), 
									gb_sets:add(Ref, Acc) end, gb_sets:new(), WorkerPids),
	StartTime = erlang:monotonic_time(),
	{noreply, State#state{start_time=StartTime, client=Client, hdr_ref=Main_HDR_Ref, worker_refs=WorkerRefs}, Time*1000};

handle_cast({result, _Pid, Ref, _R=#{sent:=WSent, rec:=WRec, timeout:=WTimeOut}, HDR_Ref}, 
	State=#state{result=Result, worker_counter=Cnt, worker_refs=WorkerRefs, hdr_ref=Main_HDR_Ref}) ->
	case gb_sets:is_member(Ref, WorkerRefs) of
		true ->
			#{sent:=ASent, rec:=ARec, timeout:=ATimeOut} = Result,
			NewResult = Result#{sent:=ASent+WSent, rec:=ARec+WRec, timeout:=ATimeOut+WTimeOut},
			_ = hdr_histogram:add(Main_HDR_Ref, HDR_Ref),
			ok = hdr_histogram:close(HDR_Ref),
			case Cnt - 1 of
				0 -> 
					gen_server:cast(self(), test_complete);
				_Else when _Else > 0 ->
					ok
			end,
			{noreply, State#state{result=NewResult, worker_counter=Cnt-1, worker_refs=gb_sets:delete(Ref, WorkerRefs)}};
		false ->
			{noreply, State}
	end;

handle_cast(test_complete, State=#state{result=Result, test_time=TestTime, client=Client, hdr_ref=Main_HDR_Ref}) ->
	#{rec:=Rec} = Result,
 	Min = hdr_histogram:min(Main_HDR_Ref)/1000, 
 	Max = hdr_histogram:max(Main_HDR_Ref)/1000,
 	Mean = hdr_histogram:mean(Main_HDR_Ref)/1000,
 	Median = hdr_histogram:median(Main_HDR_Ref)/1000,
 	Stddev = hdr_histogram:stddev(Main_HDR_Ref)/1000,
 	Ptile95 = hdr_histogram:percentile(Main_HDR_Ref, 95.0)/1000,
	% io:fwrite("Min ~pms~n", [hdr_histogram:min(Main_HDR_Ref)/1000]),
 %    io:fwrite("Max ~pms~n", [hdr_histogram:max(Main_HDR_Ref)/1000]),
 %    io:fwrite("Mean ~.3fms~n", [hdr_histogram:mean(Main_HDR_Ref)/1000]),
 %    io:fwrite("Median ~.3fms~n", [hdr_histogram:median(Main_HDR_Ref)/1000]),
 %    io:fwrite("Stddev ~.3fms~n", [hdr_histogram:stddev(Main_HDR_Ref)/1000]),
 %    io:fwrite("95ile ~.3fms~n", [hdr_histogram:percentile(Main_HDR_Ref,95.0)/1000]),
 %    io:fwrite("Memory Size ~p~n", [hdr_histogram:get_memory_size(Main_HDR_Ref)]),
 %    io:fwrite("Total Count ~p~n", [hdr_histogram:get_total_count(Main_HDR_Ref)]),
	ok = hdr_histogram:close(Main_HDR_Ref),
	Client ! {test_result, TestTime, 
		Result#{throughput:=Rec/TestTime*1000, min:=Min, max:=Max, mean:=Mean, median:=Median, stddev:=Stddev, ptile95:=Ptile95}},
	{noreply, State#state{result=new_result(), worker_pids=[], start_time=undefined, test_time=undefined}};

handle_cast(_Msg, State) ->
	io:fwrite("unexpected cast in ecoap_bench_server: ~p~n", [_Msg]),
	{noreply, State}.

handle_info(timeout, State=#state{worker_pids=WorkerPids, start_time=StartTime}) ->
	[bench_worker:stop_test(Pid) || Pid <- WorkerPids],
	TestTime = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, milli_seconds),
	{noreply, State#state{test_time=TestTime}};

% handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
% 	handle_down_worker(Ref, Pid, Reason, State);

handle_info(_Info, State) ->
	io:fwrite("unexpected info in ecoap_bench_server: ~p~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State=#state{worker_pids=WorkerPids}) ->
	_ = shutdown_workers(WorkerPids),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal

new_result() -> #{sent=>0, rec=>0, timeout=>0, throughput=>0.0, min=>0.0, max=>0.0, mean=>0.0, median=>0.0, stddev=> 0.0, ptile95=>0.0}.

shutdown_workers(WorkerPids) ->
	[bench_worker:close(Pid) || Pid <- WorkerPids].
	
