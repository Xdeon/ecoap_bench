-module(ecoap_bench_server).
-behaviour(gen_server).

%% API.
-export([start_link/1]).
-export([start_workers/1, go_test/2]).
-export([start_test/3, start_test/4, start_test/5]).

%% gen_server.
-export([init/1]).
-export([handle_continue/2]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
	worker_sup = undefined :: undefined | pid(),
	% start_worker_id = undefined :: non_neg_integer(),
	worker_pids = undefined :: [pid()],
	worker_refs = undefined :: gb_sets:set(reference()),
	start_time = undefined :: undefined | integer(),
	test_time = undefined :: undefined | non_neg_integer(),
	result = undefined :: test_result(),
	hdr_ref = undefined :: undefined | binary(),
	client = undefined :: undefined | pid()
}).

-type test_result() :: #{
							time := float(),
							sent := non_neg_integer(),
							rec := non_neg_integer(),
							timeout := non_neg_integer(),
							throughput := float(),
							min := float(),
							max := float(),
							mean := float(),
							median := float(),
							stddev := float(),
							ptile95 := float()
						}.

%% API.

-spec start_link(pid()) -> {ok, pid()}.
start_link(SupPid) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [SupPid], []).

-spec start_test(non_neg_integer(), non_neg_integer(), string()) -> {ok, test_result()} | {error, any()}.
start_test(N, Time, Uri) ->
	start_test(N, Time, Uri, 'GET').

-spec start_test(non_neg_integer(), non_neg_integer(), string(), ecoap_message:coap_method()) -> {ok, test_result()} | {error, any()}.
start_test(N, Time, Uri, Method) ->
	start_test(N, Time, Uri, Method, <<>>).

-spec start_test(non_neg_integer(), non_neg_integer(), string(), ecoap_message:coap_method(), binary()) -> {ok, test_result()} | {error, any()}.
start_test(N, Time, Uri, Method, Payload) ->
	start_workers(N),
	go_test(Time, {Uri, Method, Payload}),
	Ref = erlang:monitor(process, whereis(?MODULE)),
	receive
		{test_result, _TestTime, Result} ->
			erlang:demonitor(Ref, [flush]),
			% io:fwrite("Test complete~nTest Time: ~p~n", [TestTime/1000]),
			{ok, Result};
		{'DOWN', Ref, process, _Pid, Reason} ->
			{error, Reason}
	end.

-spec start_workers(non_neg_integer()) -> ok.
start_workers(N) ->
	gen_server:cast(?MODULE, {start_workers, N}).

-spec go_test(non_neg_integer(), {string(), ecoap_message:coap_method(), binary()}) -> ok.
go_test(Time, {Uri, Method, Payload}) ->
	gen_server:cast(?MODULE, {start_test, Time, {Uri, Method, Payload}, self()}).

%% gen_server.

init([SupPid]) ->
	process_flag(trap_exit, true),
    {ok, #state{worker_pids=[], worker_refs=gb_sets:new(), result=new_result()}, {continue, {init, SupPid}}}.

handle_continue({init, SupPid}, State) ->
	Pid = ecoap_bench_sup:find_child(SupPid, bench_worker_sup),
	{noreply, State#state{worker_sup=Pid}}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast({start_workers, N}, State=#state{worker_sup=WorkerSup}) -> 
	Pids = [begin {ok, Pid} = bench_worker_sup:start_worker(WorkerSup, [self(), ID]), link(Pid), Pid end || ID <- lists:seq(1, N)],
	{noreply, State#state{worker_pids=Pids}};

handle_cast({start_test, Time, {Uri, Method, Payload}, Client}, State=#state{worker_pids=WorkerPids}) ->
	% io:fwrite("start ~p clients for ~pms~n", [length(WorkerPids), Time*1000]),
	{ok, Main_HDR_Ref} = hdr_histogram:open(3600000000, 3),
	WorkerRefs0 = [begin Ref = make_ref(), bench_worker:start_test(Pid, Ref, {Uri, Method, Payload}), Ref end || Pid <- WorkerPids],
	StartTime = erlang:monotonic_time(),
	_ = erlang:send_after(Time*1000, self(), timeout),
	WorkerRefs = gb_sets:from_list(WorkerRefs0),
	{noreply, State#state{start_time=StartTime, client=Client, hdr_ref=Main_HDR_Ref, worker_refs=WorkerRefs}};

handle_cast({result, Pid, Ref, _R=#{sent:=WSent, rec:=WRec, timeout:=WTimeOut}, HDR_Ref}, 
	State=#state{result=Result, worker_refs=WorkerRefs, hdr_ref=Main_HDR_Ref}) ->
	case gb_sets:is_member(Ref, WorkerRefs) of
		true ->
			#{sent:=ASent, rec:=ARec, timeout:=ATimeOut} = Result,
			NewResult = Result#{sent:=ASent+WSent, rec:=ARec+WRec, timeout:=ATimeOut+WTimeOut},
			% add response time metrics to the main histogram
			_ = hdr_histogram:add(Main_HDR_Ref, HDR_Ref),
			% let the worker clean up and shutdown
			bench_worker:close(Pid),
			NewWorkerRefs = gb_sets:delete(Ref, WorkerRefs),
			case gb_sets:is_empty(NewWorkerRefs) of
				true -> gen_server:cast(self(), test_complete);
				false -> ok
			end,
			{noreply, State#state{result=NewResult, worker_refs=NewWorkerRefs}};
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
    % io:fwrite("Max ~pms~n", [hdr_histogram:max(Main_HDR_Ref)/1000]),
    % io:fwrite("Mean ~.3fms~n", [hdr_histogram:mean(Main_HDR_Ref)/1000]),
    % io:fwrite("Median ~.3fms~n", [hdr_histogram:median(Main_HDR_Ref)/1000]),
    % io:fwrite("Stddev ~.3fms~n", [hdr_histogram:stddev(Main_HDR_Ref)/1000]),
    % io:fwrite("95ile ~.3fms~n", [hdr_histogram:percentile(Main_HDR_Ref,95.0)/1000]),
    % io:fwrite("Memory Size ~p~n", [hdr_histogram:get_memory_size(Main_HDR_Ref)]),
    % io:fwrite("Total Count ~p~n", [hdr_histogram:get_total_count(Main_HDR_Ref)]),
	ok = hdr_histogram:close(Main_HDR_Ref),
	Client ! {test_result, TestTime, 
		Result#{time:=TestTime/1000, throughput:=Rec/TestTime*1000, min:=Min, max:=Max, mean:=Mean, median:=Median, stddev:=Stddev, ptile95:=Ptile95}},
	{noreply, State#state{result=new_result(), worker_pids=[], worker_refs = gb_sets:new(), start_time=undefined, test_time=undefined, hdr_ref=undefined}};

handle_cast(_Msg, State) ->
	io:fwrite("unexpected cast in ecoap_bench_server: ~p~n", [_Msg]),
	{noreply, State}.

handle_info(timeout, State=#state{worker_pids=WorkerPids, start_time=StartTime}) ->
	TestTime = erlang:convert_time_unit(erlang:monotonic_time() - StartTime, native, milli_seconds),
	[bench_worker:stop_test(Pid) || Pid <- WorkerPids],
	{noreply, State#state{test_time=TestTime}};

% handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
% 	handle_down_worker(Ref, Pid, Reason, State);
handle_info({'EXIT', Pid, normal}, State=#state{worker_pids=WorkerPids}) ->
	{noreply, State#state{worker_pids=lists:delete(Pid, WorkerPids)}};
handle_info({'EXIT', Pid, Reason}, State=#state{worker_pids=WorkerPids}) ->
	case lists:member(Pid, WorkerPids) of
		true -> {stop, {worker_failure, Reason}, State};
		false -> {noreply, State}
	end;
handle_info(_Info, State) ->
	io:fwrite("unexpected info in ecoap_bench_server: ~p~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State=#state{worker_pids=WorkerPids}) ->
	_ = shutdown_workers(WorkerPids),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% Internal

new_result() -> #{time=>0.0, sent=>0, rec=>0, timeout=>0, throughput=>0.0, min=>0.0, max=>0.0, mean=>0.0, median=>0.0, stddev=> 0.0, ptile95=>0.0}.

shutdown_workers(WorkerPids) ->
	[bench_worker:close(Pid) || Pid <- WorkerPids].
	
