-module(bench).
-export([start_test/1]).
% -export([generate_test_sequence/0]).

-define(INTERVAL, 30000).
% -define(TEST_CYCLE, 10).
-define(TEST_PERIOD, 60).

start_test(Uri) ->
	% _ = os:cmd("ulimit -n 10240"),
	{ok, _} = application:ensure_all_started(ecoap_bench),
	% TestSequence = generate_test_sequence(),
	% warmup_test(1000, ?TEST_PERIOD, Uri),
	% ok = run_test(Uri, TestSequence),
	% [test_func(N, ?TEST_PERIOD, Uri) || N <- lists:seq(1, 9)],
	[test_func(N, ?TEST_PERIOD, Uri) || N <- lists:seq(10, 90, 10)],
	[test_func(N, ?TEST_PERIOD, Uri) || N <- lists:seq(100, 200, 25)],
	[test_func(N, ?TEST_PERIOD, Uri) || N <- lists:seq(300, 900, 100)],
	[test_func(N, ?TEST_PERIOD, Uri) || N <- lists:seq(1000, 5000, 500)],
	[test_func(N, ?TEST_PERIOD, Uri) || N <- lists:seq(6000, 10000, 1000)],
	ok = application:stop(ecoap_bench).

% generate test sequence, a.k.a. number of concurrent clients
% in the following fashion:
% 10..90, 100..900, 1000..10000 with exponential step

% UPDATE: revise the sequence according to Californium offical test setting
% one run takes (60+15) + (9+5+7+9+5)*(60+15) = 2700sec = 45min
% generate_test_sequence() ->
	% lists:seq(10, 90, 10) ++ lists:seq(100, 900, 100) ++ lists:seq(1000, 10000, 1000).
	% lists:seq(10, 90, 10) ++ lists:seq(100, 200, 25) ++ lists:seq(300, 900, 100) ++ lists:seq(1000, 5000, 500) ++ lists:seq(6000, 10000, 1000).

% warm the server up and abandon the test result
% warmup_test(N, Time, Uri) ->
% 	_ = ecoap_bench_server:start_test(N, Time, Uri),
% 	timer:sleep(?INTERVAL),
% 	ok.

% used when repeating of the same test (test with same setup) is needed
% repeated_test_func(N, Time, Uri, Cycle) ->
% 	[test_func(N, Time, Uri) || _ <- lists:seq(1, Cycle)],
% 	ok.

% run_test(_Uri, []) -> ok;
% run_test(Uri, [H|T]) ->
% 	test_func(H, ?TEST_PERIOD, Uri),
% 	run_test(Uri, T).

% execute the test, format the result and print
test_func(N, Time, Uri) ->
	case ecoap_bench_server:start_test(N, Time, Uri) of
		{error, Reason} ->
			exit(Reason);
		{ok, Result} ->
			#{time:=TestTime, rec:=Recv, timeout:=Timeout, 
			throughput:=Throughput, min:=Min, max:=Max, 
			mean:=Mean, median:=Median, stddev:=Stddev, 
			ptile95:=Ptile95} = Result,
			io:fwrite("c=~p, t=~.3f, received=~p, timeouts=~p, throughput=~.2f, 
				uri=~s, min=~pms, max=~pms, mean=~.3fms, median=~.3fms, stddev=~.3fms, ptile95=~.3fms~n",
				[N, TestTime, Recv, Timeout, Throughput, Uri, Min, Max, Mean, Median, Stddev, Ptile95]),
			sleep(?INTERVAL)
	end.

sleep(Time) ->
	timer:sleep(Time).