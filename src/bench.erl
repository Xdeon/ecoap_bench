-module(bench).
-export([start_test/1]).

-define(INTERVAL, 15000).
-define(TEST_CYCLE, 3).
-define(TEST_PERIOD, 60).

start_test(Uri) ->
	_ = os:cmd("ulimit -n 65535"),
	_ = application:stop(ecoap_bench),
	{ok, _} = application:ensure_all_started(ecoap_bench),
	TestSequence = generate_test_sequence(),
	warmup_test(1000, ?TEST_PERIOD, Uri),
	[repeated_test_func(N, ?TEST_PERIOD, Uri, ?TEST_CYCLE) || N <- TestSequence],
	ok = application:stop(ecoap_bench).

% generate test sequence, a.k.a. number of concurrent clients
% in the following fashion:
% 10..90, 100..900, 1000..10000 with exponential step
generate_test_sequence() ->
	lists:seq(10, 90, 10) ++ lists:seq(100, 900, 100) ++ lists:seq(1000, 10000, 1000).

% warm the server up and abandon the test result
warmup_test(N, Time, Uri) ->
	_ = ecoap_bench_server:start_test(N, Time, Uri),
	timer:sleep(?INTERVAL),
	ok.

% used when repeating of the same test (test with same setup) is needed
repeated_test_func(N, Time, Uri, Cycle) ->
	[test_func(N, Time, Uri) || _ <- lists:seq(1, Cycle)],
	ok.

% execute the test, format the result and print
test_func(N, Time, Uri) ->
	#{rec:=Recv, timeout:=Timeout, throughput:=Throughput, min:=Min, max:=Max, mean:=Mean, median:=Median, stddev:=Stddev, ptile95:=Ptile95} = ecoap_bench_server:start_test(N, Time, Uri), 
	io:fwrite("c=~p, t=~p, received=~p, timeouts=~p, throughput=~.2f, uri=~s, min=~pms, max=~pms, mean=~.3fms, median=~.3fms, stddev=~.3fms, ptile95=~.3fms~n",
		[N, Time, Recv, Timeout, Throughput, Uri, Min, Max, Mean, Median, Stddev, Ptile95]),
	timer:sleep(?INTERVAL),
	ok.