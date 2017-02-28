-module(bench).
-export([start_test/1]).

-define(INTERVAL, 15000).

start_test(Uri) ->
	{ok, _} = application:ensure_all_started(ecoap_bench),
	_ = ecoap_bench_server:start_test(1000, 60, Uri),
	timer:sleep(?INTERVAL),
	[test_func(N, 60, Uri) || N <- lists:seq(10, 90, 10)],
	[test_func(N, 60, Uri) || N <- lists:seq(100, 900, 100)],
	[test_func(N, 60, Uri) || N <- lists:seq(1000, 10000, 1000)].

test_func(N, Time, Uri) ->
	#{rec:=Recv, timeout:=Timeout, throughput:=Throughput, min:=Min, max:=Max, mean:=Mean, median:=Median, stddev:=Stddev, ptile95:=Ptile95} = ecoap_bench_server:start_test(N, 60, Uri), 
	io:fwrite("c=~p, t=~.3f, received=~p, timeouts=~p, throughput=~p, uri=~p, min=~pms, max=~pms, mean=~.3fms, median=~.3fms, stddev=~.3fms, ptile95=~.3fms~n",
		[N, Time, Recv, Timeout, Throughput, Uri, Min, Max, Mean, Median, Stddev, Ptile95]),
	timer:sleep(?INTERVAL).