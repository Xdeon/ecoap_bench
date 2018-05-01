-module(ecoap_bench_test).

-include_lib("eunit/include/eunit.hrl").

througput_test_() ->
	{setup,
		fun() ->
            {ok, _} = benchmark:start(),
            {ok, _} = application:ensure_all_started(ecoap_bench)
        end,
        fun(_State) ->
            benchmark:stop(),
            application:stop(ecoap_bench)
        end,
        fun run_throughput/1
	}.

run_throughput(_State) ->
	[
	?_assertMatch(#{time:=Time, sent:=Sent, rec:=Rec, timeout:=Timeout, throughput:=Throughput, 
					min:=Min, max:=Max, mean:=Mean, median:=Median, stddev:=Stddev, ptile95:=P95} 
					when is_float(Time) and is_integer(Sent) and is_integer(Rec) and is_integer(Timeout) 
						and is_float(Throughput) and is_float(Min) and is_float(Max) and is_float(Mean) 
						and is_float(Median) and is_float(Stddev) and is_float(P95),
					ecoap_bench_server:start_test(20, 3, "coap://127.0.0.1/benchmark"))
	].