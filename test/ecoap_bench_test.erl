-module(ecoap_bench_test).

-include_lib("eunit/include/eunit.hrl").

througput_test_() ->
	{setup,
		fun() ->
            ok = benchmark:start(),
            {ok, _} = application:ensure_all_started(ecoap_bench)
        end,
        fun(_State) ->
            benchmark:stop(),
            application:stop(ecoap_bench)
        end,
        fun run_throughput/1
	}.

run_throughput(_State) ->
	Result = ecoap_bench_server:start_test(10, 3, "coap://127.0.0.1/benchmark"),
	[
	?_assertMatch(#{time:=_, sent:=_, rec:=_, timeout:=_, 
					throughput:=_, min:=_, max:=_, mean:=_, 
					median:=_, stddev:=_, ptile95:=_}, Result)
	].