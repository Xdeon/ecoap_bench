-module(ecoap_bench_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).
-export([start_workers/1, shutdown_workers/0]).

start(_Type, _Args) ->
	ecoap_bench_sup:start_link().

start_workers(N) ->
	ecoap_bench_server:start_workers(N).

shutdown_workers() ->
	ecoap_bench_server:shutdown_workers().

stop(_State) ->
	ok.
