-module(ecoap_bench_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [#{id => ecoap_bench_server,
				start => {ecoap_bench_server, start_link, [self()]},
				restart => permanent,
				shutdown => 10000,
				type => worker,
				modules => [ecoap_bench_server]}],
	{ok, {#{strategy => one_for_all, intensity => 0, period => 1}, Procs}}.
