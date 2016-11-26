-module(bench_worker_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_worker/2]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [#{id => bench_worker,
				start => {bench_worker, start_link, []},
				restart => temporary,
				shutdown => 5000,
				type => worker,
				modules => [bench_worker]}],
	{ok, {{simple_one_for_one, 0, 1}, Procs}}.

start_worker(SupPid, Args) ->
	supervisor:start_child(SupPid, Args).