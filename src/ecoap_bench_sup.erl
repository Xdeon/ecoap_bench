-module(ecoap_bench_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([find_child/2]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
			#{id => bench_worker_sup, 
			start => {bench_worker_sup, start_link, []}, 
			restart => permanent, 
			shutdown => infinity, 
			type => supervisor,
			modules => [bench_worker_sup]},
			#{id => ecoap_bench_server,
			start => {ecoap_bench_server, start_link, [self()]},
			restart => permanent,
			shutdown => 10000,
			type => worker,
			modules => [ecoap_bench_server]}],
	{ok, {#{strategy => one_for_all, intensity => 5, period => 10}, Procs}}.

find_child(SupPid, Id) ->
	{_, Pid, _, _} = lists:keyfind(Id, 1, supervisor:which_children(SupPid)),
	Pid.