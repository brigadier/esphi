-module(example1_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Procs = [
		#{id => example1_workers_sup,
			start => {example1_workers_sup, start_link, []},
			restart => transient,
			shutdown => brutal_kill,
			type => supervisor,
			modules => [example1_workers_sup]},
		#{id => example1_dbowner,
			start => {example1_dbowner, start_link, []},
			restart => transient,
			shutdown => brutal_kill,
			type => worker,
			modules => [example1_dbowner]}
	],
	{ok, {{one_for_one, 1, 5}, Procs}}.
