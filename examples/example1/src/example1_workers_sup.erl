-module(example1_workers_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
	ChildSpecs = [
		#{id => example1_worker,
			start => {example1_worker, start_link, []},
			restart => transient,
			shutdown => brutal_kill,
			type => worker,
			modules => [example1_worker]}
	],

	{ok, {{simple_one_for_one, 1, 5}, ChildSpecs}}.


