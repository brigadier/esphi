-module(example1).

%% API
-export([start/1]).

start(N) ->
	application:start(esphi),
	application:load(example1),
	application:set_env(example1, time, N * 60 * 1000),
	application:start(example1).
