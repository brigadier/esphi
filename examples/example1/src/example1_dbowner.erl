-module(example1_dbowner).

-behaviour(gen_server).

%% API
-export([start_link/0, pong/0]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-define(REOPEN, 30000).
-define(NWORKERS, 1000).

-record(state, {sec = 0, db = undefined, t1 = undefined, t2 = undefined, t3 = undefined}).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

pong() ->
	gen_server:cast(?SERVER, pong).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([]) ->
	erlang:display(init),
	random:seed(erlang:unique_integer()),
	{ok, Time} = application:get_env(example1, time),
	erlang:send_after(Time, self(), quit),
	erlang:send_after(0, self(), dbopen),
	erlang:send_after(1, self(), start_workers),
	erlang:send_after(0, self(), tick),

	{ok, #state{}}.



handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(pong, #state{db = Db, t1 = T1, t2 = T2, t3 = T3} = State) ->
	{ok, _} = supervisor:start_child(example1_workers_sup, [self(), Db, T1, T2, T3]),
	{noreply, State};
handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(start_workers, #state{db = Db, t1 = T1, t2 = T2, t3 = T3} = State) ->
	erlang:display(start_workers),
	lists:foreach(
		fun(_) ->
			{ok, _} = supervisor:start_child(example1_workers_sup, [self(), Db, T1, T2, T3])
		end,

		lists:seq(1, ?NWORKERS)
	),
	{noreply, State};

handle_info(dbopen, #state{db = Db, t1 = T1, t2 = T2, t3 = T3} = State) ->
	erlang:display(dbopen),
	if
		T1 =/= undefined -> esphi:esphi_table_close(T1);
		true -> ok
	end,
	if
		T2 =/= undefined -> esphi:esphi_table_close(T2);
		true -> ok
	end,
	if
		T3 =/= undefined -> esphi:esphi_table_close(T3);
		true -> ok
	end,
	if
		Db =/= undefined -> esphi:esphi_db_close(Db);
		true -> ok
	end,

	{ok, DBN} = esphi:esphi_db_open(
		"/tmp/db",
		["t1", "t2", "t3"],
		#{"db.t2.index.key" => "u64",
			"db.t3.index.upsert" => "__COUNTER__",
			"backup.path" => "/tmp/backup",
			"sophia.recover" => 1
		}),
	{ok, T1N} = esphi:esphi_table_open(DBN, "t1"),
	{ok, T2N} = esphi:esphi_table_open(DBN, "t2"),
	{ok, T3N} = esphi:esphi_table_open(DBN, "t3"),
	erlang:display(open),
	%%this would likely damage the database
%% 	erlang:send_after(random:uniform(?REOPEN), self(), dbopen),

	{noreply, State#state{db = DBN, t1 = T1N, t2 = T2N, t3 = T3N}};

handle_info(quit, #state{db = Db} = State) ->
	io:format("Qutting...~n"),
	ok = esphi:esphi_db_close(Db),
	{stop, normal, State};

handle_info(tick, #state{sec = Sec} = State) ->
	io:format("\r~B       \r", [Sec]),
	erlang:send_after(1000, self(), tick),
	{noreply, State#state{sec = Sec + 1}};


handle_info(_Info, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	io:format("Done, bye~n"),
	ok.



code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
