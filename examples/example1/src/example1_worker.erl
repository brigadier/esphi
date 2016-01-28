-module(example1_worker).
-compile(nowarn_unused_function).
-compile(nowarn_unused_vars).
-include_lib("esphi/include/esphi.hrl").
-behaviour(gen_server).

%% API
-export([start_link/5]).

%% gen_server callbacks
-export([init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {disp_pid, db = undefined, t1 = undefined, t2 = undefined, t3 = undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Disp, Db, T1, T2, T3) ->
	gen_server:start_link(?MODULE, [Disp, Db, T1, T2, T3], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


init([Disp, Db, T1, T2, T3]) ->
	random:seed(erlang:unique_integer()),
	gen_server:cast(self(), go),
	{ok, #state{disp_pid = Disp, db = Db, t1 = T1, t2 = T2, t3 = T3}}.




handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(go, #state{db = Db, t1 = T1, t2 = T2} = State) ->
%% 	donothing(),
	test_crud(Db, T1, T2),
	test_tran(Db, T1, T2),
	test_cursor(Db, T1, T2),
%% 	test_rollbacks(Db, T1, T2),
%% 	test_locks(Db, T1, T2),


	{stop, normal, State};
handle_cast(_Request, State) ->
	{noreply, State}.



handle_info(_Info, State) ->
	{noreply, State}.



terminate(_Reason, _State) ->
	example1_dbowner:pong(),
	ok.



code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
randbin() ->
	float_to_binary(random:uniform()).
randint() ->
	random:uniform(100000000).
randord() ->
	element(random:uniform(4), {'>', '>=', '<', '<='}).
randbin(N) ->
	<<R:N/binary, _/binary>> = randbin(),
	R.

donothing() ->
	timer:sleep(500).

test_cursor(_Db, T1, _T2) ->
	case esphi:esphi_cursor_open(T1, randbin(3), randord(), randbin(4)) of
		{ok, C} ->
			_R1 = esphi:esphi_cursor_next(C, ?SOPHIA_FULL_OBJECT, ?SOPHIA_BINARY, ?SOPHIA_BINARY),
%% 			io:format("~p~n", [R1]),
			_R2 = esphi:esphi_cursor_next(C, ?SOPHIA_FULL_OBJECT, ?SOPHIA_BINARY, ?SOPHIA_BINARY),
%% 			io:format("~p~n", [R2]),
			esphi:esphi_cursor_close(C);
		_E -> ok
%% 		io:format("~p~n", [E])

	end.

test_crud(_Db, T1, T2) ->
	Randi = randint(),
	Randb2 = randbin(),
	Randb = randbin(),

	ok = esphi:esphi_put(T2, randint(), randbin()),
	ok = esphi:esphi_put(T1, randbin(), randbin()),

	ok = esphi:esphi_put(T1, Randb, randbin()),
	ok = esphi:esphi_put(T2, Randi, randbin()),
	ok = esphi:esphi_put(T1, Randb2, randint()),


	_R1 = esphi:esphi_get(T2, randint(), ?SOPHIA_BINARY),
	_R2 = esphi:esphi_get(T1, randbin(), ?SOPHIA_BINARY),

	%%can be not_found, as the data can be deleted from another process
	%%remove esphi_delete call to prevent not_found
	V1 = esphi:esphi_get(T1, Randb, ?SOPHIA_BINARY),
	true = is_binary(V1) orelse is_tuple(V1) orelse V1 == not_found,
	V2 = esphi:esphi_get(T2, Randi, ?SOPHIA_BINARY),
	true = is_binary(V2) orelse is_tuple(V2) orelse V2 == not_found,
	V3 = esphi:esphi_get(T1, Randb2, ?SOPHIA_INT),
	true = is_integer(V3) orelse is_tuple(V3) orelse V3 == not_found,
	esphi:esphi_delete(T1, Randb2).
test_tran(Db, T1, T2) ->
	case esphi:esphi_transaction_begin(Db) of
		{ok, Tr} ->
			esphi:esphi_put(T2, Tr, randint(), randbin()),
			esphi:esphi_put(T1, Tr, randbin(), randbin()),
			case esphi:esphi_transaction_commit(Tr) of
				ok -> ok;
				{error, lock} -> esphi:esphi_transaction_rollback(Tr);
				{error, rolled_back} -> ok;
				{error, disposed} -> ok
			end;
		{error, _} -> ok

	end,
	case esphi:esphi_transaction_begin(Db) of
		{ok, Tr2} ->
			esphi:esphi_put(T2, Tr2, randint(), randbin()),
			esphi:esphi_put(T1, Tr2, randbin(), randbin()),
			esphi:esphi_transaction_rollback(Tr2);
		{error, _} -> ok

	end.



test_rollbacks(Db, _T1, T2) ->
	case esphi:esphi_transaction_begin(Db) of
		{ok, Tr3} ->
			esphi:esphi_put(T2, Tr3, 100, randbin()),
			timer:sleep(500),
			case esphi:esphi_transaction_commit(Tr3) of
				ok -> ok;
				{error, lock} -> esphi:esphi_transaction_rollback(Tr3);
				{error, rolled_back} -> ok;
				{error, disposed} -> ok;
				{error, busy} -> ok
			end;
		{error, _} -> ok

	end.

test_locks(Db, _T1, T2) ->
	case esphi:esphi_transaction_begin(Db) of
		{ok, Tr4} ->
			Rnd = rand:uniform(),
			if
				Rnd >= 0.5 ->
					esphi:esphi_put(T2, Tr4, 10, randbin()),
					esphi:esphi_put(T2, Tr4, 100, randbin());
				true ->
					esphi:esphi_put(T2, Tr4, 100, randbin()),
					esphi:esphi_put(T2, Tr4, 10, randbin())
			end,

			case esphi:esphi_transaction_commit(Tr4) of
				ok -> ok;
				{error, lock} ->
					io:format("LOCK~n"),
					esphi:esphi_transaction_rollback(Tr4);
				{error, rolled_back} -> ok;
				{error, disposed} -> ok;
				{error, busy} -> ok
			end;
		{error, _} -> ok

	end.

%% lists:foreach(fun erlang:garbage_collect/1, processes()).