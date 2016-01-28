%%%-------------------------------------------------------------------
%%% @author evgeny
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. янв 2016 18:45
%%%-------------------------------------------------------------------
-module(esphi).
-on_load(on_load/0).

-type sophia_ref() :: any().

-type sophia_db() :: sophia_ref().
-type sophia_table() :: sophia_ref().
-type sophia_cursor() :: sophia_ref().
-type sophia_transaction() :: sophia_ref().

-type sophia_order() :: '>' | '<' | '>=' | '<='.
-type sophia_return_type() :: 1|0. %%1 - int 0 - binary>

-export_type([sophia_db/0, sophia_table/0, sophia_cursor/0, sophia_transaction/0, sophia_order/0]).

%% API
-export([
	  esphi_db_open/3
	, esphi_db_close/1
	, esphi_print_info/1
	, esphi_param_set/3
	, esphi_param_get/3
	, esphi_last_error/1
	, esphi_table_open/2
	, esphi_table_close/1
	, esphi_table_create/3
	, esphi_table_drop/1
	, esphi_transaction_begin/1
	, esphi_transaction_commit/1
	, esphi_transaction_rollback/1
	, esphi_delete/3
	, esphi_delete/2
	, esphi_put/4
	, esphi_put/3
	, esphi_get/3
	, esphi_get/4
	, esphi_update_counter/3
	, esphi_update_counter/4
	, esphi_backup/1
	, esphi_cursor_open/4
	, esphi_cursor_close/1
	, esphi_cursor_next/4
]).

on_load() ->
	init().

init() ->
	SoName = case code:priv_dir(?MODULE) of
				 {error, bad_name} ->
					 case code:which(?MODULE) of
						 Filename when is_list(Filename) ->
							 filename:join([filename:dirname(Filename), "../priv", "esphi"]);
						 _ ->
							 filename:join("../priv", "esphi")
					 end;
				 Dir ->
					 filename:join(Dir, "esphi")
			 end,
	erlang:load_nif(SoName, application:get_all_env(esphi)).





-spec esphi_db_open(iolist(), [iolist()], map()) -> {ok, sophia_db()} | {error, {integer(), binary()}}.
esphi_db_open(_Path, _Schema, _Params) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_db_close(sophia_db()) -> ok.
esphi_db_close(_DBref) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_param_set(sophia_db(), iolist(), iolist()|integer()) -> ok | {error, disposed} | {error, {integer(), binary()}}.
esphi_param_set(_DBref, _Key, _Val) ->
	erlang:nif_error({error, not_loaded}).


-spec esphi_param_get(sophia_db(), iolist(), sophia_return_type()) -> binary() | {error, disposed|type} | {error, {integer(), binary()}}.
esphi_param_get(_DBref, _Key, _ValueType) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_print_info(sophia_db()) -> ok.
esphi_print_info(_DBref) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_last_error(sophia_db()) -> binary() | {error, {integer(), binary()}}.
esphi_last_error(_DBref) ->
	erlang:nif_error({error, not_loaded}).


-spec esphi_table_open(sophia_db(), iolist()) -> {ok, sophia_table()} | {error, disposed} | {error, {integer(), binary()}}.
esphi_table_open(_DBref, _TName) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_table_close(sophia_table()) -> ok.
esphi_table_close(_TRef) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_table_create(sophia_db(), iolist(), map()) -> ok | {error, disposed} | {error, {integer(), binary()}}.
esphi_table_create(_DBref, _TName, _Params) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_table_drop(sophia_table()) -> ok | {error, disposed} | {error, {integer(), binary()}}.
esphi_table_drop(_Tref) ->
	erlang:nif_error({error, not_loaded}).



-spec esphi_transaction_begin(sophia_db()) -> {ok, sophia_transaction()} | {error, disposed} | {error, {integer(), binary()}}.
esphi_transaction_begin(_DBref) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_transaction_commit(sophia_transaction()) -> ok | {error, disposed|rolled_back|lock|unknown|busy} | {error, {integer(), binary()}}.
esphi_transaction_commit(_TranRef) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_transaction_rollback(sophia_transaction()) -> ok.
esphi_transaction_rollback(_TranRef) ->
	erlang:nif_error({error, not_loaded}).



-spec esphi_delete(sophia_table(), iolist()|integer()) -> ok | {error, disposed} | {error, {integer(), binary()}}.
esphi_delete(_Tref, _Key) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_delete(sophia_table(), sophia_transaction(), iolist()|integer()) -> ok | {error, disposed} | {error, {integer(), binary()}}.
esphi_delete(_Tref, _TranRef, _Key) ->
	erlang:nif_error({error, not_loaded}).


-spec esphi_put(sophia_table(), iolist()|integer(), iolist()|integer()) -> ok | {error, disposed} | {error, {integer(), binary()}}.
esphi_put(_Tref, _Key, _Value) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_put(sophia_table(), sophia_transaction(), iolist()|integer(), iolist()|integer()) -> ok | {error, disposed} | {error, {integer(), binary()}}.
esphi_put(_Tref, _TranRef, _Key, _Value) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_get(sophia_table(), sophia_transaction(), iolist()|integer(), sophia_return_type()) -> integer() | not_found | {error, disposed|type} | {error, {integer(), binary()}}.
esphi_get(_Tref, _TranRef, _Key, _ValueType) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_get(sophia_table(), iolist()|integer(), sophia_return_type()) -> integer() | not_found | {error, disposed} | {error, {integer(), binary()}}.
esphi_get(_Tref, _Key, _ValueType) ->
	erlang:nif_error({error, not_loaded}).


-spec esphi_update_counter(sophia_table(), iolist()|integer(), integer()) -> ok | not_found | {error, disposed} | {error, {integer(), binary()}}.
esphi_update_counter(_Tref, _Key, _Value) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_update_counter(sophia_table(), sophia_transaction(), iolist()|integer(), integer()) -> ok | not_found | {error, disposed} | {error, {integer(), binary()}}.

esphi_update_counter(_Tref, _TranRef, _Key, _Value) ->
	erlang:nif_error({error, not_loaded}).


-spec esphi_cursor_open(sophia_table(), iolist()|0, sophia_order(), integer()|iolist()) -> {ok, sophia_cursor()} | {error, disposed} | {error, {integer(), binary()}}.
esphi_cursor_open(_Tref, _Prefix, _Order, _Seek) ->
	erlang:nif_error({error, not_loaded}).

-spec esphi_cursor_close(sophia_cursor()) -> ok.
esphi_cursor_close(_Cref) ->
	erlang:nif_error({error, not_loaded}).


-spec esphi_cursor_next(sophia_cursor(), 0|1, sophia_return_type(), sophia_return_type()) -> binary() | {error, disposed|type}| {error, {integer(), binary()}}.
esphi_cursor_next(_Cref, _KeyOnly, _KeyType, _ValueType) ->
	erlang:nif_error({error, not_loaded}).



-spec esphi_backup(sophia_db()) -> ok.
esphi_backup(_DBref) ->
	erlang:nif_error({error, not_loaded}).