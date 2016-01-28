Esphi - an Erlang dirty-NIF-based interface to Sophia embeddable database
=========
Sophia database on github: [https://github.com/pmwkaa/sophia](https://github.com/pmwkaa/sophia)
Sophia home page:  [sophia.systems](http://sophia.systems/)  

The library is loosely based on [eleveldb](https://github.com/basho/eleveldb).


The library uses experimental dirty NIF feature from the latest Erlang. In order to build it and
use it you need to have recent Erlang distribution compiled with the `--enable-dirty-schedulers`
configuration flag. It is likely possible to use these nifs with standard schedulers too, as the Sophia
in most cases should be quite fast. To do this, edit the `esphi.c` file and remove all references to
ERL_NIF_DIRTY_JOB_IO_BOUND.

The library should be thread-safe. Pay attention: at the moment the `*_close()` functions won't
delete or dispose pointers in any way, they just would set a flag which prevents the object from
being used any more (this behaviour is going to be changed in future versions). All destructors are
called only when Erlang garbage collects references to the allocated structures, which contain disposable
pointers. So in order to avoid memory leaks ensure that the garbage collector can actually collect old
references, just like with binaries. It is better to use short-lived processes for cursors and transactions,
which are usually created and destroyed very often. 
Cursors are not thread safe. Iterating over cursors from multiple erlang processes simultaneously won't crash
the VM but you will get `stop` errors instead of data. Also be careful with transactions, don't commit/rollback
the same transaction from multiple processes simultaneously.


Terminology
---------
The esphi and sophia docs have different names for objects, so pay attention to it when you read the docs:
```
|   Sophia    |   Esphi  |
|------------------------|
| environment | database |
| database    | table    |

```


What works (in Sophia terms):
---------
  * Multiple environments, each with multiple databases
  * All string and int configuration parameters
  * Databases with string and int64 keys
  * Transactions
  * Cursors with prefix, order and seek
  * Get/Put/Delete operations, with keys and values of int or string type
  * Upsert, for counters only (new_value = old_value + increment)
  * Async backups
  
What is not implemented yet:
---------
  * Point-in-Time Views
  * Multikeys
  * Function and object configuration parameters
  * Anything async
  * Sync backups
  * More types of upserts, besides counters. It should be easy to add more though
  * Other features which I'm not avare of yet
  
  
Some things to know:
---------
  * !!!! Seems like reopening databases and tables is not safe at the moment, as the the shutdown itself is
  initiated by erlang garbage collector, which for long-running proceses is quite unpredictable. As
  the result, data corruption is probably possible. So after you opened a DB keep it opened until
  you restart the VM. Or ensure somehow that the garbage collector collected all references to the DB
  and all it's dependent objects, and Sophia itself has completed all its background operations.
  * All references, including cursors, are freed only when erlang garbage collects the reference. Table and DB 
  references won't get garbage collected until all the objects which reference them get garbage collected.
  This behaviour might change in future versions with deterministic `close`.
  * Please don't serialize references to the Sophia objects in any way, including attempts to send them to 
  other Erlang nodes. In the best case it just won't work.
  * Sophia does not store DB schema. You need to supply schema and configuration parameters each time you
  open the database.
  * Many operations, such as dropping a table, doing a backup etc are async. You may encounter multiple issues doing
  this in concurrent mode. For example, if you initiate backup when another backup is still running,
  the new backup will be just silently ignored. 
  * Typechecking is weak. For example, when you Get as integer some value, which contains random binary, 
  you will get either `{error, type}` or some `int` value if the data fits in any of int types,
  which is probably not what you would want too.
  * There's race between error and returning its' description with the `get_last_error`.
  * Check return codes. Many params can be set only with the `esphi_db_open` call, and if you set
  anything manually, check if the operation was actually successful or not.
  
  
Usage
===========
`$ make && erl -pa ebin`

Open database in `/tmp/db1`, with three tables, `t1`, `t2`, `t3`. The `t2` table has integer key;
the `t3` table is used for counters. Pay attention to the ` "db.t3.index.upsert" => "__COUNTER__"`
construction - it is the only parameter which is preprocessed before getting sent to the Sophia,
the `__COUNTER__` special value is replaced with a pointer to the merge function with
`Val = OldVal + NewVal` semantics. The full list of parameters can be found in the Sophia documentation.

```Erlang
{ok, Db} = esphi:esphi_db_open(
                "/tmp/db1", 
                ["t1", "t2", "t3"], 
                #{"db.t2.index.key" => "u64", "db.t3.index.upsert" => "__COUNTER__", "backup.path" => "/tmp/backup"}
           ).
```

Open tables
-----------

```Erlang
{ok, T1} = esphi:esphi_table_open(Db, "t1").
{ok, T2} = esphi:esphi_table_open(Db, "t2").
{ok, T3} = esphi:esphi_table_open(Db, "t3").
{error, {-1, ErrMsg}} = esphi:esphi_table_open(Db, "t4").
ok = esphi:esphi_table_create(Db, "t4", #{}).   
{ok, T4} = esphi:esphi_table_open(Db, "t4").
```

Some CRUD, without transactions
------------
Both keys and values can be of either of iolist() or integer() types. iolist() will be converted to binary().
The last parameter in the `esphi_get` fuction can be either `0` - return binary or `1` - return int.
```Erlang
ok = esphi:esphi_put(T1, <<"k1">>, <<"v1">>).
ok =  esphi:esphi_put(T1, 10, <<"v2">>).       
ok = esphi:esphi_put(T1, <<"k2">>, 1000).     
ok = esphi:esphi_put(T1, 10, <<"v2">>).     
<<"v1">> = esphi:esphi_get(T1, <<"k1">>, 0).
{error,type} = esphi:esphi_get(T1, <<"k1">>, 1).
<<"v2">> = esphi:esphi_get(T1, 10, 0).  
<<232,3,0,0,0,0,0,0>> = esphi:esphi_get(T1, <<"k2">>, 0).
1000 = esphi:esphi_get(T1, <<"k2">>, 1).
ok = esphi:esphi_delete(T1, 10).   
not_found = esphi:esphi_get(T1, 10, 0).    
```

Counters
-----------
Unlike in Mnesia, it is impossible to get the result of increment immediatelly, without doing an explicit
read. Moreover, if you think about using transactions to do update_counter and then read - don't. You will
get a lot of automatic rollbacks under the load, as transactions use optimistic locking and will
refuse to commit if value has been changed by another transaction. So use these counters for
counting likes, not for generating sequences of unique IDs. For sequences serialize it with a `gen_server`.  
Under the hood the counters are implemented with Upsert feature of the Sophia. Don't forget to configure
the table, which is going to be used to store counters, with the
`"db.[table].index.upsert" => "__COUNTER__"` parameter.

```Erlang
ok = esphi:esphi_update_counter(T3, "cnt", 1000).
ok = esphi:esphi_update_counter(T3, "cnt", 1).   
1001 = esphi:esphi_get(T3, "cnt", 1).  
```
You may also use counters with transactions.

Transactions
------------
Transactions can span multiple tables. Optimistic locking, so if there are two tranactions updating
the same keys, the second one will be automatically rolled back on commit.  
Possible errors of commit:  
`{error, disposed}` - Transaction has been already disposed;  
`{error, rolled_back}` - Transaction rolled back because of conflict with another transaction;  
`{error, lock}` - Deadlocked with another transaction. You can manually roll back some of locked;  
`{error, busy}` - The transaction is about to get commited by another process  


```Erlang
{ok, A1} = esphi:esphi_transaction_begin(Db).
{ok, A2} = esphi:esphi_transaction_begin(Db).
{ok, A3} = esphi:esphi_transaction_begin(Db).
ok = esphi:esphi_put(T1, A1, <<"tk">>, <<"vk">>).
ok = esphi:esphi_put(T1, A2, <<"tk">>, <<"vk2">>).
ok = esphi:esphi_update_counter(T3, A1, "cnt", 1).  
ok = esphi:esphi_put(T1, A3, <<"tk">>, <<"vk3">>).
ok = esphi:esphi_put(T2, A1, <<"k2">>, <<"v2">>).  
ok = esphi:esphi_put(T2, A2, <<"k2">>, 10).
ok = esphi:esphi_transaction_commit(A1).
ok = esphi:esphi_transaction_rollback(A3).
{error, rolled_back} = esphi:esphi_transaction_commit(A2).
<<"vk">> = esphi:esphi_get_s(T1, <<"tk">>).
```

Cursors
-----------
One way only, ascending and descending order, with optional prefix filter for key (for binary keys only).  
Allowed orders:  
`>=` - traverse with increasing order  
`>` - traverse with increasing order, skip first key for prefix  
`<=` - traverse with decreasing order  
`<` - traverse with decreasing order, skip first key for prefix  
The second parameter in the `esphi_cursor_next`: `0` to return just key, `1` to return both key and value.
The third and fourth parameters have the same meaning as the last parameter in `esphi_get` function, for
key (former) and for value (latter).  
Put `0` in the second parameter of the `esphi_cursor_open` if you don't want to do prefix filter (for
example for the tables with `int` keys you can't do prefix seek even if you want so). Put `""` in the
last parameter if you don't want to do seek.
```Erlang
ok = esphi:esphi_put(T1, <<"sa1">>, <<"v1">>).
ok = esphi:esphi_put(T1, <<"sa2">>, <<"v2">>).
ok = esphi:esphi_put(T1, <<"sa3">>, <<"v3">>).
ok = esphi:esphi_put(T1, <<"z0">>, <<"vz0">>).
ok = esphi:esphi_put(T1, <<"sa4">>, <<"v4">>). 
{ok, C2} = esphi:esphi_cursor_open(T1, "s", '>=', "sa2").
{<<"sa2">>,<<"v2">>} = esphi:esphi_cursor_next(C2, 1, 0, 0).
<<"sa3">> = esphi:esphi_cursor_next(C2, 0, 0, 0).
{<<"sa4">>,<<"v4">>} = esphi:esphi_cursor_next(C2, 1, 0, 0).
stop = esphi:esphi_cursor_next(C2, 1, 0, 0).
```

Backup
-----------
Backups are async. Wait for the previous backup to finish its' job before running the next one. See
http://sophia.systems/v2.1/conf/backup.html and http://sophia.systems/v2.1/admin/backup.html  
You must set `"backup.path"` parameter in `esphi_db_open`, else backups won't work at all. 

```Erlang
ok = esphi:esphi_backup(Db).
0 = esphi:esphi_param_get(Db, "backup.active", 1). %%or 1
1 = esphi:esphi_param_get(Db, "backup.last", 1).
1 = esphi:esphi_param_get(Db, "backup.last_complete", 1).
```