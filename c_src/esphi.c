//-------------------------------------------------------------------
//
// based on eleveldb: Erlang Wrapper for LevelDB (http://code.google.com/p/leveldb/)
//
//
//-------------------------------------------------------------------
#include "esphi.h"


//namespace esphi {
//
//// Atoms (initialized in on_load)
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;
ERL_NIF_TERM ATOM_NOT_FOUND;
ERL_NIF_TERM ATOM_INVALID_REF;
ERL_NIF_TERM ATOM_STOP;
ERL_NIF_TERM ATOM_ROLLEDBACK;
ERL_NIF_TERM ATOM_LOCK;
ERL_NIF_TERM ATOM_UNKNOWN;
ERL_NIF_TERM ATOM_TYPE;
ERL_NIF_TERM ATOM_DISPOSED;


typedef struct {
    volatile int disposed;
    void *env;
} sphia_env;
ErlNifResourceType* sphiaEnvResource;


typedef struct {
    volatile int disposed;
    void *db;
    sphia_env *spenv;

} sphia_db;
ErlNifResourceType* sphiaDbResource;

typedef struct {
    volatile int disposed;
    void *tran;
    sphia_env *spenv;

} sphia_tran;
ErlNifResourceType* sphiaTranResource;


typedef struct {
    volatile int disposed;
    void *cur;
    sphia_db *db;
    void *o;

} sphia_cur;
ErlNifResourceType* sphiaCurResource;

static void env_dispose(ErlNifEnv *env, void *c) {
    sphia_env* spenv = (sphia_env*)c;
    spenv->disposed = 1;
}
static void env_dispose_(ErlNifEnv *env, void *c) {
    sphia_env* spenv = (sphia_env*)c;
    void *p = __sync_lock_test_and_set(&spenv->env, NULL);
    if (p != NULL ) {
        sp_destroy(p);
    }
}

static void db_dispose(ErlNifEnv *env, void *c) {
    sphia_db* db = (sphia_db*)c;
    db->disposed = 1;
}

static void db_dispose_(ErlNifEnv *env, void *c) {
    sphia_db* db = (sphia_db*)c;
    void *p = __sync_lock_test_and_set(&db->db, NULL);
    if (p != NULL ){
        sp_destroy(p);
    }
    enif_release_resource(db->spenv);
}

static void tran_dispose(ErlNifEnv *env, void *c) {
    sphia_tran* tran = (sphia_tran*)c;
    tran->disposed = 1;
}

static void tran_dispose_(ErlNifEnv *env, void *c) {
    sphia_tran* tran = (sphia_tran*)c;
    void *p = __sync_lock_test_and_set(&tran->tran, NULL);
    if (p != NULL ){
        sp_destroy(p);
    }
    enif_release_resource(tran->spenv);
}

static void cur_dispose(ErlNifEnv *env, void *c) {
    sphia_cur* cur = (sphia_cur*)c;
    cur->disposed = 1;
}

static void cur_dispose_(ErlNifEnv *env, void *c) {
    sphia_cur* cur = (sphia_cur*)c;
    void *o = __sync_lock_test_and_set(&cur->o, NULL);
    if (o != NULL ){
        sp_destroy(o);
    }
    void *p = __sync_lock_test_and_set(&cur->cur, NULL);
    if (p != NULL ){
        sp_destroy(p);
    }
    enif_release_resource(cur->db);
}

static char* join(const char* a, const char* b) {
    size_t lena = strlen(a);
    size_t lenb = strlen(b);
    char* buf = enif_alloc(lena + lenb + 1);
    memcpy(buf, a, lena);
    memcpy(buf + lena, b, lenb);
    buf[lena + lenb] = 0x0;
    return buf;
}

static bool startswith(const char *prefix, const char *str) {
    return strncmp(prefix, str, strlen(prefix)) == 0;
}

static char* bcstr(ErlNifBinary* b) {
    char* buf = enif_alloc(b->size + 1);
    strncpy(buf, (const char *)b->data, b->size);
    buf[b->size] = 0x0;
    return buf;
}


static int
upsert_callback_counter(char **result,
                char **key, int *key_size, int key_count,
                char *src, int src_size,
                char *upsert, int upsert_size,
                void *arg)
{
	(void)key;
	(void)key_size;
	(void)key_count;
	(void)arg;
	assert(upsert != NULL);
	char *c = malloc(upsert_size);
	if (c == NULL)
		return -1;
	*result = c;
	if (src == NULL) {
		memcpy(c, upsert, upsert_size);
		return upsert_size;
	}
	assert(src_size == upsert_size);
	memcpy(c, src, src_size);
	*(ErlNifSInt64*)c += *(ErlNifSInt64*)upsert;
	return upsert_size;
}




bool endsWith (char* base, char* str) {
    int blen = strlen(base);
    int slen = strlen(str);
    return (blen >= slen) && (0 == strcmp(base + blen - slen, str));
}

static int sophia_set_sparam(sphia_env* spenv, char* ckey, ErlNifBinary* bvalue) {
    int rc;
    char * cvalue;

    cvalue = bcstr(bvalue);
    if (endsWith(ckey, ".index.upsert") && (strcmp(cvalue, "__COUNTER__") == 0))
        rc = sp_setstring(spenv->env, ckey, (char*)(intptr_t)upsert_callback_counter, 0);
    else
        rc = sp_setstring(spenv->env, ckey, cvalue, 0);
    enif_free(cvalue);
    return rc;
}


static int sophia_apply_params(ErlNifEnv* env, sphia_env* spenv,  ERL_NIF_TERM map) {
    ErlNifMapIterator iter;
    ERL_NIF_TERM key, value;
    ErlNifBinary bkey, bvalue;
    ErlNifSInt64 ivalue;
    int rc;
    char* ckey;

    rc = 0;
    enif_map_iterator_create(env, map, &iter, ERL_NIF_MAP_ITERATOR_FIRST);
    while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
        enif_inspect_iolist_as_binary(env, key, &bkey);
        ckey = bcstr(&bkey);
        if (enif_get_int64(env, value, &ivalue)) {
            rc = sp_setint(spenv->env, ckey, ivalue);
        } else {
            enif_inspect_iolist_as_binary(env, value, &bvalue);
            rc = sophia_set_sparam(spenv, ckey, &bvalue);

        }
        enif_free(ckey);
        if (rc == -1) {
            break;
        }
        enif_map_iterator_next(env, &iter);
    }
    enif_map_iterator_destroy(env, &iter);
    return rc;


}

static bool sophia_validate_map(ErlNifEnv* env,  ERL_NIF_TERM map) {
    ErlNifMapIterator iter;
    ERL_NIF_TERM key, value;
    ErlNifBinary bkey, bvalue;
    ErlNifSInt64 ivalue;

    enif_map_iterator_create(env, map, &iter, ERL_NIF_MAP_ITERATOR_FIRST);
    while (enif_map_iterator_get_pair(env, &iter, &key, &value)) {
        if (!enif_inspect_iolist_as_binary(env, key, &bkey) ||
           (!enif_inspect_iolist_as_binary(env, value, &bvalue) && !enif_get_int64(env, value, &ivalue))
        ) {
            enif_map_iterator_destroy(env, &iter);
            return false;
        }
        enif_map_iterator_next(env, &iter);
    }
    enif_map_iterator_destroy(env, &iter);
    return true;
}

static ERL_NIF_TERM sofia_error(ErlNifEnv* env, sphia_env* spenv, int code, bool dispose) {
    ErlNifBinary err_binary;
    int size;
    char *error = (char *)sp_getstring(spenv->env, "sophia.error", &size);

    if (!size) {
        enif_alloc_binary(0, &err_binary);
    } else {
        enif_alloc_binary(size-1, &err_binary);
        strncpy((char*)err_binary.data, error, size-1);
    }
    free(error);
    if (dispose) {
        env_dispose(env, spenv);
        enif_release_resource(spenv);
    }
    return enif_make_tuple2(env, ATOM_ERROR,
            enif_make_tuple2(env, enif_make_int(env, code), enif_make_binary(env, &err_binary))
    );
}

static ERL_NIF_TERM env_disposed(ErlNifEnv* env) {
    return enif_make_tuple2(env, ATOM_ERROR,  ATOM_DISPOSED);
}
static ERL_NIF_TERM to_bin(ErlNifEnv* env, void* val, int size) {
    ErlNifBinary bvalue;
    enif_alloc_binary(size, &bvalue);
    memcpy((char*)bvalue.data, val, size);
    return enif_make_binary(env, &bvalue);
}
static ERL_NIF_TERM to_int(ErlNifEnv* env, void* val, int size, bool *ok) {
    *ok = true;
    if (size == sizeof(ErlNifSInt64))
        return enif_make_int64(env, *(ErlNifSInt64*)val);
    else  if (size == sizeof(int))
        return enif_make_int(env, *(int*)val);
    else  if (size == sizeof(long))
        return enif_make_long(env, *(long*)val);
    else {
        *ok = false;
        return ATOM_TYPE;
    }
}



static ERL_NIF_TERM esphi_print_info(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_env* spenv;
    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv)) {
        return enif_make_badarg(env);
    }
    if (spenv->disposed == 1)
        return env_disposed(env);

    void *cursor = sp_getobject(spenv->env , NULL);
    void *ptr = NULL;
    while ((ptr = sp_get(cursor, ptr))) {
    char *key = (char*)sp_getstring(ptr, "key", NULL);
    char *value = (char*)sp_getstring(ptr, "value", NULL);
        printf("%s", key);
    if (value)
        printf(" = %s\r\n", value);
    else
        printf(" = \r\n");
    }
    sp_destroy(cursor);

    return ATOM_OK;

}



static ERL_NIF_TERM esphi_last_error(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_env* spenv;
    ErlNifBinary err_binary;
    int size;
    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv)) {
        return enif_make_badarg(env);
    }
    if (spenv->disposed == 1)
        return env_disposed(env);


    char *error = (char *)sp_getstring(spenv->env, "sophia.error", &size);
    if (!size) {
        enif_alloc_binary(0, &err_binary);
    } else {
        enif_alloc_binary(size-1, &err_binary);
        strncpy((char*)err_binary.data, error, size-1);
    }
    free(error);
    return enif_make_binary(env, &err_binary);


}

static ERL_NIF_TERM esphi_transaction_commit(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_tran* tran;
    int rc;
    if (!enif_get_resource(env, argv[0], sphiaTranResource, (void **)&tran)) {
        return enif_make_badarg(env);
    }
    if (tran->disposed == 1)
        return env_disposed(env);
    void *p = __sync_lock_test_and_set(&tran->tran, NULL);
    rc = sp_commit(p);
    switch (rc) {
        case -1:
            return sofia_error(env, tran->spenv, -1, false);
        case 0:
            tran_dispose(env, tran);
            return ATOM_OK;
        case 1:
            tran_dispose(env, tran);
            return enif_make_tuple2(env, ATOM_ERROR, ATOM_ROLLEDBACK);
        case 2:
            __sync_lock_test_and_set(&tran->tran, p);
            return enif_make_tuple2(env, ATOM_ERROR, ATOM_LOCK);
        default:
            assert(0);
//            tran_dispose(env, tran);
//            return enif_make_tuple2(env, ATOM_ERROR, ATOM_UNKNOWN);

    }
}

static ERL_NIF_TERM esphi_transaction_rollback(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_tran* tran;
    if (!enif_get_resource(env, argv[0], sphiaTranResource, (void **)&tran)) {
        return enif_make_badarg(env);
    }
    tran_dispose(env, tran);
    return ATOM_OK;

}

static ERL_NIF_TERM esphi_transaction_begin(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_tran* tran;
    sphia_env* spenv;
    ERL_NIF_TERM res;

    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv)) {
        return enif_make_badarg(env);
    }
    if (spenv->disposed == 1)
        return env_disposed(env);

    tran = (sphia_tran*)enif_alloc_resource(sphiaTranResource,  sizeof(sphia_tran));
    if (tran == NULL) return enif_make_badarg(env);

    tran->tran = sp_begin(spenv->env);
    tran->spenv = spenv;
    tran->disposed = 0;

    if (tran->tran == NULL) {
        enif_release_resource(tran);
        return sofia_error(env, spenv, -1, false);
    } else {
        res = enif_make_resource(env, tran);
        enif_release_resource(tran);
        enif_keep_resource(spenv);
        return enif_make_tuple2(env, ATOM_OK, res);
    }

}

static ERL_NIF_TERM esphi_cursor_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    sphia_cur* cur;
    ERL_NIF_TERM res;
    ErlNifBinary bprefix;
    char order[10];
    ErlNifBinary bkey;
    ErlNifSInt64 ikey;
    bool is_binary;
    bool use_prefix;
    int no_prefix;
    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        (!(use_prefix = enif_inspect_iolist_as_binary(env, argv[1], &bprefix)) && !(enif_get_int(env, argv[1], &no_prefix))) ||
        !enif_get_atom(env, argv[2], order, 10, ERL_NIF_LATIN1) ||
        (!(is_binary = enif_inspect_iolist_as_binary(env, argv[3], &bkey)) && !enif_get_int64(env, argv[3], &ikey))) {
        return enif_make_badarg(env);
    }
    if (!use_prefix && (no_prefix != 0))
        return enif_make_badarg(env);

    if ((db->disposed == 1) || (db->spenv->disposed == 1))
        return env_disposed(env);
    cur = (sphia_cur*)enif_alloc_resource(sphiaCurResource,  sizeof(sphia_cur));
    if (cur == NULL) return enif_make_badarg(env);
    cur->cur = sp_cursor(db->spenv->env);

    if (cur->cur == NULL) {
        enif_release_resource(cur);
        return sofia_error(env, db->spenv, -1, false);
    }

    cur->o = sp_document(db->db);
    if (cur->o == NULL) {
        enif_release_resource(cur);
        return sofia_error(env, db->spenv, -1, false);
    }
    sp_setstring(cur->o, "order", order, 0);
    if (use_prefix)
        sp_setstring(cur->o, "prefix", bprefix.data, bprefix.size);

    if (is_binary && (bkey.size > 0))
        sp_setstring(cur->o, "key", bkey.data, bkey.size);
    else if (!is_binary)
        sp_setstring(cur->o, "key", &ikey, sizeof(ikey));

    cur->disposed = 0;
    cur->db = db;

    res = enif_make_resource(env, cur);
    enif_release_resource(cur);
    enif_keep_resource(db);
    return enif_make_tuple2(env, ATOM_OK, res);

}

static ERL_NIF_TERM esphi_cursor_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_cur* cur;
    if (!enif_get_resource(env, argv[0], sphiaCurResource, (void **)&cur)) {
        return enif_make_badarg(env);
    }
    cur_dispose(env, cur);
    return ATOM_OK;
}

static ERL_NIF_TERM esphi_cursor_next(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_cur* cur;
    int key_only;
    int res_key_type, res_val_type;
    void* o;
    ERL_NIF_TERM res_val, res;

    if (!enif_get_resource(env, argv[0], sphiaCurResource, (void **)&cur) ||
        !enif_get_int(env, argv[1], &key_only) ||
        !enif_get_int(env, argv[2], &res_key_type) ||
        !enif_get_int(env, argv[3], &res_val_type)) {
        return enif_make_badarg(env);
    }
    if (((key_only != SOPHIA_KEY_ONLY) && (key_only != SOPHIA_FULL_OBJECT)) ||
        ((res_key_type != SOPHIA_BINARY) && (res_key_type != SOPHIA_INT)) ||
        ((res_val_type != SOPHIA_BINARY) && (res_val_type != SOPHIA_INT)))
            return enif_make_badarg(env);
    if ((cur->disposed == 1) || (cur->db->disposed == 1) || (cur->db->spenv->disposed == 1))
        return env_disposed(env);
    o = __sync_lock_test_and_set(&cur->o, NULL);
    if (!o)
        return ATOM_STOP;
    o = sp_get(cur->cur, o);
    if (o) {
        int size;
        bool ok = true;
        void *key = sp_getstring(o, "key", &size);
        if (res_key_type == SOPHIA_BINARY) {
            res = to_bin(env, key, size);
        } else {
            res = to_int(env, key, size, &ok);
        }
        if (!ok)
            res = enif_make_tuple2(env, ATOM_ERROR, ATOM_TYPE);
        else if (key_only == SOPHIA_FULL_OBJECT) {
            ok = true;
            void *value = sp_getstring(o, "value", &size);
            if (res_val_type == SOPHIA_BINARY) {
                res_val = to_bin(env, value, size);
            } else {
                res_val = to_int(env, value, size, &ok);
            }
            if (!ok)
                res = enif_make_tuple2(env, ATOM_ERROR, ATOM_TYPE);
            else {
                res = enif_make_tuple2(env, res, res_val);
            }
        }
        __sync_lock_test_and_set(&cur->o, o);

    } else {
        res = ATOM_STOP;
    }
    return res;

}

static ERL_NIF_TERM esphi_table_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db)) {
        return enif_make_badarg(env);
    }
    db_dispose(env, db);
    return ATOM_OK;
}

static ERL_NIF_TERM esphi_table_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary btable;
    sphia_db* db;
    sphia_env* spenv;
    char* ctable, * ctable0;
    ERL_NIF_TERM res;

    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv)||
        !enif_inspect_iolist_as_binary(env, argv[1], &btable)) {
        return enif_make_badarg(env);
    }
    if (spenv->disposed == 1)
        return env_disposed(env);
    db = (sphia_db*)enif_alloc_resource(sphiaDbResource,  sizeof(sphia_db));
    if (db == NULL) return enif_make_badarg(env);
    ctable = bcstr(&btable);
    if (!startswith("db.", ctable)) {
        ctable0 = join("db.", ctable);
        enif_free(ctable);
        ctable = ctable0;
    }



    db->db = sp_getobject(spenv->env, ctable);
    enif_free(ctable);
    db->spenv = spenv;
    db->disposed = 0;

    if (db->db == NULL) {
        enif_release_resource(db);
        return sofia_error(env, spenv, -1, false);
    } else {
        res = enif_make_resource(env, db);
        enif_release_resource(db);
        enif_keep_resource(spenv);
        return enif_make_tuple2(env, ATOM_OK, res);
    }
}

static ERL_NIF_TERM esphi_param_get(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_env* spenv;
    ErlNifBinary bkey, bvalue;
    char* ckey, * cvalue;
    void *val;
    ErlNifSInt64 ivalue;
    int size;
    int res_type;

    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &bkey) ||
        !enif_get_int(env, argv[2], &res_type)) {
        return enif_make_badarg(env);
    }
    if ((res_type != SOPHIA_BINARY) && (res_type != SOPHIA_INT))
        return enif_make_badarg(env);

    if (spenv->disposed == 1)
        return env_disposed(env);

    if (res_type == SOPHIA_BINARY) {
        ckey = bcstr(&bkey);
        val = sp_getstring(spenv->env, ckey, &size);
        enif_free(ckey);
        if (val == NULL) {
            return sofia_error(env, spenv, -1, false);
        } else {
            cvalue = (char *)val;
            enif_alloc_binary(size-1, &bvalue);
            strncpy((char*)bvalue.data, cvalue, size-1);
            return enif_make_binary(env, &bvalue);
        }
    } else {
        ckey = bcstr(&bkey);
        ivalue = sp_getint(spenv->env, ckey);
        enif_free(ckey);
        if (ivalue == -1) {
            return sofia_error(env, spenv, ivalue, false);
        } else {
            return enif_make_int64(env, ivalue);
        }
    }

}


static ERL_NIF_TERM esphi_param_set(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_env* spenv;
    ErlNifBinary bkey, bvalue;
    ErlNifSInt64 ivalue;
    bool is_binary;
    char* ckey;
    int rc;

    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &bkey) ||
        (!(is_binary = enif_inspect_iolist_as_binary(env, argv[2], &bvalue)) && !enif_get_int64(env, argv[2], &ivalue))) {
        return enif_make_badarg(env);
    }
    if (spenv->disposed == 1)
        return env_disposed(env);
    ckey = bcstr(&bkey);
    if (is_binary) {
        rc = sophia_set_sparam(spenv, ckey, &bvalue);
    } else {
        rc = sp_setint(spenv->env, ckey, ivalue);
    }

    enif_free(ckey);
    if (rc == -1) {
        return sofia_error(env, spenv, rc, false);
    } else {
        return ATOM_OK;
    }

}

static ERL_NIF_TERM esphi_put(ErlNifEnv* env, sphia_db* db, void* dbt, bool is_key_binary,
        bool is_val_binary, ErlNifBinary* bkey, ErlNifBinary* bval, ErlNifSInt64 ikey, ErlNifSInt64 ival) {
    int rc;

    void *o = sp_document(db->db);
    if (is_key_binary) {
        rc = sp_setstring(o, "key", bkey->data, bkey->size);
    } else {
        rc = sp_setstring(o, "key", &ikey, sizeof(ikey));
    }
    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    }

    if (is_val_binary) {
        rc = sp_setstring(o, "value", bval->data, bval->size);
    } else {
        rc = sp_setstring(o, "value", &ival, sizeof(ival));
    }
    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    }
    rc = sp_set(dbt, o);

    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    } else {
        return ATOM_OK;
    }
}

static ERL_NIF_TERM esphi_put_notran(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    ErlNifBinary bkey, bval;
    ErlNifSInt64 ikey, ival;
    bool is_key_binary, is_val_binary;

    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        (!(is_key_binary = enif_inspect_iolist_as_binary(env, argv[1], &bkey)) && !enif_get_int64(env, argv[1], &ikey)) ||
        (!(is_val_binary = enif_inspect_iolist_as_binary(env, argv[2], &bval)) && !enif_get_int64(env, argv[2], &ival))) {
        return enif_make_badarg(env);
    }
    if (db->disposed == 1)
        return env_disposed(env);

    return esphi_put(env, db,  db->db, is_key_binary, is_val_binary, &bkey, &bval, ikey, ival);

}

static ERL_NIF_TERM esphi_put_tran(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    sphia_tran* tran;
    ErlNifBinary bkey, bval;
    ErlNifSInt64 ikey, ival;
    bool is_key_binary, is_val_binary;

    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        !enif_get_resource(env, argv[1], sphiaTranResource, (void **)&tran) ||
        (!(is_key_binary = enif_inspect_iolist_as_binary(env, argv[2], &bkey)) && !enif_get_int64(env, argv[2], &ikey)) ||
        (!(is_val_binary = enif_inspect_iolist_as_binary(env, argv[3], &bval)) && !enif_get_int64(env, argv[3], &ival))) {
        return enif_make_badarg(env);
    }
    if ((db->disposed == 1) || (tran->disposed == 1))
       return env_disposed(env);

    return esphi_put(env, db,  tran->tran, is_key_binary, is_val_binary, &bkey, &bval, ikey, ival);

}

static ERL_NIF_TERM esphi_update_counter(ErlNifEnv* env, sphia_db* db, void* dbt,
                bool is_key_binary, ErlNifBinary* bkey, ErlNifSInt64 ikey, ErlNifSInt64 ival) {
    int rc;
    void *o = sp_document(db->db);
    if (is_key_binary) {
        rc = sp_setstring(o, "key", bkey->data, bkey->size);
    } else {
        rc = sp_setstring(o, "key", &ikey, sizeof(ikey));
    }
    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    }

    rc = sp_setstring(o, "value", &ival, sizeof(ival));

    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    }

    rc = sp_upsert(dbt, o);

    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    } else {
        return ATOM_OK;
    }

}

static ERL_NIF_TERM esphi_update_counter_notran(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    ErlNifBinary bkey;
    ErlNifSInt64 ikey, ival;
    bool is_key_binary;

    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        (!(is_key_binary = enif_inspect_iolist_as_binary(env, argv[1], &bkey)) && !enif_get_int64(env, argv[1], &ikey)) ||
        !enif_get_int64(env, argv[2], &ival)) {
        return enif_make_badarg(env);
    }
    if (db->disposed == 1)
        return env_disposed(env);
    return esphi_update_counter(env, db, db->db, is_key_binary, &bkey, ikey, ival);

}

static ERL_NIF_TERM esphi_update_counter_tran(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    sphia_tran* tran;
    ErlNifBinary bkey;
    ErlNifSInt64 ikey, ival;
    bool is_key_binary;

    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        !enif_get_resource(env, argv[1], sphiaTranResource, (void **)&tran) ||
        (!(is_key_binary = enif_inspect_iolist_as_binary(env, argv[2], &bkey)) && !enif_get_int64(env, argv[2], &ikey)) ||
        !enif_get_int64(env, argv[3], &ival)) {
        return enif_make_badarg(env);
    }
    if ((db->disposed == 1) || (tran->disposed == 1))
       return env_disposed(env);
    return esphi_update_counter(env, db, tran->tran, is_key_binary, &bkey, ikey, ival);
}

static ERL_NIF_TERM esphi_get(ErlNifEnv* env, sphia_db* db, void* dbt,  bool is_binary, ErlNifBinary* bkey,
                              ErlNifSInt64 ikey, int res_type) {
    int rc;
    ERL_NIF_TERM res;

    void *o = sp_document(db->db);
    if (is_binary) {
        rc = sp_setstring(o, "key", bkey->data, bkey->size);
    } else {
        rc = sp_setstring(o, "key", &ikey, sizeof(ikey));
    }
    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    }

    o = sp_get(dbt, o);
    if (o) {

        int size;
        bool ok = true;

        void *value = sp_getstring(o, "value", &size);
        if (res_type == SOPHIA_BINARY) {
            res = to_bin(env, value, size);
        } else {
            res = to_int(env, value, size, &ok);
        }
        sp_destroy(o);
        if (ok)
            return res;
        else
            return enif_make_tuple2(env, ATOM_ERROR, ATOM_TYPE);
    } else {
        return ATOM_NOT_FOUND;
    }

}

static ERL_NIF_TERM esphi_get_notran(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    ErlNifBinary bkey;
    ErlNifSInt64 ikey;
    bool is_binary;
    int res_type;
    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        (!(is_binary = enif_inspect_iolist_as_binary(env, argv[1], &bkey)) && !enif_get_int64(env, argv[1], &ikey)) ||
        !enif_get_int(env, argv[2], &res_type)) {
        return enif_make_badarg(env);
    }
    if ((res_type != SOPHIA_BINARY) && (res_type != SOPHIA_INT))
        return enif_make_badarg(env);

    if (db->disposed == 1)
        return env_disposed(env);
    return esphi_get(env, db, db->db, is_binary, &bkey, ikey, res_type);
}

static ERL_NIF_TERM esphi_get_tran(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    sphia_tran* tran;
    ErlNifBinary bkey;
    ErlNifSInt64 ikey;
    bool is_binary;
    int res_type;
    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        !enif_get_resource(env, argv[1], sphiaTranResource, (void **)&tran) ||
        (!(is_binary = enif_inspect_iolist_as_binary(env, argv[2], &bkey)) && !enif_get_int64(env, argv[2], &ikey)) ||
        !enif_get_int(env, argv[3], &res_type)) {
        return enif_make_badarg(env);
    }
    if ((res_type != SOPHIA_BINARY) && (res_type != SOPHIA_INT))
        return enif_make_badarg(env);

    if ((db->disposed == 1) || (tran->disposed == 1))
        return env_disposed(env);
    return esphi_get(env, db, tran->tran, is_binary, &bkey, ikey, res_type);
}

static ERL_NIF_TERM esphi_delete(ErlNifEnv* env, sphia_db* db, void* dbt, bool is_binary, ErlNifBinary* bkey, ErlNifSInt64 ikey){
    int rc;
    void *o = sp_document(db->db);
    if (is_binary) {
        rc = sp_setstring(o, "key", bkey->data, bkey->size);
    } else {
        rc = sp_setstring(o, "key", &ikey, sizeof(ikey));
    }
    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    }
    rc = sp_delete(dbt, o);
    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    } else {
        return ATOM_OK;
    }
}

static ERL_NIF_TERM esphi_delete_notran(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    ErlNifBinary bkey;
    ErlNifSInt64 ikey;
    bool is_binary;
    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        (!(is_binary = enif_inspect_iolist_as_binary(env, argv[1], &bkey)) && !enif_get_int64(env, argv[1], &ikey))) {
        return enif_make_badarg(env);
    }
    if (db->disposed == 1)
        return env_disposed(env);
    return esphi_delete(env, db, db->db, is_binary, &bkey, ikey);
}

static ERL_NIF_TERM esphi_delete_tran(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    sphia_tran* tran;
    ErlNifBinary bkey;
    ErlNifSInt64 ikey;
    bool is_binary;
    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db) ||
        !enif_get_resource(env, argv[1], sphiaTranResource, (void **)&tran) ||
        (!(is_binary = enif_inspect_iolist_as_binary(env, argv[2], &bkey)) && !enif_get_int64(env, argv[2], &ikey))) {
        return enif_make_badarg(env);
    }
    if ((db->disposed == 1) || (tran->disposed == 1))
        return env_disposed(env);
    return esphi_delete(env, db, tran->tran, is_binary, &bkey, ikey);
}

static ERL_NIF_TERM esphi_backup(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_env* spenv;
    int rc;
    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv)) {
        return enif_make_badarg(env);
    }
    if (spenv->disposed == 1)
        return env_disposed(env);

    rc = sp_setint(spenv->env, "backup.run", 0);
    if (rc == -1) {
        return sofia_error(env, spenv, rc, false);
    } else {
        return ATOM_OK;
    }

}


static ERL_NIF_TERM esphi_table_create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_env* spenv;
    int rc;
    size_t mlen;
    char* cname;
    ErlNifBinary dbname;


    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv) ||
        !enif_inspect_iolist_as_binary(env, argv[1], &dbname) ||
        !enif_get_map_size(env, argv[2], &mlen) ) {
            return enif_make_badarg(env);
    }
    if (!sophia_validate_map(env, argv[1])) {
        return enif_make_badarg(env);
    }

    if (spenv->disposed == 1)
        return env_disposed(env);

    cname = bcstr(&dbname);
    rc = sp_setstring(spenv->env, "db", cname, 0);
    enif_free(cname);
    if (rc == -1) {
        return sofia_error(env, spenv, rc, false);
    }

    rc = sophia_apply_params(env, spenv,  argv[2]);
    if (rc == -1) {
        return sofia_error(env, spenv, rc, false);
    } else {
        return ATOM_OK;
    }
}

static ERL_NIF_TERM esphi_table_drop(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_db* db;
    int rc;
    if (!enif_get_resource(env, argv[0], sphiaDbResource, (void **)&db)) {
        return enif_make_badarg(env);
    }
    if (db->disposed == 1)
        return env_disposed(env);
    rc = sp_drop(db->db);
    if (rc == -1) {
        return sofia_error(env, db->spenv, rc, false);
    } else {
        return ATOM_OK;
    }
}



static ERL_NIF_TERM esphi_db_open(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ErlNifBinary path;
    unsigned llen;
    size_t mlen;
    ERL_NIF_TERM h, t;
    ErlNifBinary dbname;
    sphia_env* spenv;
    ERL_NIF_TERM res;

    char* cvalue;
    char* cpath;

    int rc;



    if (!enif_inspect_iolist_as_binary(env, argv[0], &path) ||
        !enif_get_list_length(env, argv[1], &llen) ||
        !enif_get_map_size(env, argv[2], &mlen) ) {
            return enif_make_badarg(env);
    }

    //validate db names, must be iolist each
    ERL_NIF_TERM p = argv[1];
    while (enif_get_list_cell(env, p, &h, &t)) {
        if (!enif_inspect_iolist_as_binary(env, h, &dbname)) {
            return enif_make_badarg(env);
        }
        p = t;
    }

    //validate params, must be iolist=>iolist|int each
    if (!sophia_validate_map(env, argv[1])) {
        return enif_make_badarg(env);
    }

    spenv = (sphia_env*)enif_alloc_resource(sphiaEnvResource,  sizeof(sphia_env));
    if (spenv == NULL) return enif_make_badarg(env);

    spenv->env = sp_env();
    spenv->disposed = 0;

    //path to the env
    cpath = bcstr(&path);
    rc = sp_setstring(spenv->env, "sophia.path", cpath, 0);
    enif_free(cpath);
    if (rc == -1){
		return sofia_error(env, spenv, rc, true);
	}

	//databases
    p = argv[1];
    while (enif_get_list_cell(env, p, &h, &t)) {
        enif_inspect_iolist_as_binary(env, h, &dbname);
        cvalue = bcstr(&dbname);
        rc = sp_setstring(spenv->env, "db", cvalue, 0);
        enif_free(cvalue);
        if (rc == -1) {
		    return sofia_error(env, spenv, rc, true);
		}
        p = t;
    }

    //params
    rc = sophia_apply_params(env, spenv,  argv[2]);
    if (rc == -1){
        return sofia_error(env, spenv, rc, true);
    }

    rc = sp_open(spenv->env);
    if (rc == -1){
        return sofia_error(env, spenv, rc, true);
    }


    res = enif_make_resource(env, spenv);
    enif_release_resource(spenv);

    return enif_make_tuple2(env, ATOM_OK, res);

}

static ERL_NIF_TERM esphi_db_close(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    sphia_env* spenv;
    if (!enif_get_resource(env, argv[0], sphiaEnvResource, (void **)&spenv)) {
        return enif_make_badarg(env);
    }
    env_dispose(env, spenv);
    return ATOM_OK;

}


static void on_unload(ErlNifEnv *env, void *priv_data)
{

}


static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
    int ret_val = 0;
    sphiaEnvResource = enif_open_resource_type(env, NULL, "sphiaEnvResource", &env_dispose_, ERL_NIF_RT_CREATE, NULL);
    if(sphiaEnvResource == NULL) return -1;

    sphiaDbResource = enif_open_resource_type(env, NULL, "sphiaDbResource", &db_dispose_, ERL_NIF_RT_CREATE, NULL);
    if(sphiaDbResource == NULL) return -1;

    sphiaTranResource = enif_open_resource_type(env, NULL, "sphiaTranResource", &tran_dispose_, ERL_NIF_RT_CREATE, NULL);
    if(sphiaTranResource == NULL) return -1;

    sphiaCurResource = enif_open_resource_type(env, NULL, "sphiaCurResource", &cur_dispose_, ERL_NIF_RT_CREATE, NULL);
    if(sphiaCurResource == NULL) return -1;


#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error");
    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");
    ATOM(ATOM_INVALID_REF, "invalid_ref");


    ATOM(ATOM_NOT_FOUND, "not_found");

    ATOM(ATOM_STOP, "stop");
    ATOM(ATOM_ROLLEDBACK, "rolled_back");
    ATOM(ATOM_LOCK, "lock");
    ATOM(ATOM_UNKNOWN, "unknown");
    ATOM(ATOM_TYPE, "type");

    ATOM(ATOM_DISPOSED, "disposed");


#undef ATOM


    return ret_val;
}


static ErlNifFunc nif_funcs[] =
{
    {"esphi_db_open", 3, esphi_db_open, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"esphi_db_close", 1, esphi_db_close},

    {"esphi_param_set", 3, esphi_param_set},
    {"esphi_param_get", 3, esphi_param_get},
    {"esphi_print_info", 1, esphi_print_info},
    {"esphi_last_error", 1, esphi_last_error},

    {"esphi_table_open", 2, esphi_table_open, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"esphi_table_close", 1, esphi_table_close},
    {"esphi_table_create", 3, esphi_table_create},
    {"esphi_table_drop", 1, esphi_table_drop, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"esphi_transaction_begin", 1, esphi_transaction_begin},
    {"esphi_transaction_commit", 1, esphi_transaction_commit, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"esphi_transaction_rollback", 1, esphi_transaction_rollback},

    {"esphi_delete", 2, esphi_delete_notran, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"esphi_delete", 3, esphi_delete_tran, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"esphi_put", 3, esphi_put_notran, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"esphi_put", 4, esphi_put_tran, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"esphi_update_counter", 3, esphi_update_counter_notran, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"esphi_update_counter", 4, esphi_update_counter_tran, ERL_NIF_DIRTY_JOB_IO_BOUND},


    {"esphi_get", 3, esphi_get_notran, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"esphi_get", 4, esphi_get_tran, ERL_NIF_DIRTY_JOB_IO_BOUND},

    {"esphi_cursor_open", 4, esphi_cursor_open, ERL_NIF_DIRTY_JOB_IO_BOUND},
    {"esphi_cursor_close", 1, esphi_cursor_close},
    {"esphi_cursor_next", 4, esphi_cursor_next, ERL_NIF_DIRTY_JOB_IO_BOUND},



    {"esphi_backup", 1, esphi_backup}

};


ERL_NIF_INIT(esphi, nif_funcs, &on_load, NULL, NULL, &on_unload)

