#include "lib.h"
#include "array.h"
#include "dict.h"
#include "dlua-script-private.h"
#include "dict-lua-private.h"
#include "dlua-wrapper.h"
struct lua_dict_txn {
pool_t pool;
struct dict_transaction_context *txn;
enum {
STATE_OPEN,
STATE_COMMITTED,
STATE_ABORTED,
} state;
lua_State *L;
const char *username;
};
static int lua_dict_transaction_rollback(lua_State *L);
static int lua_dict_transaction_commit(lua_State *L);
static int lua_dict_set(lua_State *L);
static int lua_dict_unset(lua_State *L);
static int lua_dict_set_timestamp(lua_State *L);
static int lua_dict_set_non_atomic(lua_State *L);
static luaL_Reg lua_dict_txn_methods[] = {
{ "rollback", lua_dict_transaction_rollback },
{ "commit", lua_dict_transaction_commit },
{ "set", lua_dict_set },
{ "unset", lua_dict_unset },
{ "set_timestamp", lua_dict_set_timestamp },
{ "set_non_atomic", lua_dict_set_non_atomic },
{ NULL, NULL },
};
static void sanity_check_txn(lua_State *L, struct lua_dict_txn *txn)
{
switch (txn->state) {
case STATE_OPEN:
return;
case STATE_COMMITTED:
luaL_error(L, "dict transaction already committed");
return;
case STATE_ABORTED:
luaL_error(L, "dict transaction already aborted");
return;
}
i_unreached();
}
static void lua_dict_txn_unref(struct lua_dict_txn *txn)
{
dict_transaction_rollback(&txn->txn);
pool_unref(&txn->pool);
}
DLUA_WRAP_C_DATA(dict_txn, struct lua_dict_txn, lua_dict_txn_unref,
lua_dict_txn_methods);
static int lua_dict_transaction_rollback(lua_State *L)
{
struct lua_dict_txn *txn;
DLUA_REQUIRE_ARGS(L, 1);
txn = xlua_dict_txn_getptr(L, 1, NULL);
sanity_check_txn(L, txn);
txn->state = STATE_ABORTED;
dict_transaction_rollback(&txn->txn);
return 0;
}
static int lua_dict_transaction_commit_continue(lua_State *L,
int status ATTR_UNUSED,
lua_KContext ctx ATTR_UNUSED)
{
if (!lua_isnil(L, -1))
lua_error(L);
lua_pop(L, 1);
return 0;
}
static void
lua_dict_transaction_commit_callback(const struct dict_commit_result *result,
struct lua_dict_txn *txn)
{
switch (result->ret) {
case DICT_COMMIT_RET_OK:
lua_pushnil(txn->L);
break;
case DICT_COMMIT_RET_NOTFOUND:
i_unreached();
case DICT_COMMIT_RET_FAILED:
case DICT_COMMIT_RET_WRITE_UNCERTAIN:
i_assert(result->error != NULL);
lua_pushfstring(txn->L, "dict transaction commit failed: %s",
result->error);
break;
}
dlua_pcall_yieldable_resume(txn->L, 1);
}
static int lua_dict_transaction_commit(lua_State *L)
{
struct lua_dict_txn *txn;
DLUA_REQUIRE_ARGS(L, 1);
txn = xlua_dict_txn_getptr(L, 1, NULL);
sanity_check_txn(L, txn);
txn->state = STATE_COMMITTED;
dict_transaction_commit_async(&txn->txn,
lua_dict_transaction_commit_callback, txn);
return lua_dict_transaction_commit_continue(L,
lua_yieldk(L, 0, 0, lua_dict_transaction_commit_continue), 0);
}
static int lua_dict_set(lua_State *L)
{
struct lua_dict_txn *txn;
const char *key, *value;
DLUA_REQUIRE_ARGS(L, 3);
txn = xlua_dict_txn_getptr(L, 1, NULL);
key = luaL_checkstring(L, 2);
value = luaL_checkstring(L, 3);
lua_dict_check_key_prefix(L, key, txn->username);
dict_set(txn->txn, key, value);
return 0;
}
static int lua_dict_unset(lua_State *L)
{
struct lua_dict_txn *txn;
const char *key;
DLUA_REQUIRE_ARGS(L, 2);
txn = xlua_dict_txn_getptr(L, 1, NULL);
key = luaL_checkstring(L, 2);
lua_dict_check_key_prefix(L, key, txn->username);
dict_unset(txn->txn, key);
return 0;
}
int lua_dict_transaction_begin(lua_State *L)
{
struct lua_dict_txn *txn;
struct dict *dict;
lua_Integer expire_secs = 0;
const char *username = NULL;
pool_t pool;
DLUA_REQUIRE_ARGS_IN(L, 1, 3);
dict = dlua_check_dict(L, 1);
if (lua_gettop(L) >= 2)
username = luaL_checkstring(L, 2);
if (lua_gettop(L) >= 3)
expire_secs = luaL_checkinteger(L, 3);
pool = pool_alloconly_create("lua dict txn", 128);
txn = p_new(pool, struct lua_dict_txn, 1);
txn->pool = pool;
struct dict_op_settings set = {
.username = username,
.expire_secs = expire_secs,
};
txn->txn = dict_transaction_begin(dict, &set);
txn->state = STATE_OPEN;
txn->L = L;
txn->username = p_strdup(txn->pool, username);
xlua_pushdict_txn(L, txn, FALSE);
return 1;
}
static int lua_dict_set_timestamp(lua_State *L)
{
struct lua_dict_txn *txn;
lua_Number tv_sec, tv_nsec;
DLUA_REQUIRE_ARGS(L, 2);
txn = xlua_dict_txn_getptr(L, 1, NULL);
if (dlua_table_get_number_by_str(L, 2, "tv_sec", &tv_sec) <= 0)
luaL_error(L, "tv_sec missing from table");
if (dlua_table_get_number_by_str(L, 2, "tv_nsec", &tv_nsec) <= 0)
luaL_error(L, "tv_nsec missing from table");
struct timespec ts = {
.tv_sec = tv_sec,
.tv_nsec = tv_nsec
};
dict_transaction_set_timestamp(txn->txn, &ts);
return 0;
}
static int lua_dict_set_non_atomic(lua_State *L)
{
struct lua_dict_txn *txn;
DLUA_REQUIRE_ARGS(L, 1);
txn = xlua_dict_txn_getptr(L, 1, NULL);
dict_transaction_set_non_atomic(txn->txn);
return 0;
}