#include "lib.h"
#include "array.h"
#include "dict.h"
#include "dlua-script-private.h"
#include "dict-lua-private.h"
#include "dlua-wrapper.h"
struct lua_dict_iter {
pool_t pool;
struct dict_iterate_context *iter;
ARRAY(int) refs;
int error_ref;
lua_State *L;
bool yielded:1;
};
static void lua_dict_iter_unref(struct lua_dict_iter *iter)
{
const char *error;
if (dict_iterate_deinit(&iter->iter, &error) < 0) {
e_error(dlua_script_from_state(iter->L)->event,
"Dict iteration failed: %s", error);
}
pool_unref(&iter->pool);
}
DLUA_WRAP_C_DATA(dict_iter, struct lua_dict_iter, lua_dict_iter_unref, NULL);
static int lua_dict_iterate_step(lua_State *L);
static int lua_dict_iterate_step_continue(lua_State *L,
int status ATTR_UNUSED,
lua_KContext ctx ATTR_UNUSED)
{
return lua_dict_iterate_step(L);
}
static void lua_dict_iterate_more(struct lua_dict_iter *iter);
static int lua_dict_iterate_step(lua_State *L)
{
struct lua_dict_iter *iter;
const int *refs;
unsigned int nrefs;
DLUA_REQUIRE_ARGS(L, 2);
iter = xlua_dict_iter_getptr(L, 1, NULL);
iter->yielded = FALSE;
lua_dict_iterate_more(iter);
if (iter->iter != NULL) {
return lua_dict_iterate_step_continue(L,
lua_yieldk(L, 0, 0, lua_dict_iterate_step_continue), 0);
}
refs = array_get(&iter->refs, &nrefs);
i_assert(nrefs % 2 == 0);
if (nrefs == 0) {
if (iter->error_ref != 0) {
lua_rawgeti(L, LUA_REGISTRYINDEX, iter->error_ref);
luaL_unref(L, LUA_REGISTRYINDEX, iter->error_ref);
return lua_error(L);
}
return 0;
}
lua_rawgeti(L, LUA_REGISTRYINDEX, refs[0]);
lua_rawgeti(L, LUA_REGISTRYINDEX, refs[1]);
luaL_unref(L, LUA_REGISTRYINDEX, refs[0]);
luaL_unref(L, LUA_REGISTRYINDEX, refs[1]);
array_delete(&iter->refs, 0, 2);
return 2;
}
static void lua_dict_iterate_more(struct lua_dict_iter *iter)
{
const char *key, *const *values;
lua_State *L = iter->L;
const char *error;
if (iter->iter == NULL)
return;
while (dict_iterate_values(iter->iter, &key, &values)) {
int ref;
lua_pushstring(L, key);
ref = luaL_ref(L, LUA_REGISTRYINDEX);
array_push_back(&iter->refs, &ref);
lua_newtable(L);
for (unsigned int i = 0; values[i] != NULL; i++) {
lua_pushstring(L, values[i]);
lua_seti(L, -2, i + 1);
}
ref = luaL_ref(L, LUA_REGISTRYINDEX);
array_push_back(&iter->refs, &ref);
}
if (dict_iterate_has_more(iter->iter))
return;
if (dict_iterate_deinit(&iter->iter, &error) < 0) {
lua_pushstring(L, error);
iter->error_ref = luaL_ref(L, LUA_REGISTRYINDEX);
}
}
static void lua_dict_iterate_callback(struct lua_dict_iter *iter)
{
if (iter->yielded)
return;
iter->yielded = TRUE;
dlua_pcall_yieldable_resume(iter->L, 1);
}
int lua_dict_iterate(lua_State *L)
{
enum dict_iterate_flags flags;
struct lua_dict_iter *iter;
struct dict *dict;
const char *path, *username = NULL;
pool_t pool;
DLUA_REQUIRE_ARGS_IN(L, 3, 4);
dict = dlua_check_dict(L, 1);
path = luaL_checkstring(L, 2);
flags = luaL_checkinteger(L, 3);
if (lua_gettop(L) >= 4)
username = luaL_checkstring(L, 4);
lua_dict_check_key_prefix(L, path, username);
struct dict_op_settings set = {
.username = username,
};
pool = pool_alloconly_create("lua dict iter", 128);
iter = p_new(pool, struct lua_dict_iter, 1);
iter->pool = pool;
iter->iter = dict_iterate_init(dict, &set, path, flags |
DICT_ITERATE_FLAG_ASYNC);
p_array_init(&iter->refs, iter->pool, 32);
iter->L = L;
dict_iterate_set_async_callback(iter->iter,
lua_dict_iterate_callback, iter);
lua_pushcfunction(L, lua_dict_iterate_step);
xlua_pushdict_iter(L, iter, FALSE);
return 2;
}