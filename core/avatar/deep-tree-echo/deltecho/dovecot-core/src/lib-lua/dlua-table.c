#include "lib.h"
#include "array.h"
#include "dlua-script-private.h"
static inline int adj(int idx, int delta)
{
if ((idx == LUA_REGISTRYINDEX) || (idx > 0))
return idx;
else
return idx - delta;
}
static int dlua_table_get(lua_State *L, int idx, int type)
{
if (!lua_istable(L, idx)) {
lua_pop(L, 1);
return -1;
}
lua_gettable(L, idx);
if (lua_isnoneornil(L, -1)) {
lua_pop(L, 1);
return 0;
}
if (lua_type(L, -1) != type) {
lua_pop(L, 1);
return -1;
}
return 1;
}
int dlua_table_get_by_str(lua_State *L, int idx, int type, const char *field)
{
lua_pushstring(L, field);
return dlua_table_get(L, adj(idx, 1), type);
}
int dlua_table_get_by_int(lua_State *L, int idx, int type, lua_Integer field)
{
lua_pushinteger(L, field);
return dlua_table_get(L, adj(idx, 1), type);
}
int dlua_table_get_by_thread(lua_State *L, int idx, int type)
{
lua_pushthread(L);
return dlua_table_get(L, adj(idx, 1), type);
}
#define GET_INTTYPE(fxn, ctype, minval, maxval, unsigned_check) \
int fxn##_by_str(lua_State *L, int idx, const char *field, \
ctype *value_r) \
{ \
lua_Integer tmp; \
int ret; \
\
ret = dlua_table_get_luainteger_by_str(L, idx, field, &tmp); \
if (ret < 1) \
return ret; \
\
if (unsigned_check) { \
if ((tmp < 0) || (((uintmax_t) tmp) > (maxval))) \
return -1; \
} else { \
if ((tmp < (minval)) || (tmp > (intmax_t) (maxval))) \
return -1; \
} \
\
*value_r = (ctype) tmp; \
\
return 1; \
} \
int fxn##_by_int(lua_State *L, int idx, lua_Integer field, \
ctype *value_r) \
{ \
lua_Integer tmp; \
int ret; \
\
ret = dlua_table_get_luainteger_by_int(L, idx, field, &tmp); \
if (ret < 1) \
return ret; \
\
if (unsigned_check) { \
if ((tmp < 0) || (((uintmax_t) tmp) > (maxval))) \
return -1; \
} else { \
if ((tmp < (minval)) || (tmp > (intmax_t) (maxval))) \
return -1; \
} \
\
*value_r = (ctype) tmp; \
\
return 1; \
} \
int fxn##_by_thread(lua_State *L, int idx, ctype *value_r) \
{ \
lua_Integer tmp; \
int ret; \
\
ret = dlua_table_get_luainteger_by_thread(L, idx, &tmp); \
if (ret < 1) \
return ret; \
\
if (unsigned_check) { \
if ((tmp < 0) || (((uintmax_t) tmp) > (maxval))) \
return -1; \
} else { \
if ((tmp < (minval)) || (tmp > (intmax_t) (maxval))) \
return -1; \
} \
\
*value_r = (ctype) tmp; \
\
return 1; \
}
#define GET_DATAPTR(fxn) \
int fxn##_by_str(lua_State *L, int idx, const char *field, \
const unsigned char **value_r, size_t *len_r) \
{ \
int ret; \
\
ret = dlua_table_get_by_str(L, idx, LUA_TSTRING, field); \
if (ret < 1) \
return ret; \
\
*value_r = (const unsigned char *) lua_tolstring(L, -1, len_r); \
lua_pop(L, 1); \
\
return 1; \
} \
int fxn##_by_int(lua_State *L, int idx, lua_Integer field, \
const unsigned char **value_r, size_t *len_r) \
{ \
int ret; \
\
ret = dlua_table_get_by_int(L, idx, LUA_TSTRING, field); \
if (ret < 1) \
return ret; \
\
*value_r = (const unsigned char *) lua_tolstring(L, -1, len_r); \
lua_pop(L, 1); \
\
return 1; \
} \
int fxn##_by_thread(lua_State *L, int idx, \
const unsigned char **value_r, size_t *len_r) \
{ \
int ret; \
\
ret = dlua_table_get_by_thread(L, idx, LUA_TSTRING); \
if (ret < 1) \
return ret; \
\
*value_r = (const unsigned char *) lua_tolstring(L, -1, len_r); \
lua_pop(L, 1); \
\
return 1; \
}
#define GET_GENERIC(fxn, ctype, ltype, cvt) \
int fxn##_by_str(lua_State *L, int idx, const char *field, ctype *value_r)\
{ \
int ret; \
\
ret = dlua_table_get_by_str(L, idx, (ltype), field); \
if (ret < 1) \
return ret; \
\
*value_r = cvt(L, -1); \
lua_pop(L, 1); \
\
return 1; \
} \
int fxn##_by_int(lua_State *L, int idx, lua_Integer field, ctype *value_r)\
{ \
int ret; \
\
ret = dlua_table_get_by_int(L, idx, (ltype), field); \
if (ret < 1) \
return ret; \
\
*value_r = cvt(L, -1); \
lua_pop(L, 1); \
\
return 1; \
} \
int fxn##_by_thread(lua_State *L, int idx, ctype *value_r) \
{ \
int ret; \
\
ret = dlua_table_get_by_thread(L, idx, (ltype)); \
if (ret < 1) \
return ret; \
\
*value_r = cvt(L, -1); \
lua_pop(L, 1); \
\
return 1; \
}
GET_INTTYPE(dlua_table_get_int, int, INT_MIN, INT_MAX, FALSE);
GET_INTTYPE(dlua_table_get_intmax, intmax_t, INTMAX_MIN, INTMAX_MAX, FALSE);
GET_INTTYPE(dlua_table_get_uint, unsigned int, 0, UINT_MAX, TRUE);
GET_INTTYPE(dlua_table_get_uintmax, uintmax_t, 0, UINTMAX_MAX, TRUE);
int dlua_table_get_luainteger_by_str(lua_State *L, int idx, const char *field,
lua_Integer *value_r)
{
int isnum;
int ret;
ret = dlua_table_get_by_str(L, idx, LUA_TNUMBER, field);
if (ret < 1)
return ret;
*value_r = lua_tointegerx(L, -1, &isnum);
lua_pop(L, 1);
return (isnum == 1) ? 1 : -1;
}
int dlua_table_get_luainteger_by_int(lua_State *L, int idx, lua_Integer field,
lua_Integer *value_r)
{
int isnum;
int ret;
ret = dlua_table_get_by_int(L, idx, LUA_TNUMBER, field);
if (ret < 1)
return ret;
*value_r = lua_tointegerx(L, -1, &isnum);
lua_pop(L, 1);
return (isnum == 1) ? 1 : -1;
}
int dlua_table_get_luainteger_by_thread(lua_State *L, int idx,
lua_Integer *value_r)
{
int isnum;
int ret;
ret = dlua_table_get_by_thread(L, idx, LUA_TNUMBER);
if (ret < 1)
return ret;
*value_r = lua_tointegerx(L, -1, &isnum);
lua_pop(L, 1);
return (isnum == 1) ? 1 : -1;
}
GET_GENERIC(dlua_table_get_number, lua_Number, LUA_TNUMBER, lua_tonumber);
GET_GENERIC(dlua_table_get_bool, bool, LUA_TBOOLEAN, lua_toboolean);
GET_GENERIC(dlua_table_get_string, const char *, LUA_TSTRING, lua_tostring);
GET_DATAPTR(dlua_table_get_data);
int dlua_strtable_to_kvarray(lua_State *L, int idx, pool_t pool,
const char *const **arr_r, const char **error_r)
{
ARRAY_TYPE(const_string) arr;
p_array_init(&arr, pool, 8);
lua_pushnil(L);
if (idx < 0)
idx--;
while (lua_next(L, idx) != 0) {
lua_pushvalue(L, -2);
const char *key = p_strdup(pool, lua_tostring(L, -1));
i_assert(key != NULL);
const char *value = p_strdup(pool, lua_tostring(L, -2));
if (value == NULL) {
*error_r = t_strdup_printf(
"Table key '%s' value has invalid type: %s",
key, lua_typename(L, lua_type(L, -2)));
lua_pop(L, 3);
return -1;
}
array_push_back(&arr, &key);
array_push_back(&arr, &value);
lua_pop(L, 2);
}
array_append_zero(&arr);
*arr_r = array_front(&arr);
return 0;
}
int dlua_table_to_array(lua_State *L, int idx, pool_t pool,
const char *const **arr_r, const char **error_r)
{
ARRAY_TYPE(const_string) arr;
p_array_init(&arr, pool, 8);
lua_pushnil(L);
if (idx < 0)
idx--;
while (lua_next(L, idx) != 0) {
const char *value = p_strdup(pool, lua_tostring(L, -1));
if (value == NULL) {
*error_r = t_strdup_printf(
"Table value has invalid type: %s",
lua_typename(L, lua_type(L, -1)));
lua_pop(L, 2);
return -1;
}
array_push_back(&arr, &value);
lua_pop(L, 1);
}
array_append_zero(&arr);
*arr_r = array_front(&arr);
return 0;
}