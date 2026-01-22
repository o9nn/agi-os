#ifndef LUA_SCRIPT_PRIVATE_H
#define LUA_SCRIPT_PRIVATE_H 1
#include "dlua-script.h"
#include "lualib.h"
#include "lauxlib.h"
#include "dlua-compat.h"
#define lua_isstring(L, n) (lua_isstring((L), (n)) == 1)
#define lua_isnumber(L, n) (lua_isnumber((L), (n)) == 1)
#define lua_toboolean(L, n) (lua_toboolean((L), (n)) == 1)
#define lua_pushboolean(L, b) lua_pushboolean((L), (b) ? 1 : 0)
#define lua_isinteger(L, n) (lua_isinteger((L), (n)) == 1)
#define DLUA_TABLE_STRING(n, val) { .name = (n),\
.type = DLUA_TABLE_VALUE_STRING, .v.s = (val) }
#define DLUA_TABLE_STRING_SELF(n) { .name = #n,\
.type = DLUA_TABLE_VALUE_STRING, .v.s = (n) }
#define DLUA_TABLE_INTEGER(n, val) { .name = (n), \
.type = DLUA_TABLE_VALUE_INTEGER, .v.i = (val) }
#define DLUA_TABLE_ENUM(n) { .name = #n, \
.type = DLUA_TABLE_VALUE_INTEGER, .v.i = (n) }
#define DLUA_TABLE_ENUM_NOPREFIX(prefix, n) { .name = #n, \
.type = DLUA_TABLE_VALUE_INTEGER, .v.i = (prefix ## n) }
#define DLUA_TABLE_DOUBLE(n, val) { .name = (n), \
.type = DLUA_TABLE_VALUE_DOUBLE, .v.d = (val) }
#define DLUA_TABLE_BOOLEAN(n, val) { .name = (n), \
.type = DLUA_TABLE_VALUE_BOOLEAN, .v.b = (val) }
#define DLUA_TABLE_NULL(n, s) { .name = (n), \
.type = DLUA_TABLE_VALUE_NULL }
#define DLUA_TABLE_END { .name = NULL }
#define DLUA_REQUIRE_ARGS_IN(L, x, y) \
STMT_START { \
if (lua_gettop(L) < (x) || lua_gettop(L) > (y)) { \
return luaL_error((L), "expected %d to %d arguments, got %d", \
(x), (y), lua_gettop(L)); \
} \
} STMT_END
#define DLUA_REQUIRE_ARGS(L, x) \
STMT_START { \
if (lua_gettop(L) != (x)) { \
return luaL_error((L), "expected %d arguments, got %d", \
(x), lua_gettop(L)); \
} \
} STMT_END
struct dlua_script {
struct dlua_script *prev,*next;
pool_t pool;
lua_State *L;
struct event *event;
const char *filename;
struct istream *in;
ssize_t last_read;
int ref;
bool init:1;
};
enum dlua_table_value_type {
DLUA_TABLE_VALUE_STRING = 0,
DLUA_TABLE_VALUE_INTEGER,
DLUA_TABLE_VALUE_DOUBLE,
DLUA_TABLE_VALUE_BOOLEAN,
DLUA_TABLE_VALUE_NULL
};
struct dlua_table_values {
const char *name;
enum dlua_table_value_type type;
union {
const char *s;
ptrdiff_t i;
double d;
bool b;
} v;
};
typedef void dlua_pcall_yieldable_callback_t(lua_State *L, void *context, int status);
extern struct event_category event_category_lua;
void dlua_register(struct dlua_script *script, const char *name,
lua_CFunction f);
struct dlua_script *dlua_script_from_state(lua_State *L);
void dlua_dovecot_register(struct dlua_script *script);
void dlua_get_dovecot(lua_State *L);
void dlua_dovecot_http_register(struct dlua_script *script);
void dlua_set_members(lua_State *L, const struct dlua_table_values *values, int idx);
void dlua_push_event(lua_State *L, struct event *event);
struct event *dlua_check_event(lua_State *L, int arg);
const char *dlua_push_vfstring(lua_State *L, const char *fmt, va_list argp) ATTR_FORMAT(2, 0);
const char *dlua_push_fstring(lua_State *L, const char *fmt, ...) ATTR_FORMAT(2, 3);
int dluaL_error(lua_State *L, const char *fmt, ...) ATTR_FORMAT(2, 3);
#define luaL_error(...) dluaL_error(__VA_ARGS__)
int dlua_table_get_luainteger_by_str(lua_State *L, int idx, const char *field, lua_Integer *value_r);
int dlua_table_get_int_by_str(lua_State *L, int idx, const char *field, int *value_r);
int dlua_table_get_intmax_by_str(lua_State *L, int idx, const char *field, intmax_t *value_r);
int dlua_table_get_uint_by_str(lua_State *L, int idx, const char *field, unsigned int *value_r);
int dlua_table_get_uintmax_by_str(lua_State *L, int idx, const char *field, uintmax_t *value_r);
int dlua_table_get_number_by_str(lua_State *L, int idx, const char *field, lua_Number *value_r);
int dlua_table_get_bool_by_str(lua_State *L, int idx, const char *field, bool *value_r);
int dlua_table_get_string_by_str(lua_State *L, int idx, const char *field, const char **value_r);
int dlua_table_get_data_by_str(lua_State *L, int idx, const char *field, const unsigned char **value_r, size_t *len_r);
int dlua_table_get_luainteger_by_int(lua_State *L, int idx, lua_Integer field, lua_Integer *value_r);
int dlua_table_get_int_by_int(lua_State *L, int idx, lua_Integer field, int *value_r);
int dlua_table_get_intmax_by_int(lua_State *L, int idx, lua_Integer field, intmax_t *value_r);
int dlua_table_get_uint_by_int(lua_State *L, int idx, lua_Integer field, unsigned int *value_r);
int dlua_table_get_uintmax_by_int(lua_State *L, int idx, lua_Integer field, uintmax_t *value_r);
int dlua_table_get_number_by_int(lua_State *L, int idx, lua_Integer field, lua_Number *value_r);
int dlua_table_get_bool_by_int(lua_State *L, int idx, lua_Integer field, bool *value_r);
int dlua_table_get_string_by_int(lua_State *L, int idx, lua_Integer field, const char **value_r);
int dlua_table_get_data_by_int(lua_State *L, int idx, lua_Integer field, const unsigned char **value_r, size_t *len_r);
int dlua_table_get_luainteger_by_thread(lua_State *L, int idx, lua_Integer *value_r);
int dlua_table_get_int_by_thread(lua_State *L, int idx, int *value_r);
int dlua_table_get_intmax_by_thread(lua_State *L, int idx, intmax_t *value_r);
int dlua_table_get_uint_by_thread(lua_State *L, int idx, unsigned int *value_r);
int dlua_table_get_uintmax_by_thread(lua_State *L, int idx, uintmax_t *value_r);
int dlua_table_get_number_by_thread(lua_State *L, int idx, lua_Number *value_r);
int dlua_table_get_bool_by_thread(lua_State *L, int idx, bool *value_r);
int dlua_table_get_string_by_thread(lua_State *L, int idx, const char **value_r);
int dlua_table_get_data_by_thread(lua_State *L, int idx, const unsigned char **value_r, size_t *len_r);
int dlua_table_get_by_str(lua_State *L, int idx, int type, const char *field);
int dlua_table_get_by_int(lua_State *L, int idx, int type, lua_Integer field);
int dlua_table_get_by_thread(lua_State *L, int idx, int type);
int dlua_strtable_to_kvarray(lua_State *L, int idx, pool_t pool,
const char *const **arr_r, const char **error_r);
int dlua_table_to_array(lua_State *L, int idx, pool_t pool,
const char *const **arr_r, const char **error_r);
int dlua_pcall(lua_State *L, const char *func_name, int nargs, int nresults,
const char **error_r);
void dlua_dump_stack(lua_State *L);
lua_State *dlua_script_new_thread(struct dlua_script *script);
void dlua_script_close_thread(struct dlua_script *script, lua_State **_L);
#ifdef DLUA_WITH_YIELDS
int dlua_pcall_yieldable(lua_State *L, const char *func_name, int nargs,
dlua_pcall_yieldable_callback_t *callback,
void *context, const char **error_r);
#define dlua_pcall_yieldable(L, func_name, nargs, callback, context, error_r) \
dlua_pcall_yieldable(L, TRUE ? func_name : \
CALLBACK_TYPECHECK(callback, void (*)(lua_State *, typeof(context), int)), \
nargs, (dlua_pcall_yieldable_callback_t *)callback, context, error_r)
void dlua_pcall_yieldable_resume(lua_State *L, int nargs);
#endif
void dlua_init_thread_table(struct dlua_script *script);
void dlua_free_thread_table(struct dlua_script *script);
void dlua_tls_set_ptr(lua_State *L, const char *name, void *ptr);
void *dlua_tls_get_ptr(lua_State *L, const char *name);
void dlua_tls_set_int(lua_State *L, const char *name, lua_Integer i);
lua_Integer dlua_tls_get_int(lua_State *L, const char *name);
void dlua_tls_clear(lua_State *L, const char *name);
#endif