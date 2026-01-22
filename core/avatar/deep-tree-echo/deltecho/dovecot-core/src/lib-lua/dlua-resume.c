#include "lib.h"
#include "ioloop.h"
#include "dlua-script-private.h"
#define PCALL_RESUME_STATE "pcall-resume-state"
#define RESUME_TIMEOUT "resume-timeout"
#define RESUME_NARGS "resume-nargs"
struct dlua_pcall_resume_state {
dlua_pcall_yieldable_callback_t *callback;
void *context;
struct timeout *to;
int status;
};
#ifdef DLUA_WITH_YIELDS
static void call_resume_callback(lua_State *L)
{
struct dlua_pcall_resume_state *state = dlua_tls_get_ptr(L, PCALL_RESUME_STATE);
timeout_remove(&state->to);
dlua_tls_clear(L, PCALL_RESUME_STATE);
state->callback(L, state->context, state->status);
i_free(state);
}
static void queue_resume_callback(lua_State *L, int status)
{
struct dlua_pcall_resume_state *state = dlua_tls_get_ptr(L, PCALL_RESUME_STATE);
i_assert(status != LUA_YIELD);
if (status != LUA_OK) {
int ret;
lua_getglobal(L, "debug");
lua_getfield(L, -1, "traceback");
lua_remove(L, -2);
lua_pushvalue(L, -2);
ret = lua_pcall(L, 1, 1, 0);
if (ret != LUA_OK) {
lua_remove(L, -1);
} else {
lua_remove(L, -2);
}
while (lua_gettop(L) > 1)
lua_remove(L, -2);
i_assert(lua_gettop(L) == 1);
}
if (status == LUA_OK)
state->status = lua_gettop(L);
else
state->status = -1;
i_assert(state->to == NULL);
state->to = timeout_add_short(0, call_resume_callback, L);
}
static void dlua_pcall_yieldable_continue(lua_State *L)
{
struct timeout *to;
int nargs, nresults;
int ret;
nargs = dlua_tls_get_int(L, RESUME_NARGS);
to = dlua_tls_get_ptr(L, RESUME_TIMEOUT);
i_assert(to != NULL);
timeout_remove(&to);
dlua_tls_clear(L, RESUME_TIMEOUT);
dlua_tls_clear(L, RESUME_NARGS);
ret = lua_resume(L, L, nargs, &nresults);
if (ret == LUA_YIELD) {
} else if (ret == LUA_OK) {
queue_resume_callback(L, ret);
} else {
queue_resume_callback(L, ret);
}
}
void dlua_pcall_yieldable_resume(lua_State *L, int nargs)
{
struct timeout *to;
to = dlua_tls_get_ptr(L, RESUME_TIMEOUT);
i_assert(to == NULL);
to = timeout_add_short(0, dlua_pcall_yieldable_continue, L);
dlua_tls_set_ptr(L, RESUME_TIMEOUT, to);
dlua_tls_set_int(L, RESUME_NARGS, nargs);
}
#undef dlua_pcall_yieldable
int dlua_pcall_yieldable(lua_State *L, const char *func_name, int nargs,
dlua_pcall_yieldable_callback_t *callback,
void *context, const char **error_r)
{
struct dlua_pcall_resume_state *state;
int ret;
int nresults;
i_assert(lua_status(L) == LUA_OK);
i_assert(lua_gettop(L) == nargs);
lua_getglobal(L, func_name);
if (!lua_isfunction(L, -1)) {
lua_pop(L, nargs + 1);
*error_r = t_strdup_printf("'%s' is not a function", func_name);
return -1;
}
state = i_new(struct dlua_pcall_resume_state, 1);
state->callback = callback;
state->context = context;
dlua_tls_set_ptr(L, PCALL_RESUME_STATE, state);
lua_insert(L, -(nargs + 1));
ret = lua_resume(L, L, nargs, &nresults);
if (ret == LUA_YIELD) {
} else {
queue_resume_callback(L, ret);
}
return 0;
}
#endif