#ifndef DLUA_WRAPPER_H
#define DLUA_WRAPPER_H
#define DLUA_WRAP_C_DATA(typename, type, putref, extra_fxns_arg)	\
struct lua_wrapper_##typename {						\
type *ptr;							\
bool ro;							\
};									\
\
static inline type *xlua_##typename##_getptr(lua_State *state, int idx,	\
bool *ro_r)			\
{									\
struct lua_wrapper_##typename *wrapper;				\
\
wrapper = luaL_checkudata(state, idx, #type);			\
\
if (ro_r != NULL)						\
*ro_r = wrapper->ro;					\
\
return wrapper->ptr;						\
}									\
\
static int xlua_wrapper_##typename##_gc(lua_State *state)		\
{									\
putref(xlua_##typename##_getptr(state, -1, NULL));		\
\
return 0;							\
}									\
\
static const luaL_Reg provided_##typename##_fxns[] = {			\
{ "__gc", xlua_wrapper_##typename##_gc },			\
{ NULL, NULL },							\
};									\
\
\
static void xlua_push##typename(lua_State *state, type *ptr, bool ro)	\
{									\
struct lua_wrapper_##typename *wrapper;				\
\
if (ptr == NULL) {						\
lua_pushnil(state);					\
return;							\
}								\
\
wrapper = lua_newuserdata(state, sizeof(struct lua_wrapper_##typename)); \
i_assert(wrapper != NULL);					\
\
wrapper->ptr = (ptr);						\
wrapper->ro = ro;						\
\
\
luaL_getmetatable(state, #type);				\
if (lua_type(state, -1) != LUA_TTABLE) {			\
\
const luaL_Reg *extra_fxns = (extra_fxns_arg);		\
lua_CFunction index;					\
\
lua_pop(state, 1);					\
luaL_newmetatable(state, #type);			\
luaL_setfuncs(state, provided_##typename##_fxns, 0);	\
\
index = NULL;						\
if (extra_fxns != NULL) {				\
unsigned int i;					\
\
luaL_setfuncs(state, extra_fxns, 0);		\
\
for (i = 0; extra_fxns[i].name != NULL; i++) {	\
if (strcmp(extra_fxns[i].name,		\
"__index") == 0) {		\
index = extra_fxns[i].func;	\
break;				\
}					\
}						\
}							\
\
if (index == NULL) {					\
\
lua_pushliteral(state, "__index");		\
lua_pushvalue(state, -2); 	\
lua_settable(state, -3);			\
}							\
}								\
\
\
lua_setmetatable(state, -2);					\
}
#endif