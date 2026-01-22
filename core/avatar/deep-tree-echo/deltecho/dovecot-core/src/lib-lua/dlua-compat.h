#ifndef DLUA_COMPAT_H
#define DLUA_COMPAT_H
#if !defined(LUA_OK)
#  define LUA_OK 0
#endif
#if LUA_VERSION_NUM <= 502
#  define luaL_newmetatable(L, tn) \
((luaL_newmetatable(L, tn) != 0) ? \
(lua_pushstring((L), (tn)), lua_setfield((L), -2, "__name"), 1) : \
0)
#endif
#if LUA_VERSION_NUM <= 501
#  define lua_load(L, r, s, fn, m) lua_load(L, r, s, fn)
#  define luaL_newlibtable(L, l) (lua_createtable(L, 0, sizeof(l)/sizeof(*(l))-1))
#  define luaL_newlib(L, l) (luaL_newlibtable(L, l), luaL_register(L, NULL, l))
#endif
#ifndef HAVE_LUAL_SETFUNCS
void luaL_setfuncs (lua_State *L, const luaL_Reg *l, int nup);
#endif
#ifndef HAVE_LUAL_SETMETATABLE
void luaL_setmetatable (lua_State *L, const char *tname);
#endif
#ifndef HAVE_LUA_ISINTEGER
int lua_isinteger(lua_State *L, int idx);
#endif
#ifndef HAVE_LUA_SETI
void lua_seti(lua_State *L, int index, lua_Integer n);
#endif
#ifndef HAVE_LUA_TOINTEGERX
lua_Integer lua_tointegerx(lua_State *L, int idx, int *isnum_r);
#endif
#if LUA_VERSION_NUM > 501 && LUA_VERSION_NUM < 504
#  define lua_resume(L, from, nargs, nresults) \
lua_resume_compat(L, from, nargs, nresults)
int lua_resume_compat(lua_State *L, lua_State *from, int nargs, int *nresults);
#endif
#endif