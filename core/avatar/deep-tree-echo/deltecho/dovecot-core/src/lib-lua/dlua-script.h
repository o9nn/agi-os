#ifndef LUA_SCRIPT_H
#define LUA_SCRIPT_H 1
struct dlua_script;
int dlua_script_create_string(const char *str, struct dlua_script **script_r,
struct event *event_parent, const char **error_r);
int dlua_script_create_file(const char *file, struct dlua_script **script_r,
struct event *event_parent, const char **error_r);
int dlua_script_create_stream(struct istream *is, struct dlua_script **script_r,
struct event *event_parent, const char **error_r);
int dlua_script_init(struct dlua_script *script, const char **error_r);
void dlua_script_ref(struct dlua_script *script);
void dlua_script_unref(struct dlua_script **_script);
bool dlua_script_has_function(struct dlua_script *script, const char *fn);
#endif