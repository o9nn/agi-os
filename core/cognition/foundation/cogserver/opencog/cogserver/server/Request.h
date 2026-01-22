#ifndef _OPENCOG_REQUEST_H
#define _OPENCOG_REQUEST_H
#include <list>
#include <string>
#include <opencog/cogserver/server/Factory.h>
namespace opencog
{
class CogServer;
class ConsoleSocket;
#define DECLARE_CMD_REQUEST(mod_type,cmd_str,do_cmd,                  \
cmd_sum,cmd_desc,shell_cmd,hidden)        \
\
class do_cmd##Request : public Request {                           \
public:                                                         \
static inline const RequestClassInfo& info(void) {          \
static const RequestClassInfo _cci(cmd_str,             \
cmd_sum,             \
cmd_desc,            \
shell_cmd,           \
hidden);             \
return _cci;                                            \
}                                                           \
do_cmd##Request(CogServer& cs) : Request(cs) {};            \
virtual ~do_cmd##Request() {};                              \
virtual bool execute(void) {                                \
logger().debug("[ %s Request] execute", cmd_str);       \
\
mod_type* mod =                                         \
static_cast<mod_type *>(_cogserver.getModule(       \
#mod_type));                \
\
std::string rs = mod->do_cmd(this, _parameters);        \
send(rs);                                               \
return true;                                            \
}                                                           \
virtual bool isShell(void) {                                \
return info().is_shell;                                 \
}                                                           \
};                                                                \
\
\
Factory<do_cmd##Request, Request> do_cmd##Factory;                \
\
\
std::string do_cmd(Request *, std::list<std::string>);            \
\
\
void do_cmd##_register(void) {                                    \
_cogserver.registerRequest(do_cmd##Request::info().id,        \
& do_cmd##Factory);               \
}                                                                 \
void do_cmd##_unregister(void) {                                  \
_cogserver.unregisterRequest(do_cmd##Request::info().id);     \
}
class Request
{
private:
ConsoleSocket*         _console;
protected:
CogServer&             _cogserver;
std::list<std::string> _parameters;
public:
Request(CogServer&);
virtual ~Request();
virtual bool execute(void) = 0;
virtual bool isShell(void) = 0;
void send(const std::string& msg) const;
void set_console(ConsoleSocket*);
ConsoleSocket *get_console(void) const { return _console; }
virtual void setParameters(const std::list<std::string>&);
virtual void addParameter(const std::string&);
};
}
#endif