#ifndef _OPENCOG_BUILTIN_REQUESTS_MODULE_H
#define _OPENCOG_BUILTIN_REQUESTS_MODULE_H
#include <opencog/cogserver/modules/commands/ShutdownRequest.h>
#include <opencog/cogserver/modules/commands/ModuleManagement.h>
#include <opencog/cogserver/server/CogServer.h>
#include <opencog/cogserver/server/Factory.h>
#include <opencog/cogserver/server/Module.h>
namespace opencog
{
class BuiltinRequestsModule : public Module
{
private:
Factory<ConfigModuleRequest, Request> configmoduleFactory;
Factory<ListModulesRequest, Request> listmodulesFactory;
Factory<LoadModuleRequest, Request> loadmoduleFactory;
Factory<UnloadModuleRequest, Request> unloadmoduleFactory;
Factory<ShutdownRequest, Request> shutdownFactory;
DECLARE_CMD_REQUEST(BuiltinRequestsModule, "exit", do_exit,
"Close the shell connection",
"Usage: exit\n\n"
"Close the shell TCP/IP connection.\n",
false, true)
DECLARE_CMD_REQUEST(BuiltinRequestsModule, "quit", do_quit,
"Close the shell connection",
"Usage: quit\n\n"
"Close the shell TCP/IP connection.\n",
false, false)
DECLARE_CMD_REQUEST(BuiltinRequestsModule, "q", do_q,
"Close the shell connection",
"Usage: q\n\n"
"Close the shell TCP/IP connection.\n",
false, true)
DECLARE_CMD_REQUEST(BuiltinRequestsModule, "", do_ctrld,
"Close the shell connection",
"Usage: ^D\n\n"
"Close the shell TCP/IP connection.\n",
false, true)
static constexpr char iaceof[3] = {(char)255, (char)236, 0};
DECLARE_CMD_REQUEST(BuiltinRequestsModule, iaceof, do_iaceof,
"Close the shell connection",
"Usage: ^D\n\n"
"Close the shell TCP/IP connection.\n",
false, true)
DECLARE_CMD_REQUEST(BuiltinRequestsModule, ".", do_dot,
"Close the shell connection",
"Usage: .\n\n"
"Close the shell TCP/IP connection.\n",
false, true)
DECLARE_CMD_REQUEST(BuiltinRequestsModule, "help", do_help,
"List the available commands; print help for a specific command",
"Usage: help [<command>]\n\n"
"If no command is specified, then print a menu of commands.\n"
"Otherwise, print verbose help for the indicated command.\n",
false, false)
DECLARE_CMD_REQUEST(BuiltinRequestsModule, "h", do_h,
"List the available commands; print help for a specific command",
"Usage: h [<command>]\n\n"
"If no command is specified, then print a menu of commands.\n"
"Otherwise, print verbose help for the indicated command.\n",
false, true)
DECLARE_CMD_REQUEST(BuiltinRequestsModule, "stats", do_stats,
"Print some diagnostic statistics about the server.",
"Usage: stats\n\n" + CogServer::stats_legend(),
false, false)
public:
static const char* id();
BuiltinRequestsModule(CogServer&);
virtual ~BuiltinRequestsModule();
virtual void init();
virtual bool config(const char *) { return false; }
};
}
#endif