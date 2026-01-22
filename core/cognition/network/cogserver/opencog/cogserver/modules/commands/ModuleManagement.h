#ifndef _COGSERVER_MODULE_MANAGEMENT_H
#define _COGSERVER_MODULE_MANAGEMENT_H
#include <opencog/cogserver/server/CogServer.h>
#include <opencog/cogserver/server/Request.h>
#include <opencog/cogserver/server/RequestClassInfo.h>
#define DEFINE_REQUEST(REQUESTNAME) \
\
class REQUESTNAME : public Request { \
public: \
REQUESTNAME(CogServer& cs) : Request(cs) {}; \
virtual ~REQUESTNAME() {}; \
static const RequestClassInfo& info(void); \
virtual bool execute(void); \
virtual bool isShell(void) { return info().is_shell; } \
};
namespace opencog
{
DEFINE_REQUEST(ConfigModuleRequest)
DEFINE_REQUEST(ListModulesRequest)
DEFINE_REQUEST(LoadModuleRequest)
DEFINE_REQUEST(UnloadModuleRequest)
};
#endif