#ifndef _OPENCOG_SHUTDOWN_REQUEST_H
#define _OPENCOG_SHUTDOWN_REQUEST_H
#include <opencog/cogserver/server/Request.h>
#include <opencog/cogserver/server/RequestClassInfo.h>
namespace opencog
{
class ShutdownRequest : public Request
{
public:
ShutdownRequest(CogServer&);
virtual ~ShutdownRequest();
static const RequestClassInfo& info();
virtual bool execute(void);
virtual bool isShell(void) {return info().is_shell;}
};
}
#endif