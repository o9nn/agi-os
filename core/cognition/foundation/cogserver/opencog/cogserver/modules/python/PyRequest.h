#ifndef _OPENCOG_PYREQUEST_H
#define _OPENCOG_PYREQUEST_H
#include <opencog/cython/PyIncludeWrapper.h>
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/cogserver/server/Factory.h>
#include <opencog/cogserver/server/Request.h>
#include <opencog/cogserver/server/RequestClassInfo.h>
namespace opencog
{
class PyRequest : public Request
{
protected:
PyObject* _pyrequest;
std::string _moduleName;
std::string _className;
std::string _last_result;
RequestClassInfo* _cci;
public:
const RequestClassInfo& info() const { return *_cci; }
PyRequest(CogServer&, const std::string& moduleName, const std::string& className,
RequestClassInfo*);
virtual ~PyRequest();
virtual bool execute(void);
virtual bool isShell(void) { return info().is_shell; }
};
}
#endif