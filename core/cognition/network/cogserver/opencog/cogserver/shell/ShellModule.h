#ifndef _OPENCOG_SHELL_MODULE_H
#define _OPENCOG_SHELL_MODULE_H
#include <string>
#include <opencog/cogserver/server/Module.h>
#define DEFINE_SHELL_MODULE2(MODNAME,EXTRA)                           \
namespace opencog {                                                   \
\
class MODNAME : public Module {                                       \
private:                                                          \
class shelloutRequest : public Request                        \
{                                                             \
public:                                                   \
static const RequestClassInfo& info(void);            \
shelloutRequest(CogServer& cs) : Request(cs) {};      \
virtual ~shelloutRequest() {};                        \
virtual bool execute(void);                           \
virtual bool isShell(void) { return true; }           \
};                                                            \
Factory<shelloutRequest, Request> shelloutFactory;            \
EXTRA;                                                        \
public:                                                           \
MODNAME(CogServer&);                                          \
virtual ~MODNAME();                                           \
static const char *id(void);                                  \
virtual void init(void);                                      \
virtual bool config(const char *);                            \
};                                                                    \
}
#define DEFINE_SHELL_MODULE(MODNAME) DEFINE_SHELL_MODULE2(MODNAME,)
#endif