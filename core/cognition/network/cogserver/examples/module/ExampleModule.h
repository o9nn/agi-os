#ifndef _OPENCOG_EXAMPLE_MODULE_H
#define _OPENCOG_EXAMPLE_MODULE_H
#include <opencog/cogserver/server/Module.h>
namespace opencog
{
class CogServer;
class ExampleModule : public Module
{
private:
public:
ExampleModule(CogServer&);
virtual ~ExampleModule();
virtual void init();
virtual const char* id();
virtual bool config(const char*) { return false; }
};
}
#endif