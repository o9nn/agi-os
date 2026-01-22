#ifndef _OPENCOG_PROXY_H
#define _OPENCOG_PROXY_H
#include <opencog/cogserver/server/Module.h>
#include <opencog/persist/sexpr/SexprEval.h>
namespace opencog {
class Proxy : public Module
{
public:
Proxy(CogServer& cs) : Module(cs) {};
virtual ~Proxy() {};
virtual void setup(SexprEval*) = 0;
};
}
#endif