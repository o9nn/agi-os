#ifndef _OPENCOG_HEBBIAN_CREATION_AGENT_H
#define _OPENCOG_HEBBIAN_CREATION_AGENT_H
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/attentionbank/bank/AttentionBank.h>
#include <opencog/cogserver/modules/agents/Agent.h>
#include "AttentionParamQuery.h"
namespace opencog
{
class HebbianCreationAgent : public Agent
{
private:
AttentionParamQuery _atq;
protected:
AttentionBank* _bank;
void addHebbian(Handle atom, Handle source);
double targetConjunction(Handle handle1, Handle handle2);
unsigned int maxLinkNum;
int localToFarLinks;
public:
virtual const ClassInfo& classinfo() const { return info(); }
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::HebbianCreationAgent");
return _ci;
}
HebbianCreationAgent(CogServer&);
virtual void run();
};
typedef std::shared_ptr<HebbianCreationAgent> HebbianCreationAgentPtr;
}
#endif