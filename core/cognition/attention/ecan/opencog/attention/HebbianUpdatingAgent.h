#ifndef _OPENCOG_HEBBIAN_UPDATING_AGENT_H
#define _OPENCOG_HEBBIAN_UPDATING_AGENT_H
#include <string>
#include <iostream>
#include <sstream>
#include <opencog/util/RandGen.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/cogserver/modules/agents/Agent.h>
namespace opencog
{
class AttentionBank;
class HebbianUpdatingAgent : public Agent
{
private:
AttentionBank* _bank;
double targetConjunction(HandleSeq handles);
void updateHebbianLinks(Handle source);
public:
virtual const ClassInfo& classinfo() const { return info(); }
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::HebbianUpdatingAgent");
return _ci;
}
HebbianUpdatingAgent(CogServer&);
virtual void run();
};
typedef std::shared_ptr<HebbianUpdatingAgent> HebbianUpdatingAgentPtr;
}
#endif