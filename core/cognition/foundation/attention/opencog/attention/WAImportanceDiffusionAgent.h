#ifndef WAIMPORTANCEDIFFUSIONAGENT_H
#define WAIMPORTANCEDIFFUSIONAGENT_H
#include <opencog/cogserver/server/CogServer.h>
#include "ImportanceDiffusionBase.h"
namespace opencog
{
class AtomSpace;
class WAImportanceDiffusionAgent : public ImportanceDiffusionBase
{
private:
void spreadImportance();
AttentionValue::sti_t calculateDiffusionAmount(Handle);
public:
WAImportanceDiffusionAgent(CogServer&);
~WAImportanceDiffusionAgent();
virtual void run();
virtual const ClassInfo& classinfo() const { return info(); }
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::WAImportanceDiffusionAgent");
return _ci;
}
HandleSeq diffusionSourceVector(void);
};
}
#endif