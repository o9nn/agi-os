#ifndef AFIMPORTANCEDIFFUSIONAGENT_H
#define AFIMPORTANCEDIFFUSIONAGENT_H
#include "ImportanceDiffusionBase.h"
class ImportanceDiffusionUTest;
namespace opencog
{
class AFImportanceDiffusionAgent : public virtual ImportanceDiffusionBase
{
private:
friend class ::ImportanceDiffusionUTest;
void spreadImportance();
AttentionValue::sti_t calculateDiffusionAmount(Handle);
public:
AFImportanceDiffusionAgent(CogServer&);
virtual void run();
virtual const ClassInfo& classinfo() const { return info(); }
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::AFImportanceDiffusionAgent");
return _ci;
}
};
}
#endif