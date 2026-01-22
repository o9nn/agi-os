#ifndef _OPENCOG_FORGETTING_AGENT_H
#define _OPENCOG_FORGETTING_AGENT_H
#include <string>
#include <math.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/attentionbank/bank/AttentionBank.h>
#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/cogserver/modules/agents/Agent.h>
namespace opencog
{
class ForgettingAgent : public Agent
{
private:
AttentionBank* _bank;
public:
virtual const ClassInfo& classinfo() const { return info(); }
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::ForgettingAgent");
return _ci;
}
AttentionValue::lti_t forgetThreshold;
int maxSize;
int accDivSize;
ForgettingAgent(CogServer&);
virtual ~ForgettingAgent();
virtual void run();
void forget();
};
typedef std::shared_ptr<ForgettingAgent> ForgettingAgentPtr;
struct ForgettingLTIThenTVAscendingSort
{
AttentionBank* _bank;
ForgettingLTIThenTVAscendingSort(AtomSpace* a) :
_bank(&attentionbank(a)) {};
bool operator()(const Handle& h1, const Handle& h2)
{
AttentionValue::lti_t lti1, lti2;
lti1 = get_lti(h1);
lti2 = get_lti(h2);
if (lti1 != lti2) return lti1 < lti2;
else {
double tv1, tv2;
tv1 = fabs(h1->getTruthValue()->get_mean());
tv2 = fabs(h2->getTruthValue()->get_mean());
return tv1 < tv2;
}
}
};
}
#endif