#ifndef _OPENCOG_WARENT_COLLECTION_AGENT_H
#define _OPENCOG_WARENT_COLLECTION_AGENT_H
#include <string>
#include <iostream>
#include <sstream>
#include <opencog/util/RandGen.h>
#include <opencog/cogserver/modules/agents/Agent.h>
#include <opencog/attentionbank/bank/StochasticImportanceDiffusion.h>
#include "RentCollectionBaseAgent.h"
namespace opencog {
class WARentCollectionAgent : public RentCollectionBaseAgent
{
private:
ecan::StochasticDiffusionAmountCalculator _sdac;
unsigned int _sti_rent, _lti_rent;
public:
const ClassInfo& classinfo() const { return info(); }
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::WARentCollectionAgent");
return _ci;
}
WARentCollectionAgent(CogServer&);
void selectTargets(HandleSeq &targetSetOut);
void collectRent(HandleSeq& targetSet);
};
}
#endif