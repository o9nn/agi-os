#ifndef _OPENCOG_AFRENT_COLLECTION_AGENT_H
#define _OPENCOG_AFRENT_COLLECTION_AGENT_H
#include <chrono>
#include <string>
#include <iostream>
#include <sstream>
#include <opencog/util/RandGen.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/cogserver/server/CogServer.h>
#include <opencog/cogserver/modules/agents/Agent.h>
#include "RentCollectionBaseAgent.h"
using namespace std::chrono;
namespace opencog {
class AFRentCollectionAgent : public RentCollectionBaseAgent {
private:
time_point<high_resolution_clock> last_update;
float update_freq;
public:
virtual const ClassInfo& classinfo() const {
return info();
}
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::AFRentCollectionAgent");
return _ci;
}
AFRentCollectionAgent(CogServer&);
virtual ~AFRentCollectionAgent();
virtual void selectTargets(HandleSeq &targetSetOut);
void collectRent(HandleSeq& targetSet);
};
}
#endif