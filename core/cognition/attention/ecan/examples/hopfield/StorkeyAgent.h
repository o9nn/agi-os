#ifndef _OPENCOG_STORKEY_AGENT_H
#define _OPENCOG_STORKEY_AGENT_H
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/truthvalue/AttentionValue.h>
#include <opencog/cogserver/server/Agent.h>
#include <opencog/util/Logger.h>
#include "Pattern.h"
namespace opencog
{
class CogServer;
class StorkeyAgent : public Agent
{
public:
typedef std::vector< std::vector<float> > w_t;
private:
bool verbose;
void setLogger(Logger* l);
Logger *log;
void setCurrentWeights(w_t& w);
bool checkWeightSymmetry(w_t& w);
void printWeights(w_t& w);
public:
float h(int i, w_t& w);
float h(int i, int j, w_t& w);
w_t getCurrentWeights();
virtual const ClassInfo& classinfo() const { return info(); }
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::StorkeyAgent");
return _ci;
}
StorkeyAgent(CogServer&);
virtual ~StorkeyAgent();
virtual void run();
Logger* getLogger();
bool convertLinks;
AttentionValue::lti_t conversionThreshold;
void setPattern(Pattern _epsilon);
void storkeyUpdate();
};
typedef std::shared_ptr<StorkeyAgent> StorkeyAgentPtr;
}
#endif