#ifndef _OPENCOG_IMPRINT_AGENT_H
#define _OPENCOG_IMPRINT_AGENT_H
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/truthvalue/AttentionValue.h>
#include <opencog/cogserver/server/Agent.h>
#include <opencog/util/Logger.h>
#include "Pattern.h"
namespace opencog
{
class CogServer;
class ImprintAgent : public Agent
{
public:
typedef std::vector< std::vector<float> > w_t;
private:
bool verbose;
void setLogger(Logger* l);
Logger *log;
Pattern epsilon;
public:
virtual const ClassInfo& classinfo() const { return info(); }
static const ClassInfo& info() {
static const ClassInfo _ci("opencog::ImprintAgent");
return _ci;
}
ImprintAgent(CogServer&);
virtual ~ImprintAgent();
virtual void run();
Logger* getLogger();
void setPattern(Pattern _epsilon);
};
typedef std::shared_ptr<ImprintAgent> ImprintAgentPtr;
}
#endif