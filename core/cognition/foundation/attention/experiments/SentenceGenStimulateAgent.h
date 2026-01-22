#include <opencog/guile/SchemeEval.h>
#include <opencog/cogserver/server/CogServer.h>
#ifndef _SENTENCEGENAGENT_H_
#define _SENTENCEGENAGENT_H_
namespace opencog {
namespace ECANExperiment {
class SentenceGenStimulateAgent: public Agent {
private:
UnorderedHandleSet _hword_wordInstance_nodes;
SchemeEval* _scm_eval;
int startcount;
time_t stime;
public:
virtual ~ SentenceGenStimulateAgent();
SentenceGenStimulateAgent(CogServer& cs);
virtual const ClassInfo& classinfo() const;
static const ClassInfo& info();
virtual void run();
void generate_stimulate_sentence();
};
typedef std::shared_ptr<SentenceGenStimulateAgent> SentenceGenStimulateAgentPtr;
}
}
#endif