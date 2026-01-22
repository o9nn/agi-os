#ifndef _OPENCOG_EXPERIMENTAL_ATTENTION_MODULE_H
#define _OPENCOG_EXPERIMENTAL_ATTENTION_MODULE_H
#include <opencog/util/concurrent_queue.h>
#include <opencog/cogserver/server/Factory.h>
#include <opencog/cogserver/server/Module.h>
#include <opencog/cogserver/server/CogServer.h>
#include <opencog/cogserver/modules/agents/Scheduler.h>
#include "AFImportanceDiffusionAgent.h"
#include "AFRentCollectionAgent.h"
#include "WAImportanceDiffusionAgent.h"
#include "WARentCollectionAgent.h"
#include "ForgettingAgent.h"
#include "HebbianUpdatingAgent.h"
#include "HebbianCreationAgent.h"
namespace opencog
{
class AttentionModule : public Module
{
private:
Scheduler* _scheduler;
Factory<AFImportanceDiffusionAgent, Agent> afImportanceFactory;
Factory<WAImportanceDiffusionAgent, Agent> waImportanceFactory;
Factory<AFRentCollectionAgent, Agent> afRentFactory;
Factory<WARentCollectionAgent, Agent> waRentFactory;
Factory<ForgettingAgent, Agent> forgettingFactory;
Factory<HebbianUpdatingAgent, Agent> hebbianUpdatingFactory;
Factory<HebbianCreationAgent, Agent> hebbianCreationFactory;
AgentPtr _forgetting_agentptr;
AgentPtr _minmaxstiupdating_agentptr;
AgentPtr _hebbianupdating_agentptr;
AgentPtr _hebbiancreation_agentptr;
AgentPtr _afImportanceAgentPtr;
AgentPtr _waImportanceAgentPtr;
AgentPtr _waRentAgentPtr;
AgentPtr _afRentAgentPtr;
int addAFConnection;
void addAFSignal(const Handle& h, const AttentionValuePtr& av_old,
const AttentionValuePtr& av_new);
void addAFSignalHandler(const Handle& h, const AttentionValuePtr& av_old,
const AttentionValuePtr& av_new);
public:
DECLARE_CMD_REQUEST(AttentionModule, "start-ecan", do_start_ecan,
"Starts  ECAN agents. use agents-active command to view a list of agents started.\n",
"Usage: start-ecan\n", false, true)
DECLARE_CMD_REQUEST(AttentionModule, "stop-ecan", do_stop_ecan,
"Stops all active  ECAN agents\n",
"Usage: stop-ecan\n", false, true)
DECLARE_CMD_REQUEST(AttentionModule, "list-ecan-param", do_list_ecan_param,
"Lists all ecan parameters and their values.\n",
"Usage: list-ecan-params\n", false, true)
DECLARE_CMD_REQUEST(AttentionModule, "set-ecan-param", do_set_ecan_param,
"Sets the value of an ecan parameter\n",
"Usage: set-ecan-param <param-name> <param-value> \n", false, true)
static inline const char* id();
static concurrent_queue<Handle> newAtomsInAV;
AttentionModule(CogServer&);
virtual ~AttentionModule();
virtual void init();
};
}
#endif