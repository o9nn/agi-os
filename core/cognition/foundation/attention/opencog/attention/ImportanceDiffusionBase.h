#ifndef IMPORTANCEDIFFUSIONBASE_H
#define IMPORTANCEDIFFUSIONBASE_H
#include <string>
#include <stack>
#include <math.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/cogserver/modules/agents/Agent.h>
#include <opencog/util/Logger.h>
#include <opencog/util/RandGen.h>
#include "AttentionParamQuery.h"
class ImportanceDiffusionUTest;
namespace opencog
{
class AttentionBank;
class ImportanceDiffusionBase : public Agent
{
protected:
friend class ::ImportanceDiffusionUTest;
AttentionBank* _bank;
double maxSpreadPercentage;
double hebbianMaxAllocationPercentage;
bool spreadHebbianOnly;
AttentionParamQuery _atq;
typedef struct DiffusionEventType
{
Handle source;
Handle target;
AttentionValue::sti_t amount;
} DiffusionEventType;
std::stack<DiffusionEventType> diffusionStack;
void processDiffusionStack();
HandleSeq diffusionSourceVector(void);
HandleSeq incidentAtoms(Handle);
HandleSeq hebbianAdjacentAtoms(Handle);
std::map<Handle, double> probabilityVector(HandleSeq);
std::map<Handle, double> probabilityVectorIncident(HandleSeq);
std::map<Handle, double> probabilityVectorHebbianAdjacent(Handle, HandleSeq);
std::map<Handle, double> combineIncidentAdjacentVectors(
std::map<Handle, double>, std::map<Handle, double>);
double calculateHebbianDiffusionPercentage(Handle);
double calculateIncidentDiffusionPercentage(Handle);
void tradeSTI(DiffusionEventType);
void diffuseAtom(Handle);
virtual void spreadImportance() = 0;
virtual AttentionValue::sti_t calculateDiffusionAmount(Handle) = 0;
public:
ImportanceDiffusionBase(CogServer&);
virtual ~ImportanceDiffusionBase();
};
}
#endif