#ifndef RENTCOLLECTIONBASE_H
#define RENTCOLLECTIONBASE_H
#include <string>
#include <iostream>
#include <sstream>
#include <opencog/util/RandGen.h>
#include <opencog/cogserver/modules/agents/Agent.h>
#include <opencog/attentionbank/bank/AttentionBank.h>
#include "AttentionParamQuery.h"
namespace opencog
{
class RentCollectionBaseAgent : public Agent
{
protected:
AttentionBank* _bank;
AttentionParamQuery _atq;
AttentionValue::sti_t STIAtomRent;
AttentionValue::lti_t LTIAtomRent;
AttentionValue::sti_t targetSTI;
AttentionValue::sti_t targetLTI;
AttentionValue::sti_t stiFundsBuffer;
AttentionValue::lti_t ltiFundsBuffer;
void load_params(void);
public:
RentCollectionBaseAgent(CogServer& cs);
double calculate_STI_Rent();
double calculate_LTI_Rent();
virtual void selectTargets(HandleSeq &targetSetOut) = 0;
virtual void collectRent(HandleSeq& targetSet) = 0;
void run();
};
}
#endif