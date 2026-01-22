#ifndef _OPENCOG_GROUNDED_PREDICATE_NODE_H
#define _OPENCOG_GROUNDED_PREDICATE_NODE_H
#include <opencog/atoms/execution/GroundedProcedureNode.h>
namespace opencog
{
class AtomSpace;
class Runner;
class GroundedPredicateNode : public GroundedProcedureNode
{
void init();
Runner* _runner;
bool _eager;
public:
GroundedPredicateNode(Type, const std::string);
GroundedPredicateNode(const std::string);
GroundedPredicateNode(const GroundedPredicateNode&) = delete;
GroundedPredicateNode& operator=(const GroundedPredicateNode&) = delete;
virtual ~GroundedPredicateNode();
virtual ValuePtr execute_args(AtomSpace*, const ValuePtr&,
bool silent=false);
static Handle factory(const Handle&);
};
NODE_PTR_DECL(GroundedPredicateNode)
#define createGroundedPredicateNode CREATE_DECL(GroundedPredicateNode)
}
#endif