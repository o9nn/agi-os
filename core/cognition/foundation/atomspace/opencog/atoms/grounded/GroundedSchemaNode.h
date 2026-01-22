#ifndef _OPENCOG_GROUNDED_SCHEMA_NODE_H
#define _OPENCOG_GROUNDED_SCHEMA_NODE_H
#include <opencog/atoms/execution/GroundedProcedureNode.h>
namespace opencog
{
class AtomSpace;
class Runner;
class GroundedSchemaNode : public GroundedProcedureNode
{
Runner* _runner;
bool _eager;
void init();
public:
GroundedSchemaNode(Type, const std::string);
GroundedSchemaNode(const std::string);
GroundedSchemaNode(const GroundedSchemaNode&) = delete;
GroundedSchemaNode& operator=(const GroundedSchemaNode&) = delete;
virtual ~GroundedSchemaNode();
virtual ValuePtr execute_args(AtomSpace*, const ValuePtr&,
bool silent=false);
static Handle factory(const Handle&);
};
NODE_PTR_DECL(GroundedSchemaNode)
#define createGroundedSchemaNode CREATE_DECL(GroundedSchemaNode)
}
#endif