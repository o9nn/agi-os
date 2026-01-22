#ifndef _OPENCOG_GROUNDED_PROCEDURE_NODE_H
#define _OPENCOG_GROUNDED_PROCEDURE_NODE_H
#include <opencog/atoms/base/Node.h>
namespace opencog
{
class AtomSpace;
class GroundedProcedureNode : public Node
{
public:
GroundedProcedureNode(Type t, const std::string s)
: Node(t, std::move(s)) {}
GroundedProcedureNode(const GroundedProcedureNode&) = delete;
GroundedProcedureNode& operator=(const GroundedProcedureNode&) = delete;
virtual ~GroundedProcedureNode() {};
virtual ValuePtr execute_args(AtomSpace*, const ValuePtr&, bool silent=false) = 0;
};
NODE_PTR_DECL(GroundedProcedureNode)
}
#endif