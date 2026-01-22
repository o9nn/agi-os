#ifndef _OPENCOG_DEFINED_PROCEDURE_NODE_H
#define _OPENCOG_DEFINED_PROCEDURE_NODE_H
#include <opencog/atoms/base/Node.h>
namespace opencog
{
class AtomSpace;
class DefinedProcedureNode : public Node
{
protected:
bool _recursing;
public:
DefinedProcedureNode(Type, const std::string&&);
public:
DefinedProcedureNode(const std::string&&);
DefinedProcedureNode(const DefinedProcedureNode&) = delete;
DefinedProcedureNode& operator=(const DefinedProcedureNode&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
NODE_PTR_DECL(DefinedProcedureNode)
#define createDefinedProcedureNode CREATE_DECL(DefinedProcedureNode)
}
#endif