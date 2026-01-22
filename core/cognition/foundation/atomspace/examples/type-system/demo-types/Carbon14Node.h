#ifndef _OPENCOG_CARBON14_NODE_H
#define _OPENCOG_CARBON14_NODE_H
#include <string>
#include <opencog/atoms/base/Node.h>
#include "examples/type-system/demo-types/chem_types.h"
namespace opencog
{
class Carbon14Node : public Node
{
protected:
std::string kind;
public:
Carbon14Node(const std::string&& s)
: Node(CARBON14_NODE, std::move(s))
{
kind = "unknown";
}
Carbon14Node(Type t, const std::string&& s)
: Node(t, std::move(s))
{
kind = "atomospheric";
}
Carbon14Node(Carbon14Node&) = delete;
Carbon14Node& operator=(const Carbon14Node&) = delete;
virtual ValuePtr execute(AtomSpace*, bool silent=true);
virtual bool is_executable() const { return true; }
static Handle factory(const Handle&);
};
NODE_PTR_DECL(Carbon14Node)
#define createCarbon14Node CREATE_DECL(Carbon14Node)
}
#endif