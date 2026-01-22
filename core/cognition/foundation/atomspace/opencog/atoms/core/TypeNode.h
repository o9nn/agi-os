#ifndef _OPENCOG_TYPE_NODE_H
#define _OPENCOG_TYPE_NODE_H
#include <opencog/util/oc_assert.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Node.h>
namespace opencog
{
class TypeNode : public Node
{
protected:
Type _kind;
public:
TypeNode(Type t, const std::string&& s)
: Node(t, std::move(s)),
_kind(nameserver().getType(_name))
{
if (TYPE_NODE == t)
{
if (NOTYPE == _kind)
throw InvalidParamException(TRACE_INFO,
"Not a valid typename: '%s'", _name.c_str());
_name = nameserver().getTypeName(_kind);
}
}
public:
TypeNode(const std::string&& s)
: Node(TYPE_NODE, std::move(s)),
_kind(nameserver().getType(_name))
{
if (NOTYPE == _kind)
throw InvalidParamException(TRACE_INFO,
"Not a valid typename: '%s'", s.c_str());
_name = nameserver().getTypeName(_kind);
}
TypeNode(Type t)
: Node(TYPE_NODE, std::string(nameserver().getTypeName(t))),
_kind(t)
{}
TypeNode(TypeNode&) = delete;
TypeNode& operator=(const TypeNode&) = delete;
static void validate(const std::string& str)
{
Type t = nameserver().getType(str);
if (NOTYPE == t)
throw InvalidParamException(TRACE_INFO,
"Not a valid typename: '%s'", str.c_str());
}
Type get_kind(void) const { return _kind; }
static Handle factory(const Handle&);
};
NODE_PTR_DECL(TypeNode)
#define createTypeNode CREATE_DECL(TypeNode)
}
#endif