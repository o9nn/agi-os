#ifndef _OPENCOG_TYPED_VARIABLE_LINK_H
#define _OPENCOG_TYPED_VARIABLE_LINK_H
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/TypeChoice.h>
namespace opencog
{
class TypedVariableLink : public Link
{
protected:
void init();
TypeChoicePtr _typech;
ContentHash compute_hash() const;
public:
TypedVariableLink(const HandleSeq&&, Type=TYPED_VARIABLE_LINK);
TypedVariableLink(const Handle& alias, const Handle& body);
TypedVariableLink(const TypedVariableLink&) = delete;
TypedVariableLink& operator=(const TypedVariableLink&) = delete;
Handle get_variable(void) const { return _outgoing.at(0); }
TypeChoicePtr get_typedecl(void) const { return _typech; }
TypeSet get_simple_typeset(void) const
{ return _typech->get_simple_typeset(); }
HandleSet get_deep_typeset(void) const
{ return _typech->get_deep_typeset(); }
GlobInterval get_glob_interval(void) const
{ return _typech->get_glob_interval(); }
bool is_globby(void) const
{ return _typech->is_globby(); }
bool is_lower_bound(size_t n) const
{ return _typech->is_lower_bound(n); }
bool is_upper_bound(size_t n) const
{ return _typech->is_upper_bound(n); }
bool is_type(const ValuePtr& vp) const
{ return _typech->is_type(vp); }
bool is_type(const Handle& h) const
{ return _typech->is_type(h); }
bool is_type(Type t) const
{ return _typech->is_type(t); }
const GlobInterval default_interval(void) const;
bool is_untyped(void) const;
bool is_equal(const TypedVariableLink&) const;
bool operator==(const Atom&) const;
std::string to_string(const std::string& indent) const;
using Atom::to_string;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(TypedVariableLink)
#define createTypedVariableLink CREATE_DECL(TypedVariableLink)
}
#endif