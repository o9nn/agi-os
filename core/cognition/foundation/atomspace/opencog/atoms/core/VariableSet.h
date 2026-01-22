#ifndef _OPENCOG_VARIABLE_SET_H
#define _OPENCOG_VARIABLE_SET_H
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/core/UnorderedLink.h>
namespace opencog
{
class VariableSet : public UnorderedLink
{
protected:
Variables _variables;
void throw_if_not_variable_set(Type t) const;
public:
VariableSet(const HandleSeq&& vardecls, Type=VARIABLE_SET);
VariableSet(const Handle& hvardecls);
VariableSet(const VariableSet&) = delete;
VariableSet& operator=(const VariableSet&) = delete;
const Variables& get_variables(void) const { return _variables; }
static Handle factory(const Handle&);
};
LINK_PTR_DECL(VariableSet)
#define createVariableSet CREATE_DECL(VariableSet)
std::string oc_to_string(const VariableSetPtr& vsp,
const std::string& indent=empty_string);
}
#endif