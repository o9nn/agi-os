#ifndef _OPENCOG_VARIABLE_LIST_H
#define _OPENCOG_VARIABLE_LIST_H
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/Variables.h>
namespace opencog
{
class VariableList : public Link
{
protected:
Variables _variables;
VariableList(Type, const HandleSeq&);
void throw_if_not_variable_list(Type t) const;
public:
VariableList(const HandleSeq&& vardecls, Type=VARIABLE_LIST);
VariableList(const Handle& hvardecls);
VariableList(const VariableList&) = delete;
VariableList& operator=(const VariableList&) = delete;
const Variables& get_variables(void) const { return _variables; }
bool is_type(const Handle& h) const { return _variables.is_type(h); }
bool is_type(const Handle& var, const Handle& val) const
{ return _variables.is_type(var, val); }
bool is_type(const HandleSeq& hseq) const { return _variables.is_type(hseq); }
Handle substitute(const Handle& tree, const HandleSeq& args) const
{ return _variables.substitute(tree, args); }
static Handle factory(const Handle&);
};
LINK_PTR_DECL(VariableList)
#define createVariableList CREATE_DECL(VariableList)
std::string oc_to_string(const VariableListPtr& vlp,
const std::string& indent=empty_string);
}
#endif