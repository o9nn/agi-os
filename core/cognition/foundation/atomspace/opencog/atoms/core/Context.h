#ifndef _OPENCOG_CONTEXT_H
#define _OPENCOG_CONTEXT_H
#include <list>
#include <string>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/core/Variables.h>
namespace opencog
{
struct Context
{
typedef std::list<Variables> VariablesStack;
Context(const Quotation& quotation=Quotation(),
const HandleSet& shadow=HandleSet(),
bool store_scope_variables=true,
const VariablesStack& scope_variables=VariablesStack());
Context(bool store_scope_variables);
Quotation quotation;
HandleSet shadow;
bool store_scope_variables;
VariablesStack scope_variables;
void update(const Handle& h);
bool is_quoted() const;
bool is_unquoted() const;
bool consumable(Type t) const;
bool is_free_variable(const Handle& h) const;
bool operator==(const Context& context) const;
bool operator<(const Context& context) const;
};
bool ohs_content_eq(const HandleSet& lhs, const HandleSet& rhs);
std::string oc_to_string(const Context& c,
const std::string& indent=empty_string);
}
#endif