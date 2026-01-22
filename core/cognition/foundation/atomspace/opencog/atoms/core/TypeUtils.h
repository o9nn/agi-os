#ifndef _OPENCOG_TYPE_UTILS_H
#define _OPENCOG_TYPE_UTILS_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/VariableSet.h>
namespace opencog
{
bool value_is_type(const Handle& type_spec, const ValuePtr& value);
bool type_match(const Handle&, const ValuePtr&);
ValuePtr type_compose(const Handle&, const ValuePtr&);
Handle filter_vardecl(const Handle& vardecl, const Handle& body);
Handle filter_vardecl(const Handle& vardecl, const HandleSeq& hs);
bool is_well_typed(Type t);
bool is_well_typed(const TypeSet& ts);
}
#endif