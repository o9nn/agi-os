#ifndef _OPENCOG_VARIABLES_H
#define _OPENCOG_VARIABLES_H
#include <map>
#include <set>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/FreeVariables.h>
#include <opencog/atoms/core/TypedVariableLink.h>
namespace opencog
{
typedef std::map<Handle, TypedVariableLinkPtr> VariableTypeMap;
struct Variables : public FreeVariables
{
Variables(bool ordered=false);
Variables(const Handle& vardecl, bool ordered=false);
Variables(const HandleSeq& vardecls, bool ordered=false);
bool _ordered;
VariableTypeMap _typemap;
void validate_vardecl(const Handle&);
void validate_vardecl(const HandleSeq&);
void unpack_vartype(const Handle&);
bool is_well_typed() const;
bool is_equal(const Variables& other) const;
bool is_equal(const Variables& other, size_t index) const;
bool operator==(const Variables& other) const;
bool operator<(const Variables& other) const;
bool is_alpha_convertible(const Handle& var,
const Handle& othervar,
const Variables& other,
bool check_type=false) const;
bool is_type(const Handle&) const;
bool is_type(Type) const;
bool is_type(const Handle& var, const Handle& val) const;
bool is_type(const Handle& var, const ValuePtr& val) const;
bool is_type(const HandleSeq& hseq) const;
bool is_lower_bound(const Handle& glob, size_t n) const;
bool is_upper_bound(const Handle& glob, size_t n) const;
bool is_globby(const Handle& glob) const;
Handle substitute(const Handle& tree,
const HandleSeq& args,
bool silent=false) const;
Handle substitute(const Handle& tree,
const HandleMap& map,
bool silent=false) const;
void extend(const Variables&);
void extend_intersect(const Variables&);
void erase(const Handle&);
void trim(const HandleSeq&);
void trim(const Handle& h) { trim(HandleSeq({h})); }
Handle get_type_decl(const Handle&, const Handle&) const;
Handle get_vardecl() const;
void find_variables(const Handle& body);
void find_variables(const HandleSeq& oset, bool ordered_link=true);
const GlobInterval get_interval(const Handle&) const;
std::string to_string(const std::string& indent=empty_string) const;
};
std::string oc_to_string(const VariableTypeMap&,
const std::string& indent=empty_string);
std::string oc_to_string(const Variables&,
const std::string& indent=empty_string);
}
#endif