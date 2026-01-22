#ifndef _OPENCOG_FREE_VARIABLES_H
#define _OPENCOG_FREE_VARIABLES_H
#include <algorithm>
#include <map>
#include <set>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/core/Replacement.h>
namespace opencog
{
struct FreeVariables : Replacement
{
HandleSeq varseq;
HandleSet varset;
IndexMap index;
FreeVariables() {}
FreeVariables(const std::initializer_list<Handle>& variables);
void init_index();
bool is_identical(const FreeVariables& other) const;
bool varset_contains(const Handle& var) const;
template<typename It>
bool varset_includes(It from, It to) const {
return std::all_of(from, to, [&](const Handle& v)
{ return varset_contains(v); });
}
template <typename C>
bool varset_includes(const C& c) const {
return varset_includes(c.begin(), c.end());
}
void find_variables(const Handle& body);
void find_variables(const HandleSeq& oset, bool ordered_link=true);
void canonical_sort(const HandleSeq&);
HandleSeq make_sequence(const HandleMap&) const;
void erase(const Handle&);
Handle substitute_nocheck(const Handle&,
const HandleSeq&,
bool silent=false,
bool do_exec=false) const;
Handle substitute_nocheck(const Handle&,
const HandleMap&,
bool silent=false,
bool do_exec=false) const;
bool operator<(const FreeVariables& other) const;
std::size_t size() const;
bool empty() const;
std::string to_string(const std::string& indent=empty_string) const;
};
struct VarScraper;
typedef std::pair<Type, Arity> TypeArityPair;
typedef std::vector<TypeArityPair> Path;
typedef std::multiset<Path> PathMultiset;
typedef std::map<Handle, PathMultiset> HandlePathsMap;
std::string oc_to_string(const TypeArityPair& tap,
const std::string& indent=empty_string);
std::string oc_to_string(const Path& path,
const std::string& indent=empty_string);
std::string oc_to_string(const PathMultiset& paths,
const std::string& indent=empty_string);
std::string oc_to_string(const HandlePathsMap& hpsm,
const std::string& indent=empty_string);
std::string oc_to_string(const VarScraper& vsc,
const std::string& indent=empty_string);
std::string oc_to_string(const FreeVariables::IndexMap& imap,
const std::string& indent=empty_string);
std::string oc_to_string(const FreeVariables& var,
const std::string& indent=empty_string);
}
#endif