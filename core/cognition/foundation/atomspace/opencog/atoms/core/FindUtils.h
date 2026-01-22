#ifndef _OPENCOG_FIND_UTILS_H
#define _OPENCOG_FIND_UTILS_H
#include <set>
#include <vector>
#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/core/Quotation.h>
#include <opencog/atoms/core/ScopeLink.h>
namespace opencog {
class FindAtoms
{
public:
TypeSet stopset;
HandleSet varset;
HandleSet holders;
HandleSet least_holders;
FindAtoms(Type t, bool subclass = false);
FindAtoms(Type ta, Type tb, bool subclass = false);
FindAtoms(const Handle& atom);
FindAtoms(const HandleSet& selection);
void search_set(const Handle& h);
void search_set(const HandleSeq& hlist);
private:
typedef enum
{
NOPE,
YEP,
IMM
} Loco;
Loco find_rec(const Handle& h, Quotation quotation=Quotation());
private:
TypeSet _target_types;
HandleSet _target_atoms;
};
bool is_atom_in_tree(const Handle& tree, const Handle& atom);
bool is_found_in_tree(const Handle& tree,
const Handle& atom,
bool (*reject)(const Handle& tree,
const Handle& subtree,
const Handle& atom));
bool is_quoted_in_tree(const Handle& tree, const Handle& atom);
bool is_unquoted_in_tree(const Handle& tree, const Handle& atom);
int min_quotation_level(const Handle& tree,
const Handle& atom,
Quotation quotation=Quotation());
int max_quotation_level(const Handle& tree,
const Handle& atom,
Quotation quotation=Quotation());
bool is_unscoped_in_tree(const Handle& tree, const Handle& atom);
bool is_constant_in_tree(const Handle& tree, const Handle& atom);
bool is_unquoted_unscoped_in_tree(const Handle& tree, const Handle& atom);
bool is_free_in_tree(const Handle& tree, const Handle& atom);
bool is_unquoted_unscoped_in_any_tree(const HandleSeq& trees,
const Handle& atom);
bool is_free_in_any_tree(const HandleSeq& hs, const Handle& atom);
bool any_atom_in_tree(const Handle& tree,
const HandleSet& atoms);
bool any_unquoted_in_tree(const Handle& tree,
const HandleSet& atoms);
bool any_unscoped_in_tree(const Handle& tree,
const HandleSet& atoms);
bool any_constant_in_tree(const Handle& tree,
const HandleSet& atoms);
bool any_unquoted_unscoped_in_tree(const Handle& tree,
const HandleSet& atoms);
bool any_free_in_tree(const Handle& tree,
const HandleSet& atoms);
bool any_free_in_tree(const Handle& tree,
const HandleSeq& atoms);
unsigned int num_unquoted_unscoped_in_tree(const Handle& tree,
const HandleSet& atoms);
HandleSet unquoted_unscoped_in_tree(const Handle& tree,
const HandleSet& atoms);
bool is_atom_in_any_tree(const HandleSeq& trees,
const Handle& atom);
bool is_unquoted_in_any_tree(const HandleSeq& trees,
const Handle& atom);
bool contains_atomtype(const Handle& clause, Type atom_type,
Quotation quotation=Quotation());
bool contains_exposed_atomtype(const Handle& clause, Type atom_type,
Quotation quotation=Quotation());
size_t contains_atomtype_count(const Handle& clause, Type atom_type,
Quotation quotation=Quotation());
HandleSet get_free_variables(const Handle& h,
Quotation quotation=Quotation());
HandleSet get_free_variables(const HandleSeq& hs,
Quotation quotation=Quotation());
HandleSet get_all_uniq_atoms(const Handle& h);
bool is_closed(const Handle& h, Quotation quotation=Quotation());
bool is_constant(const Handle& h, Quotation quotation=Quotation());
}
#endif