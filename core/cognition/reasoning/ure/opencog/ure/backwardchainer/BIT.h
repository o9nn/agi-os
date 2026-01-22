#ifndef _OPENCOG_BIT_H
#define _OPENCOG_BIT_H
#include <boost/operators.hpp>
#include <opencog/util/empty_string.h>
#include <opencog/util/RandGen.h>
#include <opencog/ure/Rule.h>
#include <opencog/ure/Utils.h>
#include <opencog/atoms/base/Handle.h>
#include "Fitness.h"
class BITUTest;
namespace opencog
{
class BITNode
{
public:
BITNode(const Handle& body=Handle::UNDEFINED,
const BITNodeFitness& fitness=BITNodeFitness());
Handle body;
BITNodeFitness fitness;
RuleTypedSubstitutionMap rules;
double complexity;
bool exhausted;
double operator()() const;
std::string to_string(const std::string& indent="") const;
};
class AndBIT : public boost::totally_ordered<AndBIT>
{
friend class ::BITUTest;
public:
Handle fcs;
typedef std::unordered_map<Handle, BITNode> HandleBITNodeMap;
HandleBITNodeMap leaf2bitnode;
double complexity;
bool exhausted;
const AtomSpace* queried_as;
AndBIT();
AndBIT(AtomSpace& bit_as, const Handle& target, Handle vardecl,
const BITNodeFitness& fitness=BITNodeFitness(),
const AtomSpace* queried_as=nullptr);
AndBIT(const Handle& fcs, double complexity=0.0,
const AtomSpace* queried_as=nullptr);
~AndBIT();
AndBIT expand(const Handle& leaf,
const RuleTypedSubstitutionPair& rule,
double prob=1.0) const;
BITNode* select_leaf();
void reset_exhausted();
bool has_cycle() const;
bool has_cycle(const Handle& h, HandleSet ancestors = {}) const;
bool operator==(const AndBIT& andbit) const;
bool operator<(const AndBIT& andbit) const;
std::string to_string(const std::string& indent="") const;
std::string fcs_to_ascii_art(const Handle& fcs) const;
std::string fcs_rewrite_to_ascii_art(const Handle& h) const;
private:
typedef std::discrete_distribution<size_t> LeafDistribution;
double expand_complexity(const Handle& leaf, double prob) const;
Handle expand_fcs(const Handle& leaf,
const RuleTypedSubstitutionPair& rule) const;
void set_leaf2bitnode();
HandleBITNodeMap::iterator
insert_bitnode(Handle leaf, const BITNodeFitness& fitness);
HandleSet get_leaves() const;
HandleSet get_leaves(const Handle& h) const;
Handle substitute_unified_variables(const Handle& leaf,
const Unify::TypedSubstitution& ts) const;
Handle expand_fcs_pattern(const Handle& fcs_pattern, const Rule& rule) const;
Handle expand_fcs_rewrite(const Handle& fcs_rewrite,
const Rule& rule) const;
bool is_argument_of(const Handle& eval, const Handle& atom) const;
bool is_locally_quoted_eq(const Handle& lhs, const Handle& rhs) const;
Handle mk_pattern(HandleSeq prs_clauses, HandleSeq virt_clauses) const;
static void remove_redundant(HandleSeq& hs);
static HandleSeq get_present_clauses(const Handle& pattern);
static HandleSeq get_present_clauses(const HandleSeq& clauses);
static HandleSeq get_virtual_clauses(const Handle& pattern);
static HandleSeq get_virtual_clauses(const HandleSeq& clauses);
static std::vector<std::string>
ascii_art_hmerge(const std::vector<std::string>& laa ,
const std::vector<std::string>& raa ,
unsigned dst);
static std::string ascii_art_hmerge(const std::vector<std::string>& aas,
unsigned dst=1);
static std::string ascii_art_hmerge(const std::string& laa,
const std::string& raa,
unsigned dst);
static std::vector<std::string> reverse_split(const std::string& aa);
static std::string bottom_line(const std::string& aa);
static unsigned leading_spaces(const std::string& line);
static std::string remove_vowels(std::string str, size_t tg_size);
static std::string remove_consonants(std::string str, size_t tg_size);
static std::string abbreviate(std::string str, size_t tg_size);
static std::string line_separator(const std::string& up_aa,
const std::string& low_aa,
const Handle& gsn,
bool unordered_premises=false);
};
class BIT
{
public:
AtomSpace bit_as;
typedef std::vector<AndBIT> AndBITs;
AndBITs andbits;
BIT();
BIT(AtomSpace& as, const Handle& target, const Handle& vardecl,
const BITNodeFitness& fitness=BITNodeFitness());
~BIT();
bool empty() const;
size_t size() const;
AndBIT* init();
AndBIT* expand(AndBIT& andbit, BITNode& bitleaf,
const RuleTypedSubstitutionPair& rule,
double prob=1.0);
AndBIT* insert(AndBIT& andbit);
template<typename It> AndBITs::iterator erase(It pos);
void reset_exhausted_flags();
bool andbits_exhausted() const;
bool contains(const BITNode& bitnode,
const RuleTypedSubstitutionPair& rule) const;
private:
AtomSpace* _as;
Handle _init_target;
Handle _init_vardecl;
BITNodeFitness _init_fitness;
};
template<typename It>
BIT::AndBITs::iterator BIT::erase(It pos)
{
remove_hypergraph(bit_as, pos->fcs);
return andbits.erase(pos);
}
std::string oc_to_string(const BITNode& bitnode,
const std::string& indent=empty_string);
std::string oc_to_string(const AndBIT& andbit,
const std::string& indent=empty_string);
}
#endif