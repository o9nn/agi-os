#ifndef _OPENCOG_UNIFY_UTILS_H
#define _OPENCOG_UNIFY_UTILS_H
#include <boost/operators.hpp>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/core/Context.h>
#include <opencog/atoms/core/VariableList.h>
#include <opencog/atoms/core/Variables.h>
#include <opencog/atoms/pattern/BindLink.h>
namespace opencog {
class Unify
{
friend class UnifyUTest;
public:
struct CHandle : public boost::totally_ordered<CHandle>
{
CHandle(const Handle& handle, const Context& context=Context());
Handle handle;
Context context;
bool is_variable() const;
bool is_free_variable() const;
HandleSet get_free_variables() const;
Context::VariablesStack::const_iterator
find_variables(const Handle& h) const;
bool is_consumable() const;
bool is_quoted() const;
bool is_unquoted() const;
void update();
bool is_node_satisfiable(const CHandle& other) const;
bool operator==(const CHandle& other) const;
bool operator<(const CHandle& other) const;
explicit operator bool() const;
};
typedef std::pair<CHandle, CHandle> CHandlePair;
typedef std::set<CHandle> Block;
typedef std::map<Block, CHandle> Partition;
typedef Partition::value_type TypedBlock;
typedef std::vector<TypedBlock> TypedBlockSeq;
typedef std::set<Partition> Partitions;
static const Partitions empty_partitions;
static const Partitions empty_partition_singleton;
struct SolutionSet : Partitions
{
explicit SolutionSet(const Partitions& p);
explicit SolutionSet(bool s=false);
bool is_satisfiable() const;
void insert(const SolutionSet& sol);
void remove_cycles();
};
typedef std::map<Handle, CHandle> HandleCHandleMap;
typedef std::map<HandleCHandleMap, Handle> TypedSubstitutions;
typedef std::pair<HandleCHandleMap, Handle> TypedSubstitution;
Unify(const Handle& lhs, const Handle& rhs,
const Handle& lhs_vardecl=Handle::UNDEFINED,
const Handle& rhs_vardecl=Handle::UNDEFINED);
Unify(const Handle& lhs, const Handle& rhs,
const Variables& lhs_vars, const Variables& rhs_vars);
TypedSubstitutions typed_substitutions(const SolutionSet& sol,
const Handle& pre) const;
TypedSubstitution typed_substitution(const Partition& partition,
const Handle& pre) const;
HandleCHandleMap substitution_closure(const HandleCHandleMap& var2val) const;
Handle substitution_vardecl(const HandleCHandleMap& var2val) const;
static BindLinkPtr consume_quotations(BindLinkPtr bl);
static bool is_pm_connector(const Handle& h);
static bool is_pm_connector(Type t);
static HandleMultimap vargraph(const Partition& partition);
static HandleMultimap vargraph(const Block& blk);
static bool has_cycle(const Partition& partition);
static bool has_cycle(const Block& blk);
static bool has_cycle(const HandleMultimap& vg);
static HandleMultimap closure(const HandleMultimap& vg);
static HandleMultimap closure_step(const HandleMultimap& vg);
static Handle substitute(BindLinkPtr bl,
const TypedSubstitution& ts,
const AtomSpace* queried_as=nullptr);
static Handle substitute(BindLinkPtr bl, const HandleMap& var2val,
Handle vardecl=Handle::UNDEFINED,
const AtomSpace* queried_as=nullptr);
static Handle substitute_vardecl(const Handle& vardecl,
const HandleMap& var2val);
static Handle remove_constant_clauses(const Handle& vardecl,
const Handle& clauses,
const AtomSpace* queried_as=nullptr);
SolutionSet operator()();
private:
Handle _lhs;
Handle _rhs;
Variables _variables;
public:
void set_variables(const Handle& lhs, const Handle& rhs,
const Handle& lhs_vardecl=Handle::UNDEFINED,
const Handle& rhs_vardecl=Handle::UNDEFINED);
private:
CHandle find_least_abstract(const TypedBlock& block, const Handle& pre) const;
SolutionSet unify(const CHandle& lhs, const CHandle& rhs) const;
SolutionSet unify(const Handle& lhs, const Handle& rhs,
Context lhs_context=Context(),
Context rhs_context=Context()) const;
SolutionSet unordered_unify(const HandleSeq& lhs, const HandleSeq& rhs,
Context lhs_context=Context(),
Context rhs_context=Context()) const;
SolutionSet ordered_unify(const HandleSeq& lhs, const HandleSeq& rhs,
Context lhs_context=Context(),
Context rhs_context=Context()) const;
SolutionSet pairwise_unify(const std::set<CHandlePair>& pchs) const;
SolutionSet comb_unify(const std::set<CHandle>& lhs,
const std::set<CHandle>& rhs) const;
SolutionSet comb_unify(const std::set<CHandle>& chs) const;
HandleSeq cp_erase(const HandleSeq& hs, Arity i) const;
SolutionSet mkvarsol(CHandle lhs, CHandle rhs) const;
public:
SolutionSet join(const SolutionSet& lhs, const SolutionSet& rhs) const;
private:
SolutionSet join(const SolutionSet& lhs, const Partition& rhs) const;
SolutionSet join(const Partition& lhs, const Partition& rhs) const;
SolutionSet join(const SolutionSet& sol, const TypedBlock& block) const;
SolutionSet join(const Partition& partition, const TypedBlock &block) const;
TypedBlock join(const TypedBlockSeq& common_blocks,
const TypedBlock& block) const;
TypedBlock join(const TypedBlock& lhs, const TypedBlock& rhs) const;
SolutionSet subunify(const TypedBlockSeq& common_blocks,
const TypedBlock& block) const;
SolutionSet subunify(const TypedBlock& lhs, const TypedBlock& rhs) const;
bool is_satisfiable(const TypedBlock& block) const;
public:
CHandle type_intersection(const CHandle& lch, const CHandle& rch) const;
private:
TypeSet simplify_type_union(TypeSet& type) const;
TypeSet get_union_type(const Handle& h) const;
bool inherit(const CHandle& lhs, const CHandle& rhs) const;
bool inherit(const Handle& lhs, const Handle& rhs,
Context lc=Context(), Context rc=Context()) const;
bool inherit(Type lhs, Type rhs) const;
bool inherit(Type lhs, const TypeSet& rhs) const;
bool inherit(const TypeSet& lhs, const TypeSet& rhs) const;
bool inherit(const std::pair<double, double> &lgm,
const std::pair<double, double> &rgm) const;
bool is_declared_variable(const Handle& h) const;
bool is_declared_variable(const CHandle& ch) const;
bool is_free_declared_variable(const CHandle& ch) const;
bool is_free_declared_variable(const Context& c, const Handle& h) const;
bool is_node_satisfiable(const CHandle& lch, const CHandle& rch) const;
template<typename F, typename A>
static A fixpoint(const F& fun, const A& arg)
{
A res = fun(arg);
if (res == arg)
return res;
return fixpoint(fun, res);
}
HandleSeq tail(const HandleSeq &seq) const;
HandleSeq tail(const HandleSeq &seq, const size_t offset) const;
void ordered_unify_glob(const HandleSeq &lhs, const HandleSeq &rhs,
SolutionSet &sol,
Context lhs_context=Context(),
Context rhs_context=Context(),
bool flip=false) const;
};
bool unifiable(const Handle& lhs, const Handle& rhs,
const Handle& lhs_vardecl=Handle::UNDEFINED,
const Handle& rhs_vardecl=Handle::UNDEFINED);
bool hm_content_eq(const HandleMap& lhs, const HandleMap& rhs);
bool hchm_content_eq(const Unify::HandleCHandleMap& lhs,
const Unify::HandleCHandleMap& rhs);
bool ts_content_eq(const Unify::TypedSubstitution& lhs,
const Unify::TypedSubstitution& rhs);
bool tss_content_eq(const Unify::TypedSubstitutions& lhs,
const Unify::TypedSubstitutions& rhs);
HandleMap strip_context(const Unify::HandleCHandleMap& hchm);
Variables merge_variables(const Variables& lv, const Variables& rv);
Handle merge_vardecl(const Handle& l_vardecl, const Handle& r_vardecl);
std::string oc_to_string(const Unify::CHandle& ch,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::Block& pb,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::Partition& hshm,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedBlock& tb,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedBlockSeq& tbs,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::Partitions& par,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::HandleCHandleMap& hchm,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::HandleCHandleMap::value_type& hch,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedSubstitution& ts,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedSubstitutions::value_type& ts,
const std::string& indent=empty_string);
std::string oc_to_string(const Unify::TypedSubstitutions& tss,
const std::string& indent=empty_string);
}
#endif