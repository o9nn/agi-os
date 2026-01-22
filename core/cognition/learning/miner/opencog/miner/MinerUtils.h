#ifndef OPENCOG_MINER_UTILS_H_
#define OPENCOG_MINER_UTILS_H_
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/unify/Unify.h>
#include "Valuations.h"
namespace opencog
{
typedef std::vector<HandleSeqSeq> HandleSeqSeqSeq;
typedef std::pair<HandleSet, GlobInterval> ValIntvlPair;
typedef std::map<Handle, ValIntvlPair> HandleValIntvlMap;
class MinerUtils
{
public:
static const bool use_present_link = true;
static HandleSetSeq shallow_abstract(const Valuations &valuations,
unsigned ms,
bool enable_type,
bool enable_glob,
const HandleSeq& ignore_vars);
static HandleSet focus_shallow_abstract(const Valuations &valuations,
unsigned ms, bool enable_type,
bool enable_glob);
static bool is_nullary(const Handle& h);
static Handle shallow_abstract_of_val(const Handle& value);
static Handle shallow_abstract_of_val(const Handle& value, const HandleSeq& rnd_vars);
static HandleSeq glob_shallow_abstract_of_val(const Handle &val,
const Handle &var, bool enable_type);
static HandleSeq glob_shallow_abstract_of_lst(const Handle &value,
const HandleSeq &vars, bool enable_type);
static Handle variable_set(const HandleSeq& vars);
static Handle lambda(const Handle& vardecl, const Handle& body);
static Handle quote(const Handle& h);
static Handle unquote(const Handle& h);
static Handle local_quote(const Handle& h);
static Handle compose(const Handle& pattern, const HandleMap& var2pat);
static Handle compose_nocheck(const Handle& pattern, const HandlePair& var2pat);
static HandleSeq get_db(const Handle& db_cpt);
static unsigned get_uint(const Handle& h);
static double get_double(const Handle& h);
static unsigned support(const Handle& pattern,
const HandleSeq& db,
unsigned ms);
static unsigned component_support(const Handle& pattern,
const HandleSeq& db,
unsigned ms);
static bool enough_support(const Handle& pattern,
const HandleSeq& db,
unsigned ms);
static HandleSetSeq shallow_abstract(const Handle& pattern,
const HandleSeq& db,
unsigned ms,
bool enable_type,
bool enable_glob,
const HandleSeq& ignore_vars);
static HandleSet shallow_specialize(const Handle& pattern,
const HandleSeq& db,
unsigned ms,
unsigned mv=UINT_MAX,
bool enable_type=false,
bool enable_glob=false,
const HandleSeq& ignore_vars={});
static Handle mk_body(const HandleSeq clauses);
static Handle mk_pattern_no_vardecl(const HandleSeq& clauses);
static Handle mk_pattern_filtering_vardecl(const Handle& vardecl,
const HandleSeq& clauses);
static Handle mk_pattern(const Handle& vardecl, const HandleSeq& clauses);
static HandleSeq get_component_patterns(const Handle& pattern);
static HandleSeqSeq get_components(const HandleSeq& clauses);
static HandleSeq get_conjuncts(const Handle& pattern);
static Handle restricted_satisfying_set(const Handle& pattern,
const HandleSeq& db,
unsigned ms=UINT_MAX);
static bool totally_abstract(const Handle& pattern);
static HandleSeq gen_rand_variables(size_t n);
static Handle gen_rand_variable();
static HandleSeq gen_rand_globs(size_t n);
static Handle gen_rand_glob();
static const Variables& get_variables(const Handle& pattern);
static Handle get_vardecl(const Handle& pattern);
static const Handle& get_body(const Handle& pattern);
static HandleSeq get_clauses(const Handle& pattern);
static HandleSeq get_clauses_of_body(const Handle& body);
static unsigned n_conjuncts(const Handle& pattern);
static Handle remove_useless_clauses(const Handle& pattern);
static Handle remove_useless_clauses(const Handle& vardecl,
const Handle& body);
static void remove_useless_clauses(const Handle& vardecl,
HandleSeq& clauses);
static void remove_constant_clauses(const Handle& vardecl,
HandleSeq& clauses);
static void remove_redundant_subclauses(HandleSeq& clauses);
static void remove_redundant_clauses(HandleSeq& clauses);
static void remove_abstract_clauses(HandleSeq& clauses);
static bool has_only_joint_variables(const Handle& clause,
const HandleSeq& clauses);
static bool is_blk_syntax_more_abstract(const HandleSeq& l_blk,
const HandleSeq& r_blk,
const Handle& var);
static bool is_pat_syntax_more_abstract(const Handle& l_pat,
const Handle& r_pat,
const Handle& var);
static bool is_pat_more_abstract(const Handle& l_pat,
const Handle& r_pat,
const Handle& var);
static bool is_blk_more_abstract(const HandleSeq& l_blk,
const HandleSeq& r_blk,
const Handle& var);
static bool is_more_abstract_foreach_var(const Handle& clause,
const HandleSeq& others);
static HandleSeqSeq powerseq_without_empty(const HandleSeq& blk);
static Handle alpha_convert(const Handle& pattern,
const Variables& other_vars);
static bool is_value(const Unify::HandleCHandleMap::value_type& var_val,
const Variables& vars, const Handle& var);
static HandleSeqSeq connected_subpatterns_with_var(const HandleSeqSeq& partition,
const Handle& var);
static HandleSeq connected_subpattern_with_var(const HandleSeq& blk,
const Handle& var);
static HandleSeqSeqSeq combinatorial_insert(const Handle& h,
const HandleSeqSeq& hss);
static HandleSeqSeqSeq combinatorial_insert(const Handle& h,
HandleSeqSeq::const_iterator from,
HandleSeqSeq::const_iterator to);
static HandleSeqSeqSeq partitions(const HandleSeq& hs);
static HandleSeqSeqSeq partitions(HandleSeq::const_iterator from,
HandleSeq::const_iterator to);
static HandleSeqSeqSeq partitions_without_pattern(const Handle& pattern);
static Handle expand_conjunction_disconnect(const Handle& cnjtion,
const Handle& pattern);
static Handle expand_conjunction_connect(const Handle& cnjtion,
const Handle& pattern,
const Handle& cnjtion_var,
const Handle& pattern_var);
static Handle expand_conjunction_connect(const Handle& cnjtion,
const Handle& pattern,
const HandleMap& pv2cv);
static HandleSet expand_conjunction_rec(const Handle& cnjtion,
const Handle& pattern,
const HandleSeq& db,
unsigned ms,
unsigned mv,
const HandleMap& pv2cv=HandleMap(),
unsigned pvi=0);
static HandleSet expand_conjunction_es_rec(const Handle& cnjtion,
const Handle& pattern,
const HandleSeq& db,
unsigned ms,
unsigned mv,
const HandleMap& pv2cv=HandleMap(),
unsigned pvi=0);
static HandleSet expand_conjunction(const Handle& cnjtion,
const Handle& pattern,
const HandleSeq& db,
unsigned ms,
unsigned mv=UINT_MAX,
bool es=true);
static const Handle& support_key();
static void set_support(const Handle& pattern, double support);
static double get_support(const Handle& pattern);
static double support_mem(const Handle& pattern,
const HandleSeq& db,
unsigned ms);
static void remove_if(HandleSeq& clauses,
std::function<bool(const Handle&, const HandleSeq&)> fun);
static HandleSet type_restrict_patterns(const HandleSeqMap &);
static Handle type_restrict_pattern(const HandleSeqMap::value_type &pair);
static Handle lwst_com_types_decl(const Handle &var, const HandleSeq &vector,
const GlobInterval &);
static TypeSet lwst_com_types(HandleSeq vals);
static TypeSet lwst_com_types(TypeSet tsets);
static HandleValIntvlMap simple_unify(const HandleSeq &pat, const HandleSeq &val);
static void extend_seq_map(HandleValIntvlMap &sup, const HandleValIntvlMap &sub);
};
std::string oc_to_string(const HandleSeqSeqSeq& hsss,
const std::string& indent=empty_string);
}
#endif