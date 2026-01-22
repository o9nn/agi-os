#ifndef _OPENCOG_PATTERN_MATCH_ENGINE_H
#define _OPENCOG_PATTERN_MATCH_ENGINE_H
#include <map>
#include <set>
#include <stack>
#include <vector>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/pattern/Pattern.h>
#include <opencog/query/PatternMatchCallback.h>
namespace opencog {
class PatternMatchEngine
{
PatternMatchCallback &_pmc;
NameServer& _nameserver;
private:
const Variables* _variables;
const Pattern* _pat;
std::stack<const Variables*> _stack_variables;
std::stack<const Pattern*> _stack_pattern;
void push_redex(void);
void pop_redex(void);
GroundingMap var_grounding;
GroundingMap clause_grounding;
void record_grounding(const PatternTermPtr& ptm, const Handle& hg);
void clear_current_state(void);
typedef std::map<PatternTermPtr, size_t> ChoiceState;
ChoiceState _choice_state;
bool _need_choice_push;
size_t curr_choice(const PatternTermPtr&, const Handle&);
bool have_choice(const PatternTermPtr&, const Handle&);
bool _choose_next;
typedef PatternTermSeq Permutation;
typedef std::map<PatternTermPtr, Permutation> PermState;
typedef std::map<PatternTermPtr, int> PermCount;
typedef std::map<PatternTermPtr, bool> PermOdo;
typedef std::map<PatternTermPtr, PermOdo> PermOdoState;
PermState _perm_state;
Permutation curr_perm(const PatternTermPtr&);
bool have_perm(const PatternTermPtr&);
bool _perm_take_step;
bool _perm_have_more;
bool _perm_go_around;
PatternTermPtr _perm_to_step;
std::stack<PatternTermPtr> _perm_step_saver;
PatternTermPtr _perm_breakout;
PermOdo _perm_odo;
PermOdo _perm_podo;
PermOdoState _perm_odo_state;
std::stack<bool> _perm_take_stack;
std::stack<bool> _perm_more_stack;
std::stack<PatternTermPtr> _perm_stepper_stack;
std::stack<PatternTermPtr> _perm_breakout_stack;
std::stack<PermOdoState> _perm_odo_stack;
std::stack<PermState> _perm_stack;
PermCount _perm_count;
std::stack<PermCount> _perm_count_stack;
void perm_push(void);
void perm_pop(void);
typedef std::pair<PatternTermPtr, std::pair<size_t, size_t>> GlobPos;
typedef std::stack<GlobPos> GlobPosStack;
typedef std::map<PatternTermPtr, size_t> GlobGrd;
typedef std::pair<GlobGrd, GlobPosStack> GlobState;
std::map<PatternTermSeq, GlobState> _glob_state;
typedef std::vector<int> Rotors;
typedef std::map<PatternTermPtr, Rotors> SparseState;
typedef std::map<PatternTermPtr, Handle> SparseGlob;
typedef std::map<PatternTermPtr, PatternTermSeq> SparseTerm;
SparseState _sparse_state;
SparseGlob _sparse_glob;
SparseTerm _sparse_term;
bool _sparse_take_step;
bool setup_rotors(const PatternTermPtr&, const Handle&);
bool have_more_rotors(const PatternTermPtr&);
bool record_sparse(const PatternTermPtr&, const Handle&);
bool do_next_clause(void);
bool clause_accepted;
bool next_untried_present(const PatternTermPtr&,
const PatternTermPtr&,
PatternTermPtr&, PatternTermPtr&,
Handle&);
typedef std::set<PatternTermPtr> IssuedSet;
IssuedSet issued_present;
bool is_clause_grounded(const PatternTermPtr&) const;
HandleSeq clause_grounding_key(const Handle&,
const HandleSeq&) const;
std::unordered_map<HandleSeq, Handle> _gnd_cache;
std::unordered_set<HandleSeq> _nack_cache;
void solution_push(void);
void solution_pop(void);
void solution_drop(void);
std::stack<GroundingMap> var_solutn_stack;
std::stack<GroundingMap> _clause_solutn_stack;
std::stack<ChoiceState> choice_stack;
void clause_stacks_push(void);
void clause_stacks_pop(void);
void clause_stacks_clear(void);
unsigned int _clause_stack_depth;
std::vector<GroundingMap> _var_ground_cache;
std::vector<GroundingMap> _term_ground_cache;
bool _forall_state = true;
bool _did_check_forall;
bool report_grounding(const GroundingMap &var_soln,
const GroundingMap &term_soln);
bool report_forall(void);
std::set<GroundingMap> _grouping;
bool assign_grouping(const GroundingMap &var_soln,
const GroundingMap &term_soln);
unsigned int depth;
typedef enum {
CALL_ORDER,
CALL_GLOB,
CALL_SPARSE,
CALL_UNORDER,
CALL_PRESENT,
CALL_CHOICE,
CALL_CACHE,
CALL_SOLN
} Caller;
bool tree_compare(const PatternTermPtr&, const Handle&, Caller);
bool variable_compare(const Handle&, const Handle&);
bool self_compare(const PatternTermPtr&);
bool node_compare(const Handle&, const Handle&);
bool present_compare(const PatternTermPtr&, const Handle&);
bool choice_compare(const PatternTermPtr&, const Handle&);
bool ordered_compare(const PatternTermPtr&, const Handle&);
bool unorder_compare(const PatternTermPtr&, const Handle&);
bool sparse_compare(const PatternTermPtr&, const Handle&);
bool glob_compare(const PatternTermSeq&, const HandleSeq&);
bool explore_clause(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_clause_direct(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_clause_evaluatable(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_clause_identical(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_term_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_up_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_upord_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_upund_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_upspar_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_upglob_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_glob_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_sparse_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_type_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_odometer(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_unordered_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_choice_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_present_branches(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_single_branch(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool do_term_up(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool clause_accept(const PatternTermPtr&, const Handle&);
public:
PatternMatchEngine(PatternMatchCallback&);
void set_pattern(const Variables&, const Pattern&);
bool explore_neighborhood(const PatternTermPtr&, const Handle&,
const PatternTermPtr&);
bool explore_constant_evaluatables(const PatternTermSeq& clauses);
static void print_solution(const GroundingMap &vars,
const GroundingMap &clauses);
static void log_solution(const GroundingMap &vars,
const GroundingMap &clauses);
static void log_term(const HandleSet &vars,
const HandleSeq &clauses);
};
}
#endif