#ifndef _MOSES_BUILD_KNOBS_H
#define _MOSES_BUILD_KNOBS_H
#include <vector>
#include <boost/utility.hpp>
#include <boost/ptr_container/ptr_vector.hpp>
#include <moses/comboreduct/type_checker/type_tree.h>
#include <moses/comboreduct/combo/action.h>
#include <moses/comboreduct/combo/perception.h>
#include "field_set.h"
#include "representation.h"
namespace opencog { namespace moses {
using namespace combo;
struct build_knobs : boost::noncopyable
{
build_knobs(combo_tree& exemplar,
const type_tree& tt,
representation& rep,
const operator_set& ignore_ops = operator_set(),
const combo_tree_ns_set* perceptions = NULL,
const combo_tree_ns_set* actions = NULL,
bool linear_regression = true,
contin_t step_size = 1.0,
contin_t expansion = 1.0,
field_set::width_t depth = 4,
float perm_ratio = 0.0);
protected:
void build_logical(combo_tree::iterator sub,
combo_tree::iterator it);
void build_contin(combo_tree::iterator it);
void build_enum(combo_tree::iterator it);
void build_action(combo_tree::iterator it);
protected:
combo_tree& _exemplar;
representation& _rep;
bool _skip_disc_probe;
const combo::arity_t _arity;
const type_tree _signature;
bool _linear_contin;
contin_t _step_size, _expansion;
field_set::width_t _depth;
float _perm_ratio;
const operator_set& _ignore_ops;
const combo_tree_ns_set* _perceptions;
const combo_tree_ns_set* _actions;
protected:
bool permitted_op(const vertex& v) const;
void logical_canonize(combo_tree::iterator);
template<typename It>
boost::ptr_vector<logical_subtree_knob> logical_probe_rec(
combo_tree::iterator subtree,
combo_tree& exemplar,
combo_tree::iterator it,
It from, It to,
bool add_if_in_exemplar,
unsigned n_jobs = 1) const;
void logical_cleanup();
void add_logical_knobs(combo_tree::iterator subtree,
combo_tree::iterator it,
bool add_if_in_exemplar = true);
void sample_logical_perms(combo_tree::iterator it,
std::vector<combo_tree>& perms);
void insert_typed_arg(combo_tree &tr,
type_tree_sib_it arg_type,
const argument &arg,
bool negate = false);
bool disc_probe(combo_tree::iterator subtree, disc_knob_base& kb) const;
void contin_canonize(combo_tree::iterator);
void canonize_div(combo_tree::iterator it);
void add_constant_child(combo_tree::iterator it, contin_t v);
combo_tree::iterator canonize_times(combo_tree::iterator it);
void linear_canonize_times(combo_tree::iterator it);
void linear_canonize(combo_tree::iterator it);
void rec_canonize(combo_tree::iterator it);
void append_linear_combination(combo_tree::iterator it);
combo_tree::iterator mult_add(combo_tree::iterator it, const vertex& v);
void enum_canonize(combo_tree::iterator);
void action_canonize(combo_tree::iterator);
void add_action_knobs(combo_tree::iterator it,
bool add_if_in_exemplar = true);
void add_simple_action_knobs(combo_tree::iterator it,
bool add_if_in_exemplar = true);
void sample_action_perms(combo_tree::iterator it,
std::vector<combo_tree>& perms);
void simple_action_probe(combo_tree::iterator it, bool add_if_in_exemplar);
void action_probe(std::vector<combo_tree>& perms,
combo_tree::iterator it, bool add_if_in_exemplar);
void action_cleanup();
void ann_canonize(combo_tree::iterator);
typedef boost::shared_mutex shared_mutex;
typedef boost::shared_lock<shared_mutex> shared_lock;
typedef boost::unique_lock<shared_mutex> unique_lock;
mutable shared_mutex lp_mutex;
};
}
}
#endif