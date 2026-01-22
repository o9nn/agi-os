#ifndef _COMBO_VERTEX_H
#define _COMBO_VERTEX_H
#include <boost/functional/hash.hpp>
#include <boost/variant.hpp>
#include <opencog/util/tree.h>
#include <opencog/util/numeric.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/oc_assert.h>
#include <moses/comboreduct/combo/action.h>
#include <moses/comboreduct/combo/action_symbol.h>
#include <moses/comboreduct/combo/ann.h>
#include <moses/comboreduct/combo/argument.h>
#include <moses/comboreduct/combo/builtin_action.h>
#include <moses/comboreduct/combo/common_def.h>
#include <moses/comboreduct/combo/definite_object.h>
#include <moses/comboreduct/combo/enum_type.h>
#include <moses/comboreduct/combo/indefinite_object.h>
#include <moses/comboreduct/combo/message.h>
#include <moses/comboreduct/combo/perception.h>
namespace opencog { namespace combo {
class procedure_call_base;
typedef const procedure_call_base* procedure_call;
namespace id {
enum __attribute__ ((packed)) builtin
{
null_vertex = 0,
logical_true, logical_false,
logical_and, logical_or, logical_not,
plus, times, div, exp,
log,
sin,
greater_than_zero,
impulse,
rand,
list,
car, cdr, cons,
foldr, foldl,
cond,
equ,
lambda,
apply,
contin_if,
builtin_count
};
}
typedef id::builtin builtin;
namespace id {
enum wild_card {
asterisk = 0,
wild_card_count
};
}
typedef id::wild_card wild_card;
typedef double contin_t;
typedef boost::variant < contin_t,
enum_t,
builtin,
wild_card,
argument,
action,
builtin_action,
perception,
definite_object,
indefinite_object,
message,
procedure_call,
action_symbol,
ann_type > vertex;
typedef std::set<vertex> vertex_set;
typedef vertex_set::iterator vertex_set_it;
typedef vertex_set::const_iterator vertex_set_const_it;
typedef std::vector<vertex> vertex_seq;
typedef vertex_seq::iterator vertex_seq_it;
typedef vertex_seq::const_iterator vertex_seq_const_it;
static const vertex_seq empty_vertex_seq = vertex_seq();
typedef std::set<vertex> operator_set;
typedef operator_set::iterator operator_set_it;
typedef operator_set::iterator operator_set_const_it;
typedef std::vector<vertex> argument_list;
typedef argument_list::iterator argument_list_it;
typedef argument_list::const_iterator argument_list_const_it;
typedef std::vector<argument_list> argument_list_list;
typedef argument_list_list::iterator argument_list_list_it;
typedef argument_list_list::const_iterator argument_list_list_const_it;
std::istream& operator>>(std::istream& in, combo::vertex& v);
inline bool operator==(const vertex& v, contin_t c)
{
if (const contin_t* vc = boost::get<contin_t>(&v))
return (*vc == c);
return false;
}
inline bool operator==(contin_t c, const vertex& v)
{
return (v == c);
}
inline bool operator!=(const vertex& v, contin_t c)
{
return !(v == c);
}
inline bool operator!=(contin_t c, const vertex& v)
{
return !(v == c);
}
inline bool operator==(const vertex& v, builtin h)
{
if (const builtin* vh = boost::get<builtin>(&v))
return (*vh == h);
return false;
}
inline bool operator==(builtin h, const vertex& v)
{
return (v == h);
}
inline bool operator!=(const vertex& v, builtin h)
{
return !(v == h);
}
inline bool operator!=(builtin h, const vertex& v)
{
return !(v == h);
}
inline bool operator==(const vertex& v, const wild_card& w)
{
if (const wild_card* vw = boost::get<wild_card>(&v))
return (*vw == w);
return false;
}
inline bool operator==(const wild_card& w, const vertex& v)
{
return (w == v);
}
inline bool operator!=(const vertex& v, const wild_card& w)
{
return !(v == w);
}
inline bool operator!=(const wild_card& w, const vertex& v)
{
return !(v == w);
}
inline bool operator==(const vertex& v, const action& a)
{
if (const action* va = boost::get<action>(&v))
return (*va == a);
return false;
}
inline bool operator==(action a, const vertex& v)
{
return (v == a);
}
inline bool operator!=(const vertex& v, action a)
{
return !(v == a);
}
inline bool operator!=(action a, const vertex& v)
{
return !(v == a);
}
inline bool operator==(const vertex& v, builtin_action a)
{
if (const builtin_action* va = boost::get<builtin_action>(&v))
return (*va == a);
return false;
}
inline bool operator==(builtin_action a, const vertex& v)
{
return (v == a);
}
inline bool operator!=(const vertex& v, builtin_action a)
{
return !(v == a);
}
inline bool operator!=(builtin_action a, const vertex& v)
{
return !(v == a);
}
inline bool operator==(const vertex& v, perception p)
{
if (const perception* vp = boost::get<perception>(&v))
return (*vp == p);
return false;
}
inline bool operator==(perception p, const vertex& v)
{
return (v == p);
}
inline bool operator!=(const vertex& v, perception p)
{
return !(v == p);
}
inline bool operator!=(perception p, const vertex& v)
{
return !(v == p);
}
inline bool operator==(const vertex& v, const definite_object& d)
{
if (const definite_object*
vd = boost::get<definite_object>(&v))
return (*vd == d);
return false;
}
inline bool operator==(const definite_object& d, const vertex& v)
{
return (v == d);
}
inline bool operator!=(const vertex& v, const definite_object& d)
{
return !(v == d);
}
inline bool operator!=(const definite_object& d, const vertex& v)
{
return !(v == d);
}
inline bool operator==(const vertex& v, indefinite_object i)
{
if (const indefinite_object*
vi = boost::get<indefinite_object>(&v))
return (*vi == i);
return false;
}
inline bool operator==(indefinite_object i, const vertex& v)
{
return (v == i);
}
inline bool operator!=(const vertex& v, indefinite_object i)
{
return !(v == i);
}
inline bool operator!=(indefinite_object i, const vertex& v)
{
return !(v == i);
}
inline bool operator==(const vertex& v, const enum_t& m)
{
if (const enum_t* vm = boost::get<enum_t>(&v))
return (*vm == m);
return false;
}
inline bool operator==(const enum_t& m, const vertex& v)
{
return (v == m);
}
inline bool operator!=(const vertex& v, const enum_t& m)
{
return !(v == m);
}
inline bool operator!=(const enum_t& m, const vertex& v)
{
return !(v == m);
}
inline bool operator==(const vertex& v, const message& m)
{
if (const message* vm = boost::get<message>(&v))
return (*vm == m);
return false;
}
inline bool operator==(const message& m, const vertex& v)
{
return (v == m);
}
inline bool operator!=(const vertex& v, const message& m)
{
return !(v == m);
}
inline bool operator!=(const message& m, const vertex& v)
{
return !(v == m);
}
inline bool operator==(const vertex& v, action_symbol i)
{
if (const action_symbol*
vi = boost::get<action_symbol>(&v))
return (*vi == i);
return false;
}
inline bool operator==(action_symbol i, const vertex& v)
{
return (v == i);
}
inline bool operator!=(const vertex& v, action_symbol i)
{
return !(v == i);
}
inline bool operator!=(action_symbol i, const vertex& v)
{
return !(v == i);
}
#if BOOST_VERSION < 105800
inline bool operator!=(const vertex& v1, const vertex& v2)
{
return !(v1 == v2);
}
#endif
inline size_t hash_value(const message& m) noexcept
{
return boost::hash_value(m.getContent());
}
inline size_t hash_value(const vertex& v) noexcept
{
using boost::hash_combine;
static const size_t c1 = size_t(id::builtin_count);
#define MAX_TREE_ARGS 256000
static const size_t c2 = c1 + MAX_TREE_ARGS;
static const size_t c3 = c2 + size_t(id::action_count);
static const size_t c_last = c3;
if (const builtin* h = boost::get<builtin>(&v))
return size_t(*h);
if (const wild_card* w = boost::get<wild_card>(&v))
return size_t(*w);
if (const argument* a = boost::get<argument>(&v))
return size_t(a->idx * (a->is_negated() + 2)) + c1;
if (const contin_t* c = boost::get<contin_t>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(*c));
return tmp;
}
if (const enum_t* m = boost::get<enum_t>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(m->getContent()));
return tmp;
}
if (const action* a = boost::get<action>(&v))
return size_t(*a) + c2;
if (const builtin_action* b = boost::get<builtin_action>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(*b));
return tmp;
}
if (const perception* p = boost::get<perception>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(*p));
return tmp;
}
if (const definite_object* d = boost::get<definite_object>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(*d));
return tmp;
}
if (const indefinite_object* i = boost::get<indefinite_object>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(*i));
return tmp;
}
if (const message* m = boost::get<message>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(m->getContent()));
return tmp;
}
if (const procedure_call* pc = boost::get<procedure_call>(&v)) {
size_t tmp = c_last;
std::cout << pc << std::endl;
return tmp;
}
if (const action_symbol* as = boost::get<action_symbol>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(*as));
return tmp;
}
if (const ann_type* a = boost::get<ann_type>(&v)) {
size_t tmp = c_last;
hash_combine(tmp, boost::hash_value(a->idx));
return tmp;
}
OC_ASSERT(false, "A case is missing");
return 0;
}
typedef tree<vertex> combo_tree;
typedef std::vector<combo_tree> combo_tree_seq;
typedef combo_tree_seq::iterator combo_tree_seq_it;
typedef combo_tree_seq::const_iterator combo_tree_seq_const_it;
typedef std::set<combo_tree, size_tree_order<vertex> > combo_tree_ns_set;
typedef combo_tree_ns_set::iterator combo_tree_ns_set_it;
typedef combo_tree_ns_set::const_iterator combo_tree_ns_set_const_it;
bool operator<(const combo_tree& lt, const combo_tree& rt);
template<typename T>
inline bool is_associative(const T& v)
{
return (v == id::logical_and || v == id::logical_or ||
v == id::plus || v == id::times ||
v == id::sequential_and || v == id::sequential_or ||
v == id::sequential_exec);
}
template<typename T>
inline bool is_commutative(const T& v)
{
return (v == id::logical_and || v == id::logical_or ||
v == id::plus || v == id::times
|| is_symmetric(v));
}
template<typename T>
inline bool is_ultrametric(const T& v)
{
if (is_perception(v))
return get_perception(v)->is_ultrametric();
else return false;
}
template<typename T>
inline bool is_transitive(const T& v)
{
if (is_perception(v))
return get_perception(v)->is_transitive();
else return false;
}
template<typename T>
inline bool is_reflexive(const T& v)
{
if (is_perception(v))
return get_perception(v)->is_reflexive();
else return false;
}
template<typename T>
inline bool is_irreflexive(const T& v)
{
if (is_perception(v))
return get_perception(v)->is_irreflexive();
else return false;
}
template<typename T>
inline bool is_symmetric(const T& v)
{
if (is_perception(v))
return get_perception(v)->is_symmetric();
else return false;
}
template<typename T>
inline bool is_identity_of_indiscernibles(const T& v)
{
if (is_perception(v))
return get_perception(v)->is_identity_of_indiscernibles();
else return false;
}
bool is_procedure_call(const vertex& v);
const procedure_call& get_procedure_call(const vertex& v);
bool is_action_symbol(const vertex& v);
const action_symbol& get_action_symbol(const vertex& v);
bool is_indefinite_object(const vertex& v);
const indefinite_object& get_indefinite_object(const vertex& v);
bool is_message(const vertex& v);
const message& get_message(const vertex& v);
bool is_enum_type(const vertex& v);
const enum_t& get_enum_type(const vertex& v);
bool is_builtin(const vertex& v);
builtin get_builtin(const vertex& v);
bool is_wild_card(const vertex& v);
wild_card get_wild_card(const vertex& v);
bool is_contin(const vertex& v);
contin_t get_contin(const vertex& v);
bool is_argument(const vertex& v);
argument& get_argument(vertex& v);
bool is_ann_type(const vertex& v);
ann_type& get_ann_type(vertex& v);
const argument& get_argument(const vertex& v);
bool is_negated(vertex& v);
bool is_action(const vertex& v);
action get_action(const vertex& v);
bool is_builtin_action(const vertex& v);
builtin_action get_builtin_action(const vertex& v);
bool is_action_result(const vertex& v);
bool is_perception(const vertex& v);
perception get_perception(const vertex& v);
bool is_definite_object(const vertex& v);
definite_object get_definite_object(const vertex& v);
inline builtin bool_to_builtin(bool b)
{
return (b ? id::logical_true : id::logical_false);
}
inline vertex bool_to_vertex(bool b)
{
return bool_to_builtin(b);
}
inline bool vertex_to_bool(const vertex& v)
{
OC_ASSERT(v == id::logical_true || v == id::logical_false,
"vertex should be 'id::logical_true' or 'id::logical_false'.");
return (v == id::logical_true);
}
inline bool builtin_to_bool(const builtin& b)
{
OC_ASSERT(b == id::logical_true || b == id::logical_false,
"builtin should be 'id::logical_true' or 'id::logical_false'.");
return (b == id::logical_true);
}
vertex negate_vertex(const vertex& v);
builtin negate_builtin(builtin b);
inline bool is_complement(const vertex& x, const vertex& y)
{
if (const argument* ax = boost::get<argument>(&x)) {
if (const argument* ay = boost::get<argument>(&y)) {
return (ax->idx == -ay->idx);
}
}
return false;
}
template<typename T> bool is_boolean(const T& v)
{
return (v == id::logical_true || v == id::logical_false);
}
contin_t cast_contin(const vertex& v);
template<typename T>
inline bool is_logical_operator(const T& v)
{
return (v == id::logical_and || v == id::logical_or || v == id::logical_not);
}
vertex swap_and_or(const vertex& v);
template<typename T>
inline bool is_constant(const T& v)
{
return (is_boolean(v) || is_contin(v)
|| is_enum_type(v) || is_action_result(v));
}
template<typename T>
inline bool is_contin_expr(const T& v)
{
return (is_contin(v) ||
(v == id::div) ||
(v == id::exp) ||
(v == id::impulse) ||
(v == id::log) ||
(v == id::plus) ||
(v == id::rand) ||
(v == id::sin) ||
(v == id::times));
}
inline bool is_predicate(const combo_tree::iterator& it)
{
if (*it == id::greater_than_zero) return true;
if ((*it == id::logical_not) &&
(*it.begin() == id::greater_than_zero)) return true;
return false;
}
void copy_without_null_vertices(combo_tree::iterator src,
combo_tree& dst_tr, combo_tree::iterator dst);
inline bool may_have_side_effects(combo_tree::iterator )
{
return false;
}
}
}
namespace std
{
template<>
struct hash<opencog::combo::vertex>
{
size_t operator()(opencog::combo::vertex v) const noexcept
{
return opencog::combo::hash_value(v);
}
};
template<>
struct hash<opencog::combo::combo_tree>
{
size_t operator()(const opencog::combo::combo_tree& tre) const noexcept
{
size_t hsh = 0;
for (const opencog::combo::vertex& v: tre)
hsh ^= std::hash<opencog::combo::vertex>{}(v)
+ 0x9e3779b9 + (hsh << 6) + (hsh >> 2);
return hsh;
}
};
}
#endif