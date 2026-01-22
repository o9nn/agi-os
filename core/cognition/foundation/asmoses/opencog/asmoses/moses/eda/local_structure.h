#ifndef _EDA_LOCAL_STRUCTURE_H
#define _EDA_LOCAL_STRUCTURE_H
#include <vector>
#include <opencog/asmoses/utils/digraph.h>
#include <opencog/util/oc_assert.h>
#include <opencog/asmoses/utils/tree.h>
#include "opencog/asmoses/moses/representation/field_set.h"
#include <boost/bind/bind.hpp>
namespace opencog {
namespace moses {
using std::vector;
using boost::make_counting_iterator;
using boost::make_transform_iterator;
typedef vector<int> dtree_node;
typedef tree<dtree_node> dtree;
struct local_structure_model : public nullary_function<instance>,
public vector<dtree>
{
typedef vector<dtree> super;
template<typename It>
local_structure_model(const field_set& fields, It from, It to);
instance operator()() const;
void split(int, int, dtree::iterator);
protected:
typedef vector<const instance*> iptr_seq;
typedef iptr_seq::iterator iptr_iter;
size_t _instance_length;
vector<unsigned int> _ordering;
digraph _initial_deps;
field_set _fields;
bool is_uniform_on(iptr_iter l, iptr_iter u, int idx) const;
void rec_split_term(iptr_iter l, iptr_iter u, int src_idx, int idx,
dtree::iterator node, term_tree::iterator osrc);
void rec_split_contin(iptr_iter l, iptr_iter u,
int src_idx, int idx, dtree::iterator node);
void make_dtree(super::iterator, int);
void sample(dtree::iterator, disc_t&, const vector<disc_t>&) const;
};
struct univariate
{
typedef local_structure_model model_type;
template<typename It>
void operator()(const field_set&, It, It,
const local_structure_model&) const {}
};
struct local_structure_probs_learning
{
typedef local_structure_model model_type;
template<typename It>
void operator()(const field_set&, It, It, local_structure_model&) const;
protected:
template<typename It>
void rec_learn(const field_set&, It, It, int, dtree::iterator) const;
};
template<typename It>
local_structure_model::local_structure_model(const field_set& fs,
It from, It to) :
super(fs.raw_size()),
_instance_length(fs.packed_width()),
_ordering(make_counting_iterator(0), make_counting_iterator(int(size()))),
_initial_deps(size()),
_fields(fs)
{
super::iterator dtr = begin();
if (!_fields.contin().empty() || !_fields.term().empty())
{
iptr_seq iptrs(make_transform_iterator(from, addressof<const instance>),
make_transform_iterator(to, addressof<const instance>));
for (const field_set::term_spec& o : _fields.term())
{
int idx_base = distance(begin(), dtr);
make_dtree(dtr++, o.tr->begin().number_of_children() + 1);
for (field_set::width_t i = 1; i < o.depth; ++i, ++dtr)
{
make_dtree(dtr, 0);
_initial_deps.insert(idx_base + i - 1, idx_base + i);
rec_split_term(iptrs.begin(), iptrs.end(),
idx_base, idx_base + i,
dtr->begin(), o.tr->begin());
}
}
for (const field_set::contin_spec& c : _fields.contin())
{
int idx_base = distance(begin(), dtr);
make_dtree(dtr++, 3);
for (field_set::width_t i = 1;i < c.depth;++i, ++dtr)
{
make_dtree(dtr, 3);
_initial_deps.insert(idx_base + i - 1, idx_base + i);
rec_split_contin(iptrs.begin(), iptrs.end(),
idx_base + i - 1, idx_base + i, dtr->begin());
}
}
}
for (const field_set::disc_spec& d : _fields.disc_and_bit())
make_dtree(dtr++, d.multy);
randomized_topological_sort(_initial_deps, _ordering.begin());
}
template<typename It>
void local_structure_probs_learning::operator()(const field_set& fs,
It from, It to,
local_structure_model& dst) const
{
for_each(dst.begin(), dst.end(), make_counting_iterator(0),
boost::bind(&local_structure_probs_learning::rec_learn<It>,
this, std::ref(fs),
from, to, boost::placeholders::_2,
boost::bind(&dtree::begin, boost::placeholders::_1)));
}
template<typename It>
void local_structure_probs_learning::rec_learn(const field_set& fs,
It from, It to,
int idx, dtree::iterator dtr) const
{
if (dtr.is_childless())
{
while (from != to)
{
OC_ASSERT(fs.get_raw(*from, idx) < dtr->size() - 1);
++(*dtr)[fs.get_raw(*from++, idx)];
}
dtr->back() = accumulate(dtr->begin(), --(dtr->end()), 0);
}
else
{
int raw_arity = dtr.number_of_children();
vector<It> pivots(raw_arity + 1);
pivots.front() = from;
pivots.back() = to;
n_way_partition(from, to,
std::bind(&field_set::get_raw, &fs, std::placeholders::_1, dtr->front()),
raw_arity, ++pivots.begin());
for_each(pivots.begin(), --pivots.end(), ++pivots.begin(),
make_counting_iterator(dtr.begin()),
std::bind(&local_structure_probs_learning::rec_learn<It>, this,
std::ref(fs), std::placeholders::_1, std::placeholders::_2, idx, std::placeholders::_3));
}
}
}
}
#endif