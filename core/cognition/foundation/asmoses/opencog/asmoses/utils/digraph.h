#ifndef _OPENCOG_DIGRAPH_H
#define _OPENCOG_DIGRAPH_H
#include <queue>
#include <vector>
#include <set>
#include <opencog/util/algorithm.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/oc_assert.h>
#include <boost/iterator/counting_iterator.hpp>
namespace opencog
{
struct digraph {
typedef unsigned int size_type;
typedef size_type value_type;
typedef std::set<value_type> value_set;
digraph(size_type n) : _incoming(n), _outgoing(n) { }
void insert(value_type src, value_type dst) {
_incoming[dst].insert(src);
_outgoing[src].insert(dst);
}
void erase(value_type src, value_type dst) {
_incoming[dst].erase(src);
_outgoing[src].erase(dst);
}
size_type n_nodes() const {
return _incoming.size();
}
size_type n_edges() const {
return accumulate2d(_incoming.begin(), _incoming.end(), size_type(0));
}
bool empty() const {
return (n_edges() == 0);
}
const value_set& incoming(value_type x) const {
return _incoming[x];
}
const value_set& outgoing(value_type x) const {
return _outgoing[x];
}
protected:
std::vector<value_set> _incoming;
std::vector<value_set> _outgoing;
};
template<typename Out>
Out randomized_topological_sort(digraph g, Out out)
{
typedef digraph::value_type value_t;
std::vector<value_t>
nodes(boost::make_counting_iterator(digraph::size_type(0)),
boost::make_counting_iterator(g.n_nodes()));
std::random_shuffle(nodes.begin(), nodes.end());
std::queue<value_t> q;
for (value_t node : nodes)
if (g.incoming(node).empty())
q.push(node);
while (!q.empty()) {
value_t src = q.front();
q.pop();
*out++ = src;
std::vector<value_t> outgoing(g.outgoing(src).begin(),
g.outgoing(src).end());
for (value_t dst : outgoing) {
g.erase(src, dst);
if (g.incoming(dst).empty())
q.push(dst);
}
}
OC_ASSERT(g.empty(), "digraph - g must be a DAG.");
return out;
}
}
#endif