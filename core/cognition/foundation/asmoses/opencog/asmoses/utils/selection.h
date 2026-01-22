#ifndef _OPENCOG_SELECTION_H
#define _OPENCOG_SELECTION_H
#include <opencog/asmoses/utils/functional.h>
#include <opencog/util/numeric.h>
#include <iterator>
#include <opencog/util/dorepeat.h>
#include <opencog/util/RandGen.h>
#include <opencog/util/mt19937ar.h>
namespace opencog
{
struct tournament_selection
{
tournament_selection(unsigned int t_size_, RandGen& _rng = randGen())
: t_size(t_size_), rng(_rng)
{
OC_ASSERT(t_size > 0);
}
unsigned int t_size;
RandGen& rng;
template<typename In, typename Out>
void operator()(In from, In to, Out dst, unsigned int n_select) const
{
typename std::iterator_traits<In>::difference_type d =
distance(from, to);
dorepeat (n_select) {
In res = from + rng.randint(d);
dorepeat (t_size - 1) {
In tmp = from + rng.randint(d);
if (*res < *tmp)
res = tmp;
}
*dst++ = *res;
}
}
};
template<typename It, typename ScoreT>
It roulette_select(It from, It to, ScoreT sum, RandGen& rng = randGen())
{
sum = ScoreT(double(sum) * rng.randdouble());
do {
sum -= *from++;
} while ((sum > 0) && (from != to));
return --from;
}
template<typename It>
It roulette_select(It from, It to, RandGen& rng = randGen())
{
typedef typename std::iterator_traits<It>::value_type score_type;
return roulette_select(from, to,
std::accumulate(from, to, score_type(0)),
rng);
}
template<typename NodeT>
class NodeSelector
{
public:
typedef NodeT value_type;
typedef std::vector<std::pair<NodeT, int> > PSeq;
NodeSelector(RandGen& _rng = randGen()) : rng(_rng) {
}
NodeT select(int arity) const {
return roulette_select
(boost::make_transform_iterator
(_byArity[arity].begin(), select2nd<NodeT, int>),
boost::make_transform_iterator
(_byArity[arity].end(), select2nd<NodeT, int>),
_aritySums[arity], rng).base()->first;
}
int select_arity(int from) const {
return distance(_aritySums.begin(),
roulette_select(_aritySums.begin() + from,
_aritySums.end(), rng));
}
void add(const NodeT& n, int arity, int prob) {
if ((int)_byArity.size() <= arity) {
_byArity.resize(arity + 1);
_aritySums.resize(arity + 1, 0);
}
_byArity[arity].push_back(make_pair(n, prob));
_aritySums[arity] += prob;
}
private:
RandGen& rng;
std::vector<PSeq> _byArity;
std::vector<int> _aritySums;
};
}
#endif