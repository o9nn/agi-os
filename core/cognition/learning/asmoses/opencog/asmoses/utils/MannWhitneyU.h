#ifndef _OPENCOG_MANNWHITNEYU_H
#define _OPENCOG_MANNWHITNEYU_H
#include <boost/range/numeric.hpp>
#include <boost/range/adaptor/map.hpp>
#include <opencog/util/Counter.h>
#include <opencog/asmoses/utils/ranking.h>
namespace opencog {
using boost::adaptors::map_values;
template<typename Key, typename FloatT>
FloatT MannWhitneyU(const Counter<Key, FloatT>& c1,
const Counter<Key, FloatT>& c2,
FloatT n2 = -1) {
typedef Counter<Key, FloatT> counter_t;
counter_t c = c1 + c2;
counter_t r = ranking(c);
FloatT sum2 = 0;
for (const auto& v : c2)
sum2 += r[v.first];
if (n2 < 0)
n2 = boost::accumulate(c2 | map_values, 0);
return sum2 - n2*(n2+1)/2;
}
template<typename Key, typename FloatT>
FloatT standardizedMannWhitneyU(const Counter<Key, FloatT>& c1,
const Counter<Key, FloatT>& c2,
FloatT n1 = -1, FloatT n2 = -1) {
if(n1 < 0)
n1 = boost::accumulate(c1 | map_values, 0);
if(n2 < 0)
n2 = boost::accumulate(c2 | map_values, 0);
FloatT U = MannWhitneyU(c1, c2, n2),
mU = n1*n2/2,
sU = sqrt(mU*(n1+n2+1)/6);
return (U - mU) / sU;
}
}
#endif