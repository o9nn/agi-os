#ifndef _OPENCOG_RANKING_H
#define _OPENCOG_RANKING_H
#include <opencog/util/Counter.h>
namespace opencog {
template<typename Key, typename FloatT>
Counter<Key, FloatT> ranking(const Counter<Key, FloatT>& counter) {
Counter<Key, FloatT> res;
FloatT lrank = 1;
for (const auto& v : counter) {
res.insert(res.end(), {v.first, (2*lrank + v.second - 1) / 2});
lrank += v.second;
}
return res;
}
}
#endif