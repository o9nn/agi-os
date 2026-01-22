#ifndef _OPENCOG_JACCARD_INDEX_H
#define _OPENCOG_JACCARD_INDEX_H
#include <opencog/util/algorithm.h>
namespace opencog {
template<typename Set>
float jaccard_index(const Set& s1, const Set& s2) {
return (float)set_intersection(s1, s2).size()
/ (float)set_union(s1, s2).size();
}
}
#endif