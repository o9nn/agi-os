#ifndef _OPENCOG_ALGORITHM_H
#define _OPENCOG_ALGORITHM_H
#include <algorithm>
#include <functional>
#include <set>
#include <opencog/util/exceptions.h>
#include <opencog/util/numeric.h>
namespace opencog
{
template<typename It1, typename It2, typename F>
F for_each(It1 from1, It1 to1, It2 from2, F f)
{
for (;from1 != to1;++from1, ++from2)
f(*from1, *from2);
return f;
}
template<typename It1, typename It2, typename It3, typename F>
F for_each(It1 from1, It1 to1, It2 from2, It3 from3, F f)
{
for (;from1 != to1;++from1, ++from2, ++from3)
f(*from1, *from2, *from3);
return f;
}
template<typename It1, typename It2, typename It3, typename It4, typename F>
F for_each(It1 from1, It1 to1, It2 from2, It3 from3, It4 from4, F f)
{
for (;from1 != to1;++from1, ++from2, ++from3, ++from4)
f(*from1, *from2, *from3, *from4);
return f;
}
template <class iter, class T>
T accumulate2d(iter first, iter last, T init)
{
for ( ;first != last;++first)
init = std::accumulate(first->begin(), first->end(), init);
return init;
}
template<class T>
void append(T& a, const T& b) {
a.insert(a.end(), b.begin(), b.end());
}
template<typename Erase, typename It1, typename It2, typename Comp>
void erase_set_intersection(Erase erase, It1 from1, It1 to1,
It2 from2, It2 to2, Comp comp)
{
OC_ASSERT(std::is_sorted(from1, to1, comp),
"algorithm - from1 -> to1 aren't sorted (erase_set_intersection).");
OC_ASSERT(std::is_sorted(from2, to2, comp),
"algorithm - from2 -> to2 aren't sorted (erase_set_intersection).");
while (from1 != to1 && from2 != to2) {
if (comp(*from1, *from2)) {
++from1;
} else if (comp(*from2, *from1)) {
++from2;
} else {
erase(from1++);
++from2;
}
}
}
template<typename Erase, typename It1, typename It2, typename Comp>
void erase_set_difference(Erase erase, It1 from1, It1 to1,
It2 from2, It2 to2, Comp comp)
{
OC_ASSERT(std::is_sorted(from1, to1, comp),
"algorithm - from1 -> to1 aren't sorted (erase_set_difference).");
OC_ASSERT(std::is_sorted(from2, to2, comp),
"algorithm - from2 -> to2 aren't sorted (erase_set_difference).");
while (from1 != to1 && from2 != to2) {
if (comp(*from1, *from2)) {
erase(from1++);
} else if (comp(*from2, *from1)) {
++from2;
} else {
++from1;
++from2;
}
}
while (from1 != to1)
erase(from1++);
}
template<typename Insert, typename It1, typename It2, typename Comp>
void insert_set_complement(Insert insert, It1 from1, It1 to1,
It2 from2, It2 to2, Comp comp)
{
cassert(TRACE_INFO, std::is_sorted(from1, to1, comp),
"algorithm - from1 -> to1 aren't sorted (insert_set_complement).");
cassert(TRACE_INFO, std::is_sorted(from2, to2, comp),
"algorithm - from2 -> to2 aren't sorted (insert_set_complement).");
while (from1 != to1 && from2 != to2) {
if (comp(*from1, *from2)) {
++from1;
} else if (comp(*from2, *from1)) {
insert(from1, *from2);
++from2;
} else {
++from1;
++from2;
}
}
while (from2 != to2) {
insert(from1, *from2);
++from2;
}
}
template<typename It1, typename It2, typename Comp>
bool has_empty_intersection(It1 from1, It1 to1,
It2 from2, It2 to2, Comp comp)
{
cassert(TRACE_INFO, std::is_sorted(from1, to1, comp),
"algorithm - from1 -> to1 aren't sorted (has_empty_intersection).");
cassert(TRACE_INFO, std::is_sorted(from2, to2, comp),
"algorithm - from2 -> to2 aren't sorted (has_empty_intersection).");
while (from1 != to1 && from2 != to2) {
if (comp(*from1, *from2))
++from1;
else if (comp(*from2, *from1))
++from2;
else
return false;
}
return true;
}
template<typename Set>
bool has_empty_intersection(const Set& ls, const Set& rs) {
return has_empty_intersection(ls.begin(), ls.end(),
rs.begin(), rs.end(),
ls.key_comp());
}
template<class Set1, class Set2>
bool is_disjoint(const Set1 &set1, const Set2 &set2)
{
if (set1.empty() || set2.empty()) return true;
typename Set1::const_iterator
from1 = set1.begin(),
to1 = set1.end();
typename Set2::const_iterator
from2 = set2.begin(),
to2 = set2.end();
if (*set2.rbegin() < *from1 || *set1.rbegin() < *from2) return true;
while (from1 != to1 && from2 != to2)
{
if (*from1 == *from2)
return false;
if (*from1 < *from2)
++from1;
else
++from2;
}
return true;
}
template<typename Set>
Set make_singleton_set(const typename Set::value_type& v) {
Set ret;
ret.insert(v);
return ret;
}
template<typename Set>
void set_union_modify(Set& s1, const Set& s2) {
s1.insert(s2.begin(), s2.end());
}
template<typename Set>
Set set_union(const Set& s1, const Set& s2) {
Set res(s1);
set_union_modify(res, s2);
return res;
}
template<typename Set>
Set set_intersection(const Set& s1, const Set& s2) {
Set res;
std::set_intersection(s1.begin(), s1.end(), s2.begin(), s2.end(),
std::inserter(res, res.end()));
return res;
}
template<typename Set>
Set set_difference(const Set& s1, const Set& s2) {
Set res;
std::set_difference(s1.begin(), s1.end(), s2.begin(), s2.end(),
std::inserter(res, res.end()));
return res;
}
template<typename Set>
Set set_symmetric_difference(const Set& s1, const Set& s2) {
Set res;
std::set_symmetric_difference(s1.begin(), s1.end(), s2.begin(), s2.end(),
std::inserter(res, res.end()));
return res;
}
template<typename It, typename Pred, typename Out>
Out n_way_partition(It begin, It end, const Pred p, int n, Out out)
{
for (int i = 0;i < n - 1;++i)
*out++ = begin = std::partition(begin, end,
[&p, i](const auto& x) { return p(x) == i; });
return out;
}
template<typename Set> std::set<Set> powerset(const Set& s, size_t n,
bool exact=false)
{
std::set<Set> res;
if (n > 0) {
std::set<Set> ps = powerset(s, n-1, exact);
for (const Set& ss : ps)
for (const auto& el : s) {
Set subset(ss);
if (subset.find(el) == subset.end()) {
subset.insert(el);
res.insert(subset);
}
}
if (!exact)
res.insert(ps.begin(), ps.end());
} else
res.insert(Set());
return res;
}
template<typename Set> std::set<Set> powerset(const Set& s)
{
return powerset(s, s.size());
}
template<typename C>
std::set<std::vector<typename C::value_type>> cartesian_product(const C& c,
size_t nfold=2)
{
typedef typename C::value_type T;
if (nfold > 0) {
std::set<std::vector<T>> res;
std::set<std::vector<T>> cp = cartesian_product(c, nfold - 1);
for (const std::vector<T>& t : cp) {
for (const auto& el : c) {
std::vector<T> tel(t);
tel.push_back(el);
res.insert(tel);
}
}
return res;
}
return {{}};
}
template<typename Indices, typename Seq>
Seq seq_filtered(const Seq& seq, const Indices& indices)
{
Seq res;
for (const auto& idx : indices)
res.push_back(seq[idx]);
return res;
}
template<typename T>
bool contains(const typename std::set<T>& set,
const typename std::set<T>::value_type& el)
{
return set.find(el) != set.end();
}
template<typename Container>
bool contains(const Container& c, const typename Container::value_type& el)
{
return std::find(c.begin(), c.end(), el) != c.end();
}
template<typename C>
void clear_by_swap(C& c)
{
C empty;
c.swap(empty);
}
}
#endif