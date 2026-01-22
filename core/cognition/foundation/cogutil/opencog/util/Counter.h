#ifndef _OPENCOG_COUNTER_H
#define _OPENCOG_COUNTER_H
#include <initializer_list>
#include <map>
#include <numeric>
#include <ostream>
#include <set>
namespace opencog {
template<typename T, typename CT, typename CMP = std::less<T>>
class Counter : public std::map<T, CT, CMP>
{
protected:
template<typename IT>
void init(IT from, IT to) {
while(from != to) {
this->operator[](*from) += 1;
++from;
}
}
public:
typedef std::map<T, CT, CMP> super;
typedef typename super::value_type value_type;
Counter() {}
template<typename IT>
Counter(IT from, IT to)
{
init(from, to);
}
template<typename Container>
Counter(const Container& c)
{
init(c.begin(), c.end());
}
Counter(const std::initializer_list<value_type>& il)
{
for(const auto& v : il)
this->operator[](v.first) = v.second;
}
CT get(const T& key, CT c = CT()) const
{
typename super::const_iterator it = this->find(key);
return it == this->cend()? c : it->second;
}
CT total_count() const
{
CT sum = CT(0);
for (const auto& pair : *this) {
sum += pair.second;
}
return sum;
}
T mode() const
{
T key = super::begin()->first;
CT cnt = super::begin()->second;
for (const auto& v : *this) {
if (cnt < v.second)
key = v.first;
}
return key;
}
Counter& operator+=(const Counter& other) {
for (const auto& v : other)
this->operator[](v.first) += v.second;
return *this;
}
Counter& operator-=(const Counter& other) {
for (const auto& v : other)
this->operator[](v.first) -= v.second;
return *this;
}
Counter& operator*=(const Counter& other) {
auto it = this->begin();
auto other_it = other.begin();
while (it != this->end() and other_it != other.end()) {
if (this->key_comp()(it->first, other_it->first)) {
it->second = CT();
++it;
} else if (this->key_comp()(other_it->first, it->first)) {
this->emplace_hint(it, other_it->first, CT());
++other_it;
} else {
it->second *= other_it->second;
++it;
++other_it;
}
}
for (; it != this->end(); ++it)
it->second = CT();
for (; other_it != other.end(); ++other_it)
this->emplace_hint(it, other_it->first, CT());
return *this;
}
Counter& operator/=(const Counter& other) {
for (const auto& v : other)
this->operator[](v.first) /= v.second;
return *this;
}
Counter& operator+=(const CT& num) {
for (auto& v : *this)
v.second += num;
return *this;
}
Counter& operator-=(const CT& num) {
for (auto& v : *this)
v.second -= num;
return *this;
}
Counter& operator*=(const CT& num) {
for (auto& v : *this)
v.second *= num;
return *this;
}
Counter& operator/=(const CT& num) {
for (auto& v : *this)
v.second /= num;
return *this;
}
std::set<T> keys() const {
std::set<T> ks;
for (auto& v : *this)
ks.insert(v.first);
return ks;
}
Counter operator+(const Counter& other) const {
Counter result(*this);
result += other;
return result;
}
Counter operator-(const Counter& other) const {
Counter result(*this);
result -= other;
return result;
}
Counter operator*(const Counter& other) const {
Counter result(*this);
result *= other;
return result;
}
Counter operator/(const Counter& other) const {
Counter result(*this);
result /= other;
return result;
}
Counter operator+(const CT& num) const {
Counter result(*this);
result += num;
return result;
}
Counter operator-(const CT& num) const {
Counter result(*this);
result -= num;
return result;
}
Counter operator*(const CT& num) const {
Counter result(*this);
result *= num;
return result;
}
Counter operator/(const CT& num) const {
Counter result(*this);
result /= num;
return result;
}
};
template<typename T, typename CT, typename CMP = std::less<T>>
std::ostream& operator<<(std::ostream& out, const Counter<T, CT, CMP>& c)
{
typedef Counter<T, CT, CMP> counter_t;
out << "{";
for (typename counter_t::const_iterator it = c.begin(); it != c.end();) {
out << it->first << ": " << it->second;
++it;
if(it != c.end())
out << ", ";
}
out << "}";
return out;
}
}
#endif