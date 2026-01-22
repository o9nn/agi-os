#ifndef _OPENCOG_RANDOM_H
#define _OPENCOG_RANDOM_H
#include <iomanip>
#include <sstream>
#include <limits>
#include <type_traits>
#include <opencog/util/dorepeat.h>
#include <opencog/util/RandGen.h>
#include <opencog/util/mt19937ar.h>
#include <opencog/util/numeric.h>
namespace opencog {
template<typename C>
const typename C::value_type& rand_element(const C& c, RandGen& rng=randGen())
{
OC_ASSERT(!c.empty());
return *std::next(c.begin(), rng.randint(c.size()));
}
template<typename C>
typename C::value_type& rand_element(C& c, RandGen& rng=randGen())
{
OC_ASSERT(!c.empty());
return *std::next(c.begin(), rng.randint(c.size()));
}
template<typename C, typename D>
const typename C::value_type& rand_element(const C& c, D& d, RandGen& rng=randGen())
{
OC_ASSERT(!c.empty());
return *std::next(c.begin(), d(rng));
}
template<typename C, typename D>
typename C::value_type& rand_element(C& c, D& d, RandGen& rng=randGen())
{
OC_ASSERT(!c.empty());
return *std::next(c.begin(), d(rng));
}
template<typename C>
typename C::value_type rand_element_erase(C& c, RandGen& rng=randGen())
{
OC_ASSERT(!c.empty());
auto it = std::next(c.begin(), rng.randint(c.size()));
typename C::value_type val = *it;
c.erase(it);
return val;
}
template<typename T>
T gaussian_rand(T mean, T std_dev, RandGen& rng=randGen())
{
double val = mean + std_dev *
std::sqrt(-2.0 * std::log(rng.randdouble_one_excluded())) *
std::cos(2.0 * M_PI * rng.randdouble_one_excluded());
if (std::is_integral<T>::value) {
if (val > static_cast<double>(std::numeric_limits<T>::max())) {
return std::numeric_limits<T>::max();
} else if (val < static_cast<double>(std::numeric_limits<T>::min())) {
return std::numeric_limits<T>::min();
}
}
return static_cast<T>(val);
}
static inline bool biased_randbool(float b, RandGen& rng=randGen())
{
return b > rng.randfloat();
}
static inline std::string randstr(const std::string& prefix=std::string(),
unsigned n=1, int base=16,
RandGen& rng=randGen())
{
std::stringstream ss;
ss << prefix << std::setbase(base);
dorepeat(n)
ss << rng.randint();
return ss.str();
}
}
#endif