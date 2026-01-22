#ifndef _OPENCOG_NUMERIC_H
#define _OPENCOG_NUMERIC_H
#include <algorithm>
#include <cmath>
#include <climits>
#include <cstdlib>
#include <limits>
#include <numeric>
#include <vector>
#include <opencog/util/oc_assert.h>
namespace opencog
{
#define PROB_EPSILON 1e-127
#define DISTANCE_EPSILON 1e-32
template<typename T>
struct absolute_value_order
{
bool operator()(T a,T b) const {
return (a == -b) ? a < b : std::abs(a) < std::abs(b);
}
};
inline unsigned int integer_log2(size_t v)
{
#ifdef __GNUC__
if (0 == v) return 0;
return (8*sizeof(size_t) - 1) - __builtin_clzl(v);
#else
static const int MultiplyDeBruijnBitPosition[32] = {
0, 1, 28, 2, 29, 14, 24, 3, 30, 22, 20, 15, 25, 17, 4, 8,
31, 27, 13, 23, 21, 19, 16, 7, 26, 12, 18, 6, 11, 5, 10, 9
};
v |= v >> 1;
v |= v >> 2;
v |= v >> 4;
v |= v >> 8;
v |= v >> 16;
v = (v >> 1) + 1;
return MultiplyDeBruijnBitPosition[static_cast<uint32_t>(v * 0x077CB531UL) >> 27];
#endif
}
inline size_t next_power_of_two(size_t x)
{
OC_ASSERT(x > 0);
#ifdef __GNUC__
if (1==x) return 1;
return 1UL << (8*sizeof(size_t) - __builtin_clzl(x-1));
#else
x--;
x |= x >> 1;
x |= x >> 2;
x |= x >> 4;
x |= x >> 8;
x |= x >> 16;
x++;
return x;
#endif
}
inline unsigned int nbits_to_pack(size_t multy)
{
OC_ASSERT(multy > 0);
#ifdef ALIGNED_NOT_ACTUALLY_REQUIRED
return integer_log2(multy -1) + 1;
#else
return next_power_of_two(integer_log2(multy -1) + 1);
#endif
}
template<typename FloatT> bool is_between(FloatT x, FloatT min_, FloatT max_)
{
return x >= min_ and x <= max_;
}
static inline bool is_approx_eq_ulp(double x, double y, int64_t max_ulps)
{
if ((x < 0) != (y < 0))
{
return x == y;
}
int64_t* xbits = reinterpret_cast<int64_t*>(&x);
int64_t* ybits = reinterpret_cast<int64_t*>(&y);
static_assert(sizeof(int64_t) == sizeof(double), "Unexpected sizeof(double)");
int64_t ulps = std::abs(*xbits - *ybits);
return max_ulps > ulps;
}
template<typename FloatT> bool is_within(FloatT x, FloatT y, FloatT epsilon)
{
return std::abs(x - y) <= epsilon;
}
template<typename FloatT> bool is_approx_eq(FloatT x, FloatT y, FloatT epsilon)
{
FloatT diff = std::fabs(x - y);
if (diff < epsilon) return true;
FloatT amp = std::fabs(x + y);
return diff <= epsilon * amp;
}
template<typename Float>
Float clamp(Float x, Float l, Float u)
{
return std::max(l, std::min(u, x));
}
template<typename FloatT> FloatT weighted_information(FloatT p)
{
return p > PROB_EPSILON? -p * std::log2(p) : 0;
}
template<typename FloatT> FloatT binary_entropy(FloatT p)
{
OC_ASSERT(p >= 0 and p <= 1,
"binaryEntropy: probability %f is not between 0 and 1", p);
return weighted_information(p) + weighted_information(1.0 - p);
}
template<typename It> double entropy(It from, It to)
{
double res = 0;
for(; from != to; ++from)
res += weighted_information(*from);
return res;
}
template<typename C>
double entropy(const C& c)
{
return entropy(c.begin(), c.end());
}
template<typename IntT> IntT smallest_divisor(IntT n)
{
OC_ASSERT(n > 0, "smallest_divisor: n must be superior than 0");
if(n<3)
return n;
else {
bool found_divisor = false;
IntT i = 2;
for(; i*i <= n and !found_divisor; i++) {
found_divisor = n%i==0;
}
if(found_divisor)
return i-1;
else return n;
}
}
template<typename T> T sq(T x) { return x*x; }
template<typename OutInt> OutInt pow2(unsigned int x)
{
OC_ASSERT(8*sizeof(OutInt) - (std::numeric_limits<OutInt>::is_signed?1:0) > x,
"pow2: Amount to shift is out of range.");
return static_cast<OutInt>(1) << x;
}
inline unsigned int pow2(unsigned int x) { return pow2<unsigned int>(x); }
template<typename It, typename Float>
Float generalized_mean(It from, It to, Float p = 1.0)
{
Float pow_sum =
std::accumulate(from, to, 0.0,
[&](Float l, Float r) { return l + pow(r, p); });
return pow(pow_sum / std::distance(from, to), 1.0 / p);
}
template<typename C, typename Float>
Float generalized_mean(const C& c, Float p = 1.0)
{
return generalized_mean(c.begin(), c.end(), p);
}
template<typename Vec, typename Float>
Float p_norm_distance(const Vec& a, const Vec& b, Float p=1.0)
{
OC_ASSERT (a.size() == b.size(),
"Cannot compare unequal-sized vectors!  %d %d\n",
a.size(), b.size());
typename Vec::const_iterator ia = a.begin(), ib = b.begin();
Float sum = 0.0;
if (1.0 == p) {
for (; ia != a.end(); ++ia, ++ib)
sum += fabs (*ia - *ib);
return sum;
}
if (2.0 == p) {
for (; ia != a.end(); ++ia, ++ib)
sum += sq (*ia - *ib);
return sqrt(sum);
}
if (0.0 >= p) {
for (; ia != a.end(); ++ia, ++ib) {
Float diff = fabs (*ia - *ib);
if (sum < diff) sum = diff;
}
return sum;
}
for (; ia != a.end(); ++ia, ++ib) {
Float diff = fabs (*ia - *ib);
if (0.0 < diff)
sum += pow(diff, p);
}
return pow(sum, 1.0/p);
}
template<typename Vec, typename Float>
Float tanimoto_distance(const Vec& a, const Vec& b)
{
OC_ASSERT (a.size() == b.size(),
"Cannot compare unequal-sized vectors!  %d %d\n",
a.size(), b.size());
Float ab = std::inner_product(a.begin(), a.end(), b.begin(), Float(0)),
aa = std::inner_product(a.begin(), a.end(), a.begin(), Float(0)),
bb = std::inner_product(b.begin(), b.end(), b.begin(), Float(0)),
numerator = aa + bb - ab;
if (numerator >= Float(DISTANCE_EPSILON))
return 1 - (ab / numerator);
else
return 0;
}
template<typename Vec, typename Float>
Float angular_distance(const Vec& a, const Vec& b, bool pos_n_neg = true)
{
OC_ASSERT (a.size() == b.size(),
"Cannot compare unequal-sized vectors!  %d %d\n",
a.size(), b.size());
Float ab = std::inner_product(a.begin(), a.end(), b.begin(), Float(0)),
aa = std::inner_product(a.begin(), a.end(), a.begin(), Float(0)),
bb = std::inner_product(b.begin(), b.end(), b.begin(), Float(0)),
numerator = sqrt(aa * bb);
if (numerator >= Float(DISTANCE_EPSILON)) {
Float r = clamp(ab / numerator, Float(-1), Float(1));
return (pos_n_neg ? 1 : 2) * acos(r) / M_PI;
}
else
return 0;
}
#undef PROB_EPSILON
#undef DISTANCE_EPSILON
}
#endif