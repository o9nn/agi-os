#ifndef _OPENCOG_ZIPF_H
#define _OPENCOG_ZIPF_H
#include <algorithm>
#include <cmath>
#include <random>
namespace opencog {
template<class IntType = unsigned long, class RealType = double>
class zipf_distribution
{
public:
typedef IntType result_type;
static_assert(std::numeric_limits<IntType>::is_integer, "");
static_assert(!std::numeric_limits<RealType>::is_integer, "");
zipf_distribution(const IntType n=std::numeric_limits<IntType>::max(),
const RealType s=1.0,
const RealType q=0.0)
: n(n)
, _s(s)
, _q(q)
, oms(1.0-s)
, spole(abs(oms) < epsilon)
, rvs(spole ? 0.0 : 1.0/oms)
, H_x1(H(1.5) - h(1.0))
, H_n(H(n + 0.5))
, cut(1.0 - H_inv(H(1.5) - h(1.0)))
, dist(H_x1, H_n)
{
if (-0.5 >= q)
throw std::runtime_error("Range error: Parameter q must be greater than -0.5!");
}
void reset() {}
IntType operator()(std::mt19937& rng)
{
while (true)
{
const RealType u = dist(rng);
const RealType x = H_inv(u);
const IntType  k = std::round(x);
if (k - x <= cut) return k;
if (u >= H(k + 0.5) - h(k))
return k;
}
}
RealType s() const { return _s; }
RealType q() const { return _q; }
result_type min() const { return 1; }
result_type max() const { return n; }
private:
IntType    n;
RealType   _s;
RealType   _q;
RealType   oms;
bool       spole;
RealType   rvs;
RealType   H_x1;
RealType   H_n;
RealType   cut;
std::uniform_real_distribution<RealType> dist;
static constexpr RealType epsilon = 2e-5;
static double
expxm1bx(const double x)
{
if (std::abs(x) > epsilon)
return std::expm1(x) / x;
return (1.0 + x/2.0 * (1.0 + x/3.0 * (1.0 + x/4.0)));
}
static RealType
log1pxbx(const RealType x)
{
if (std::abs(x) > epsilon)
return std::log1p(x) / x;
return 1.0 - x * ((1/2.0) - x * ((1/3.0) - x * (1/4.0)));
}
const RealType h(const RealType x)
{
return std::pow(x + _q, -_s);
}
const RealType H(const RealType x)
{
if (not spole)
return std::pow(x + _q, oms) / oms;
const RealType log_xpq = std::log(x + _q);
return log_xpq * expxm1bx(oms * log_xpq);
}
const RealType H_inv(const RealType y)
{
if (not spole)
return std::pow(y * oms, rvs) - _q;
return std::exp(y * log1pxbx(oms * y)) - _q;
}
};
template<class IntType = unsigned long, class RealType = double>
class zipf_table_distribution
{
public:
typedef IntType result_type;
static_assert(std::numeric_limits<IntType>::is_integer, "");
static_assert(!std::numeric_limits<RealType>::is_integer, "");
zipf_table_distribution(const IntType n,
const RealType s=1.0,
const RealType q=0.0) :
_n(init(n,s,q)),
_s(s),
_q(q),
_dist(_pdf.begin(), _pdf.end())
{}
void reset() {}
IntType operator()(std::mt19937& rng)
{
return _dist(rng);
}
RealType s() const { return _s; }
RealType q() const { return _q; }
result_type min() const { return 1; }
result_type max() const { return _n; }
private:
std::vector<RealType>               _pdf;
IntType                             _n;
RealType                            _s;
RealType                            _q;
std::discrete_distribution<IntType> _dist;
IntType init(const IntType n, const RealType s, const RealType q)
{
_pdf.reserve(n+1);
_pdf.emplace_back(0.0);
for (IntType i=1; i<=n; i++)
_pdf.emplace_back(std::pow(q + (double)i, -s));
return n;
}
};
}
#endif