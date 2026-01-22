#ifndef OPENCOG_UTIL_GEOMETRIC_MEAN_MIRROR
#define OPENCOG_UTIL_GEOMETRIC_MEAN_MIRROR
#include  <cmath>
#include <boost/mpl/placeholders.hpp>
#include <boost/accumulators/framework/accumulator_base.hpp>
#include <boost/accumulators/framework/extractor.hpp>
#include <boost/accumulators/numeric/functional.hpp>
#include <boost/accumulators/framework/parameters/sample.hpp>
#include <boost/accumulators/framework/parameters/weight.hpp>
#include <boost/accumulators/framework/accumulators/external_accumulator.hpp>
#include <boost/accumulators/framework/depends_on.hpp>
#include <boost/accumulators/statistics_fwd.hpp>
#include <boost/accumulators/statistics/count.hpp>
namespace boost { namespace accumulators
{
namespace impl
{
template<typename Sample, typename Tag>
struct geometric_mean_mirror_impl
: accumulator_base
{
typedef Sample result_type;
template<typename Args>
geometric_mean_mirror_impl(Args const &args)
: prod(1.0)
{
}
template<typename Args>
void operator ()(Args const &args)
{
this->prod *= (1.0 - args[parameter::keyword<Tag>::get()]);
}
template<typename Args>
result_type result(Args const &args) const
{
return 1 - std::pow(this->prod, 1.0 / count(args));
}
private:
Sample prod;
};
}
namespace tag
{
struct geometric_mean_mirror
: depends_on<count>
{
typedef accumulators::impl::geometric_mean_mirror_impl<mpl::_1, tag::sample> impl;
};
}
namespace extract
{
extractor<tag::geometric_mean_mirror> const geometric_mean_mirror = {};
BOOST_ACCUMULATORS_IGNORE_GLOBAL(geometric_mean_mirror)
}
using extract::geometric_mean_mirror;
}}
#endif