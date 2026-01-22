#ifndef _OPENCOG_BETADISTRIBUTION_H_
#define _OPENCOG_BETADISTRIBUTION_H_
#include <boost/math/distributions/beta.hpp>
#include <opencog/util/mt19937ar.h>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
namespace opencog
{
class BetaDistribution
{
public:
BetaDistribution(const TruthValuePtr& tv,
double prior_alpha=1.0, double prior_beta=1.0);
BetaDistribution(double pos_count, double count,
double prior_alpha=1.0, double prior_beta=1.0);
double operator()(RandGen& rng=randGen()) const;
double alpha() const;
double beta() const;
double mean() const;
double variance() const;
std::vector<double> cdf(int bins) const;
std::vector<double> pdf(int bins) const;
double pd(double x) const;
std::string cdf_csv(int bins) const;
std::string pdf_csv(int bins) const;
std::string to_string(const std::string& indent) const;
private:
boost::math::beta_distribution<double> _beta_distribution;
};
BetaDistribution mk_beta_distribution(const TruthValuePtr& tv);
TruthValuePtr mk_stv(double mean, double variance,
double prior_alpha=1.0, double prior_beta=1.0);
std::string oc_to_string(const BetaDistribution& bd,
const std::string& indent=empty_string);
}
#endif