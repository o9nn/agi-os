#ifndef _OPENCOG_THOMPSON_SAMPLING_H_
#define _OPENCOG_THOMPSON_SAMPLING_H_
#include <opencog/util/mt19937ar.h>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
#include "BetaDistribution.h"
namespace opencog
{
class ThompsonSampling
{
public:
ThompsonSampling(const TruthValueSeq& tvs, unsigned bins=100);
std::vector<double> distribution() const;
size_t operator()(RandGen& rng=randGen()) const;
std::string to_string(const std::string& indent=empty_string) const;
private:
double Pi(size_t i, const std::vector<std::vector<double>>& cdfs) const;
const TruthValueSeq& _tvs;
unsigned _bins;
};
std::string	oc_to_string(const ThompsonSampling& asel,
const std::string& indent=empty_string);
}
#endif