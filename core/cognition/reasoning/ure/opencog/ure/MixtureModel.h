#ifndef _OPENCOG_MIXTUREMODEL_H_
#define _OPENCOG_MIXTUREMODEL_H_
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/truthvalue/TruthValue.h>
namespace opencog
{
class MixtureModel
{
public:
HandleSet models;
double cpx_penalty;
double compressiveness;
double data_set_size;
MixtureModel(const HandleSet& models,
double cpx_penalty=1.0,
double compressiveness=0.0);
TruthValuePtr operator()() const;
TruthValuePtr weighted_average(const std::vector<TruthValuePtr>& tvs,
const std::vector<double>& weights) const;
double beta_factor(const Handle& model) const;
double prior_estimate(const Handle& model) const;
double kolmogorov_estimate(double remain_data_size) const;
double prior(double length) const;
private:
double infer_data_set_size() const;
};
}
#endif