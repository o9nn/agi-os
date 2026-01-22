#ifndef _OPENCOG_ENSEMBLE_PARAMS_H
#define _OPENCOG_ENSEMBLE_PARAMS_H
namespace opencog {
namespace moses {
struct ensemble_parameters
{
ensemble_parameters() :
do_boosting(false),
experts(false),
exact_experts(true),
expalpha(2.0),
bias_scale(1.0),
num_to_promote(1)
{}
bool do_boosting;
bool experts;
bool exact_experts;
double expalpha;
double bias_scale;
int num_to_promote;
};
}};
#endif