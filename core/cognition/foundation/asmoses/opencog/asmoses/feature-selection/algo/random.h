#ifndef _OPENCOG_FEATURE_SELECTION_RANDOM_ALGO_H
#define _OPENCOG_FEATURE_SELECTION_RANDOM_ALGO_H
#include <opencog/util/random.h>
#include "opencog/asmoses/feature-selection/main/feature-selection.h"
namespace opencog {
feature_set_pop random_select_feature_sets(const CompressedTable& ctable,
const feature_selection_parameters& fs_params);
template<typename FeatureSet>
FeatureSet random_selection(const FeatureSet& features,
size_t num_desired)
{
FeatureSet final;
while (0 < num_desired) {
final.insert(rand_element(features));
num_desired --;
}
return final;
}
}
#endif