#ifndef _OPENCOG_FS_SCORERS_OPTIM_H
#define _OPENCOG_FS_SCORERS_OPTIM_H
#include <opencog/util/numeric.h>
#include <opencog/asmoses/combo/combo/common_def.h>
#include <opencog/asmoses/moses/representation/field_set.h>
#include <opencog/asmoses/moses/representation/instance_scorer.h>
#include <opencog/asmoses/moses/moses/types.h>
namespace opencog {
using namespace moses;
using namespace combo;
std::set<arity_t> get_feature_set(const field_set& fields,
const instance& inst);
template<typename FSScorer>
struct deme_based_scorer : public iscorer_base
{
deme_based_scorer(const FSScorer& fs_scorer, const field_set& fields)
: _fs_scorer(fs_scorer), _fields(fields) {}
composite_score operator()(const instance& inst) const
{
std::set<arity_t> fs = get_feature_set(_fields, inst);
composite_score csc(_fs_scorer(fs), fs.size(), 0);
if (logger().is_fine_enabled()) {
logger().fine()
<< "moses_based_scorer - Evaluate instance: "
<< _fields.to_string(inst) << " " << csc;
}
return csc;
}
const FSScorer& _fs_scorer;
const field_set& _fields;
};
}
#endif