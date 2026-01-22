#ifndef _INSTANCE_SCORER_H
#define _INSTANCE_SCORER_H
#include "instance.h"
#include "field_set.h"
#include "representation.h"
#include "../scoring/behave_cscore.h"
namespace opencog { namespace moses {
struct iscorer_base
{
typedef instance argument_type;
typedef composite_score result_type;
virtual composite_score operator()(const instance&) const = 0;
virtual ~iscorer_base() {}
};
struct distance_based_scorer : public iscorer_base
{
distance_based_scorer(const field_set& _fs,
const instance& _target_inst)
: fs(_fs), target_inst(_target_inst) {}
composite_score operator()(const instance& inst) const
{
score_t sc = -fs.hamming_distance(target_inst, inst);
if (logger().is_fine_enabled()) {
logger().fine() << "distance_based_scorer - Evaluate instance: "
<< fs.to_string(inst) << "\n"
<< "Score = " << sc << std::endl;
}
return composite_score(sc, 0, 0, 0);
}
protected:
const field_set& fs;
const instance& target_inst;
};
struct complexity_based_scorer : public iscorer_base
{
complexity_based_scorer(behave_cscore& cs,
representation& rep, bool reduce)
: _cscorer(cs), _rep(rep), _reduce(reduce) {}
composite_score operator()(const instance& inst) const
{
if (logger().is_fine_enabled()) {
logger().fine() << "complexity_based_scorer - Evaluate instance: "
<< _rep.fields().to_string(inst);
}
try {
combo_tree tr = _rep.get_candidate(inst, _reduce);
return _cscorer.get_cscore(tr);
} catch (...) {
combo_tree raw_tr = _rep.get_candidate(inst, false);
combo_tree red_tr = _rep.get_candidate(inst, true);
logger().warn() << "The following instance could not be evaluated: "
<< _rep.fields().to_string(inst)
<< "\nUnreduced tree: " << raw_tr
<< "\nreduced tree: "<< red_tr;
}
return worst_composite_score;
}
protected:
behave_cscore& _cscorer;
representation& _rep;
bool _reduce;
};
}
}
#endif