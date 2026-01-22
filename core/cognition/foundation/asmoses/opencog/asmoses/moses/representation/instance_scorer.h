#ifndef _INSTANCE_SCORER_H
#define _INSTANCE_SCORER_H
#include "instance.h"
#include "field_set.h"
#include "representation.h"
#include "opencog/asmoses/moses/scoring/behave_cscore.h"
#include <opencog/atomspace/AtomSpace.h>
#include "opencog/asmoses/combo/converter/combo_atomese.h"
namespace opencog
{
namespace moses
{
struct iscorer_base
{
typedef instance argument_type;
typedef composite_score result_type;
virtual composite_score operator()(const instance &) const = 0;
virtual ~iscorer_base() {}
};
struct distance_based_scorer : public iscorer_base
{
distance_based_scorer(const field_set &_fs,
const instance &_target_inst)
: fs(_fs), target_inst(_target_inst)
{}
composite_score operator()(const instance &inst) const;
protected:
const field_set &fs;
const instance &target_inst;
};
struct combo_based_scorer : public iscorer_base
{
combo_based_scorer(behave_cscore &cs,
representation &rep, bool reduce)
: _cscorer(cs), _rep(rep), _reduce(reduce)
{}
composite_score operator()(const instance &inst) const;
protected:
behave_cscore &_cscorer;
representation &_rep;
bool _reduce;
};
struct atomese_based_scorer : public iscorer_base
{
atomese_based_scorer(behave_cscore &cs, representation &rep, bool reduce,
ComboToAtomese& to_atomese,
const string_seq &labels={},
AtomSpacePtr as=nullptr)
: _as(as), _cscorer(cs), _rep(rep), _reduce(reduce), _labels(labels),
_to_atomese(to_atomese)
{}
composite_score operator()(const instance &inst) const;
protected:
AtomSpacePtr _as;
behave_cscore &_cscorer;
representation &_rep;
bool _reduce;
string_seq _labels;
ComboToAtomese &_to_atomese;
};
}
}
#endif