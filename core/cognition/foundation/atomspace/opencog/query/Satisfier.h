#ifndef _OPENCOG_SATISFIER_H
#define _OPENCOG_SATISFIER_H
#include <vector>
#include <opencog/atoms/value/ContainerValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/query/ContinuationMixin.h>
namespace opencog {
class Satisfier :
public ContinuationMixin
{
protected:
Handle _pattern_body;
bool _have_variables;
public:
Satisfier(AtomSpace* as) :
ContinuationMixin(as),
_result(false) {}
DECLARE_PE_MUTEX;
HandleSeq _varseq;
Handle _ground;
bool _result;
virtual void set_pattern(const Variables& vars,
const Pattern& pat)
{
_varseq = vars.varseq;
ContinuationMixin::set_pattern(vars, pat);
_have_variables = not vars.varseq.empty();
_pattern_body = pat.body;
}
virtual bool propose_grounding(const GroundingMap &var_soln,
const GroundingMap &term_soln);
virtual bool search_finished(bool);
};
class SatisfyingSet :
public ContinuationMixin
{
protected:
AtomSpace* _as;
DECLARE_PE_MUTEX;
PatternLinkPtr _plp;
HandleSeq _varseq;
ContainerValuePtr _result_queue;
std::map<Handle, ContainerValuePtr> _var_marginals;
void setup_marginals(void);
ValuePtr wrap_result(const GroundingMap &var_soln);
size_t _num_results;
std::map<GroundingMap, ValueSet> _groups;
public:
SatisfyingSet(AtomSpace* as, const ContainerValuePtr& cvp) :
ContinuationMixin(as),
_as(as), _result_queue(cvp),
_num_results(0), max_results(SIZE_MAX) {}
size_t max_results;
virtual void set_pattern(const Variables& vars,
const Pattern& pat)
{
_varseq = vars.varseq;
ContinuationMixin::set_pattern(vars, pat);
setup_marginals();
}
virtual bool satisfy(const PatternLinkPtr& plp) {
_plp = plp;
return ContinuationMixin::satisfy(plp);
}
virtual bool propose_grounding(const GroundingMap &var_soln,
const GroundingMap &term_soln);
virtual bool propose_grouping(const GroundingMap &var_soln,
const GroundingMap &term_soln,
const GroundingMap &group);
virtual bool start_search(void);
virtual bool search_finished(bool);
};
};
#endif