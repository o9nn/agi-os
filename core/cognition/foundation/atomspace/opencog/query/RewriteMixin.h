#ifndef _OPENCOG_REWRITE_MIXIN_H
#define _OPENCOG_REWRITE_MIXIN_H
#include <vector>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/execution/Instantiator.h>
#include <opencog/atoms/value/ContainerValue.h>
#include <opencog/query/PatternMatchCallback.h>
namespace opencog {
class RewriteMixin :
public virtual PatternMatchCallback
{
protected:
AtomSpace* _as;
DECLARE_PE_MUTEX;
ValueSet _result_set;
ContainerValuePtr _result_queue;
void insert_result(ValuePtr);
PatternLinkPtr _plp;
HandleSeq _varseq;
HandleSeq _implicand;
std::map<Handle, ContainerValuePtr> _var_marginals;
std::map<Handle, ContainerValuePtr> _implicand_grnds;
void setup_marginals(void);
void set_plp(const PatternLinkPtr& plp)
{
_plp = plp;
_implicand = _plp->get_implicand();
}
void record_marginals(const GroundingMap&);
size_t _num_results;
std::map<GroundingMap, ValueSet> _groups;
std::map<GroundingMap, size_t> _group_sizes;
Instantiator inst;
public:
RewriteMixin(AtomSpace*, ContainerValuePtr&);
size_t max_results;
virtual void set_pattern(const Variables& vars,
const Pattern& pat)
{
_varseq = vars.varseq;
setup_marginals();
}
virtual bool propose_grounding(const GroundingMap &var_soln,
const GroundingMap &term_soln);
virtual bool propose_grouping(const GroundingMap &var_soln,
const GroundingMap &term_soln,
const GroundingMap &grouping);
virtual bool start_search(void);
virtual bool search_finished(bool);
};
};
#endif