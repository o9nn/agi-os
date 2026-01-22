#ifndef _OPENCOG_BACKWARDCHAINER_H_
#define _OPENCOG_BACKWARDCHAINER_H_
#include "../Rule.h"
#include "../UREConfig.h"
#include "BIT.h"
#include "TraceRecorder.h"
#include "ControlPolicy.h"
class BackwardChainerUTest;
namespace opencog
{
class BackwardChainer
{
friend class ::BackwardChainerUTest;
public:
BackwardChainer(AtomSpace& kb_as,
AtomSpace& rb_as,
const Handle& rbs,
const Handle& target,
const Handle& vardecl=Handle::UNDEFINED,
AtomSpace* trace_as=nullptr,
AtomSpace* control_as=nullptr,
const Handle& focus_set=Handle::UNDEFINED,
const BITNodeFitness& bitnode_fitness=BITNodeFitness(),
const AndBITFitness& andbit_fitness=AndBITFitness());
BackwardChainer(AtomSpace& kb_as,
const Handle& rbs,
const Handle& target,
const Handle& vardecl=Handle::UNDEFINED,
AtomSpace* trace_as=nullptr,
AtomSpace* control_as=nullptr,
const Handle& focus_set=Handle::UNDEFINED,
const BITNodeFitness& bitnode_fitness=BITNodeFitness(),
const AndBITFitness& andbit_fitness=AndBITFitness());
UREConfig& get_config();
const UREConfig& get_config() const;
void do_chain();
void do_step();
bool termination();
Handle get_results() const;
const HandleSet& get_results_set() const;
private:
void expand_meta_rules();
void expand_bit();
void expand_bit(AndBIT& andbit);
void fulfill_bit();
void fulfill_fcs(const Handle& fcs);
void reduce_bit();
void remove_unlikely_expandable_andbit();
std::vector<double> expansion_andbit_weights();
AndBIT* select_expansion_andbit();
const AndBIT* select_fulfillment_andbit() const;
double complexity_factor(const AndBIT& andbit) const;
double operator()(const AndBIT& andbit) const;
AtomSpace& _kb_as;
AtomSpace& _rb_as;
UREConfig _config;
BIT _bit;
AndBITFitness _andbit_fitness;
TraceRecorder _trace_recorder;
ControlPolicy _control;
RuleSet& _rules;
int _iteration;
const AndBIT* _last_expansion_andbit;
HandleSet _results;
};
}
#endif