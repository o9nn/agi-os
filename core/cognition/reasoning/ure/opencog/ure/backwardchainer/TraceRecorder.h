#ifndef _OPENCOG_TRACERECORDER_H_
#define _OPENCOG_TRACERECORDER_H_
#include <opencog/atomspace/AtomSpace.h>
#include "BIT.h"
#include "../Rule.h"
namespace opencog
{
class TraceRecorder
{
public:
static const std::string target_predicate_name;
static const std::string andbit_predicate_name;
static const std::string expand_andbit_schema_name;
static const std::string proof_predicate_name;
TraceRecorder(AtomSpace* tr_as);
HandleSeqSet traces();
HandleSeqSet traces(const Handle& fcs);
void target(const Handle& target);
void andbit(const AndBIT& andbit);
void expansion(const Handle& andbit_fcs, const Handle& bitleaf_body,
const Rule& rule, const AndBIT& new_andbit);
void proof(const Handle& andbit_fcs, const Handle& target_result);
private:
AtomSpace* _trace_as;
Handle _target_predicate, _andbit_predicate, _expand_andbit_schema,
_proof_predicate;
Handle dont_exec(const Handle& h);
Handle add_execution(const Handle& schema,
const Handle& input, const Handle& output,
TruthValuePtr tv);
Handle add_execution(const Handle& schema,
const Handle& input1,
const Handle& input2,
const Handle& input3,
const Handle& output,
TruthValuePtr tv);
Handle add_evaluation(const Handle& predicate,
const Handle& argument,
TruthValuePtr tv);
Handle add_evaluation(const Handle& predicate,
const Handle& arg1, const Handle& arg2,
TruthValuePtr tv);
HandleSet get_expansion_sources(const Handle& fcs_target);
HandleSet get_fcs_proofs();
};
}
#endif