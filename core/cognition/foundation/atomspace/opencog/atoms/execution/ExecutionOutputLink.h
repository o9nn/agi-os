#ifndef _OPENCOG_EXECUTION_OUTPUT_LINK_H
#define _OPENCOG_EXECUTION_OUTPUT_LINK_H
#include <stdlib.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/core/FunctionLink.h>
namespace opencog
{
class ExecutionOutputLink : public FunctionLink
{
private:
static ValuePtr do_execute(AtomSpace*,
const Handle& schema,
const Handle& args,
bool silent=false);
ValuePtr execute_once(AtomSpace* as, bool silent=false);
protected:
void check_schema(const Handle& schema) const;
public:
ExecutionOutputLink(const HandleSeq&&, Type=EXECUTION_OUTPUT_LINK);
ExecutionOutputLink(const Handle& schema, const Handle& args);
ExecutionOutputLink(const ExecutionOutputLink&) = delete;
ExecutionOutputLink& operator=(const ExecutionOutputLink&) = delete;
virtual bool is_executable() const { return true; }
virtual ValuePtr execute(AtomSpace* as, bool silent=false);
Handle get_schema(void) const { return getOutgoingAtom(0); }
Handle get_args(void) const { return getOutgoingAtom(1); }
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ExecutionOutputLink)
#define createExecutionOutputLink CREATE_DECL(ExecutionOutputLink)
}
#endif