#ifndef _OPENCOG_SEXPR_EVAL_H
#define _OPENCOG_SEXPR_EVAL_H
#include <mutex>
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/eval/GenericEval.h>
#include <opencog/persist/sexcom/Dispatcher.h>
namespace opencog {
class SexprEval : public GenericEval
{
friend class SexprShell;
private:
AtomSpacePtr _atomspace;
static Dispatcher _interpreter;
std::mutex _mtx;
std::string _answer;
SexprEval(const AtomSpacePtr&);
public:
virtual ~SexprEval();
virtual std::string get_name(void) const { return "SexprEval"; }
virtual void begin_eval(void);
virtual void eval_expr(const std::string&);
virtual std::string poll_result(void);
virtual void interrupt(void);
static SexprEval* get_evaluator(const AtomSpacePtr&);
static SexprEval* get_evaluator(AtomSpace* as) {
AtomSpacePtr asp(AtomSpaceCast(as));
return get_evaluator(asp); }
void install_handler(const std::string& cmd, Dispatcher::Meth impl) {
_interpreter.install_handler(cmd, impl);
}
};
}
#endif