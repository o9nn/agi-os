#ifndef _OPENCOG_JSON_EVAL_H
#define _OPENCOG_JSON_EVAL_H
#include <mutex>
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/eval/GenericEval.h>
namespace opencog {
class AtomSpace;
class JsonEval : public GenericEval
{
private:
AtomSpacePtr _atomspace;
std::mutex _mtx;
std::string _answer;
JsonEval(const AtomSpacePtr&);
public:
virtual ~JsonEval();
virtual std::string get_name(void) const { return "JsonEval"; }
virtual void begin_eval(void);
virtual void eval_expr(const std::string&);
virtual std::string poll_result(void);
virtual void interrupt(void);
static JsonEval* get_evaluator(const AtomSpacePtr&);
};
}
#endif