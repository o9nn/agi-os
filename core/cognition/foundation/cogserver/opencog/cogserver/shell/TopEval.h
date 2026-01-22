#ifndef _OPENCOG_TOP_EVAL_H
#define _OPENCOG_TOP_EVAL_H
#include <condition_variable>
#include <mutex>
#include <string>
#include <opencog/eval/GenericEval.h>
#include <opencog/cogserver/server/CogServer.h>
namespace opencog {
class TopEval : public GenericEval
{
private:
CogServer& _cserver;
std::mutex _sleep_mtx;
std::condition_variable _sleeper;
double _refresh;
int _nlines;
bool _started;
bool _done;
std::string _msg;
TopEval(CogServer&);
public:
virtual ~TopEval();
virtual std::string get_name(void) const { return "TopEval"; }
virtual void begin_eval(void);
virtual void eval_expr(const std::string&);
virtual std::string poll_result(void);
virtual void interrupt(void);
void cmd();
void set_interval(double);
static TopEval* get_evaluator(CogServer&);
};
}
#endif