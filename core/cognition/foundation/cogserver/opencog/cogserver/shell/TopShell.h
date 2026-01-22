#ifndef _OPENCOG_TOP_SHELL_H
#define _OPENCOG_TOP_SHELL_H
#include <string>
#include <opencog/network/GenericShell.h>
#include <opencog/cogserver/server/CogServer.h>
namespace opencog {
class TopEval;
class TopShell : public GenericShell
{
private:
CogServer& _shellserver;
TopEval* _top_eval;
double _refresh;
protected:
virtual void user_interrupt();
virtual void line_discipline(const std::string&);
public:
TopShell(CogServer&);
virtual ~TopShell();
virtual GenericEval* get_evaluator(void);
void set_interval(double);
};
}
#endif