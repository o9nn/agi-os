#ifdef HAVE_CYTHON
#ifndef PYTHONSHELL_H
#define PYTHONSHELL_H
#include <opencog/network/GenericShell.h>
namespace opencog
{
class PythonShell: public GenericShell
{
private:
GenericEval* evaluator;
public:
PythonShell(void);
virtual ~PythonShell();
virtual GenericEval* get_evaluator(void);
virtual void eval(const std::string &);
};
}
#endif
#endif