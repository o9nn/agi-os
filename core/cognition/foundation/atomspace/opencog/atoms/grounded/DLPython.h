#ifndef _OPENCOG_DL_PYTHON_H
#define _OPENCOG_DL_PYTHON_H
namespace opencog
{
class AtomSpace;
class PythonEval;
PythonEval* get_evaluator_for_python(AtomSpace*);
}
#endif