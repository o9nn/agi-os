#ifndef _OPENCOG_SEXPR_SHELL_H
#define _OPENCOG_SEXPR_SHELL_H
#include <opencog/network/GenericShell.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog {
class SexprShell : public GenericShell
{
protected:
AtomSpacePtr _shellspace;
public:
SexprShell(const AtomSpacePtr&);
virtual ~SexprShell();
virtual GenericEval* get_evaluator(void);
};
}
#endif