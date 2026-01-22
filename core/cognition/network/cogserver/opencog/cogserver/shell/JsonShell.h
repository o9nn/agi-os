#ifndef _OPENCOG_JSON_SHELL_H
#define _OPENCOG_JSON_SHELL_H
#include <opencog/network/GenericShell.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog {
class JsonShell : public GenericShell
{
protected:
AtomSpacePtr _shellspace;
public:
JsonShell(const AtomSpacePtr&);
virtual ~JsonShell();
virtual GenericEval* get_evaluator(void);
};
}
#endif