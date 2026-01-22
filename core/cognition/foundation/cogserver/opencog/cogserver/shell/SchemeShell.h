#ifdef HAVE_GUILE
#ifndef _OPENCOG_SCHEME_SHELL_H
#define _OPENCOG_SCHEME_SHELL_H
#include <string>
#include <opencog/network/GenericShell.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog {
class SchemeShell : public GenericShell
{
protected:
AtomSpacePtr _shellspace;
void thread_init();
static std::string _prompt;
public:
SchemeShell(const AtomSpacePtr&);
virtual ~SchemeShell();
virtual GenericEval* get_evaluator(void);
};
}
#endif
#endif