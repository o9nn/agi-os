#ifndef _OPENCOG_EXECUTION_FORCE_H
#define _OPENCOG_EXECUTION_FORCE_H
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
Handle force_execute(AtomSpace*, const Handle&, bool silent=false);
}
#endif