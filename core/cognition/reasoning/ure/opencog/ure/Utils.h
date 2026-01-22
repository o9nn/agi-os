#ifndef _OPENCOG_URE_UTILS_H
#define _OPENCOG_URE_UTILS_H
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
bool remove_hypergraph(AtomSpace&, const Handle&);
}
#endif