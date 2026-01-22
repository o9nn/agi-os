#ifndef _OPENCOG_UTILITIES_H
#define _OPENCOG_UTILITIES_H
#include "opencog/atoms/base/Handle.h"
#include "opencog/atomspace/AtomSpace.h"
namespace opencog {
void initialize_python();
void finalize_python();
Handle add_node(Type, std::string);
Handle add_link(Type, HandleSeq);
AtomSpacePtr get_context_atomspace(void);
}
#endif