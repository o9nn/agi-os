#ifndef _OPENCOG_DEME_PARAMETERS_H
#define _OPENCOG_DEME_PARAMETERS_H
#include "feature_selector.h"
#include "opencog/asmoses/moses/representation/representation.h"
namespace opencog { namespace moses {
struct deme_parameters
{
deme_parameters(int mcpd=-1,
bool _reduce_all=true,
const feature_selector* _fstor=nullptr,
bool as_store=true,
bool as_port=false,
AtomSpacePtr as=nullptr) :
max_candidates_per_deme(mcpd),
reduce_all(_reduce_all),
fstor(_fstor),
atomspace_store(as_store),
atomspace_port(as_port),
atomspace(as)
{}
int max_candidates_per_deme;
bool reduce_all;
const feature_selector* fstor;
bool atomspace_store;
bool atomspace_port;
AtomSpacePtr atomspace;
};
}
}
#endif