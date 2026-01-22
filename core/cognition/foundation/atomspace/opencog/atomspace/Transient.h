#ifndef _OPENCOG_TRANSIENT_H
#define _OPENCOG_TRANSIENT_H
namespace opencog
{
class AtomSpace;
AtomSpace* grab_transient_atomspace(AtomSpace*);
void release_transient_atomspace(AtomSpace*);
}
#endif