#ifndef _OPENCOG_FOREIGN_PERSIST_SCM_H
#define _OPENCOG_FOREIGN_PERSIST_SCM_H
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
class BridgePersistSCM
{
private:
static void* init_in_guile(void*);
static void init_in_module(void*);
void init(void);
AtomSpacePtr _as;
public:
BridgePersistSCM(AtomSpace*);
~BridgePersistSCM();
HandleSeq do_load_tables(const Handle&);
HandleSeq do_load_rows(const Handle&, const Handle&, const Handle&, const Handle&);
};
}
extern "C" {
void opencog_persist_bridge_init(void);
};
#endif