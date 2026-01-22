#ifndef _OPENCOG_SIMPLE_COG_PERSIST_SCM_H
#define _OPENCOG_SIMPLE_COG_PERSIST_SCM_H
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/cog-simple/CogSimpleStorage.h>
namespace opencog
{
class CogSimplePersistSCM
{
private:
static void* init_in_guile(void*);
static void init_in_module(void*);
void init(void);
CogSimpleStorageNodePtr _storage;
AtomSpacePtr _as;
public:
CogSimplePersistSCM(AtomSpace*);
~CogSimplePersistSCM();
void do_open(const std::string&);
void do_close(void);
void do_load(void);
void do_store(void);
void do_stats(void);
void do_clear_stats(void);
};
}
extern "C" {
void opencog_persist_cog_simple_init(void);
};
#endif