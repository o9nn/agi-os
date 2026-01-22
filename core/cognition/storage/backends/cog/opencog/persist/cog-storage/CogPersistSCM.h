#ifndef _OPENCOG_COG_PERSIST_SCM_H
#define _OPENCOG_COG_PERSIST_SCM_H
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/cog-storage/CogStorage.h>
namespace opencog
{
class CogPersistSCM
{
private:
static void* init_in_guile(void*);
static void init_in_module(void*);
void init(void);
CogStorageNodePtr _storage;
AtomSpacePtr _as;
public:
CogPersistSCM(AtomSpace*);
~CogPersistSCM();
void do_open(const std::string&);
void do_close(void);
};
}
extern "C" {
void opencog_persist_cog_init(void);
};
#endif