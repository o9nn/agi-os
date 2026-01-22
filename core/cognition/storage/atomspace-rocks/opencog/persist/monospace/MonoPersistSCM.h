#ifndef _OPENCOG_MONO_PERSIST_SCM_H
#define _OPENCOG_MONO_PERSIST_SCM_H
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/monospace/MonoStorage.h>
namespace opencog
{
class MonoPersistSCM
{
private:
static void* init_in_guile(void*);
static void init_in_module(void*);
void init(void);
MonoStorageNodePtr _storage;
AtomSpacePtr _as;
public:
MonoPersistSCM(AtomSpace*);
~MonoPersistSCM();
void do_open(const std::string&);
void do_close(void);
void do_load(void);
void do_store(void);
void do_get(const std::string&);
void do_stats(const Handle&);
void do_clear_stats(const Handle&);
void do_print(const Handle&, const std::string&);
};
}
extern "C" {
void opencog_persist_mono_init(void);
};
#endif