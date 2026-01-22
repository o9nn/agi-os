#ifndef _OPENCOG_ROCKS_PERSIST_SCM_H
#define _OPENCOG_ROCKS_PERSIST_SCM_H
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/rocks/RocksStorage.h>
namespace opencog
{
class RocksPersistSCM
{
private:
static void* init_in_guile(void*);
static void init_in_module(void*);
void init(void);
RocksStorageNodePtr _storage;
AtomSpacePtr _as;
public:
RocksPersistSCM(AtomSpace*);
~RocksPersistSCM();
void do_open(const std::string&);
void do_close(void);
void do_load(void);
void do_store(void);
void do_get(const std::string&);
void do_stats(const Handle&);
void do_clear_stats(const Handle&);
void do_print(const Handle&, const std::string&);
void do_check(const Handle&);
void do_scrub(const Handle&);
};
}
extern "C" {
void opencog_persist_rocks_init(void);
};
#endif