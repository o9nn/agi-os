#ifndef _OPENCOG_SQL_PERSIST_SCM_H
#define _OPENCOG_SQL_PERSIST_SCM_H
#ifdef HAVE_GUILE
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/persist/sql/multi-driver/SQLAtomStorage.h>
namespace opencog
{
class SQLPersistSCM
{
private:
static void* init_in_guile(void*);
static void init_in_module(void*);
void init(void);
PostgresStorageNodePtr _storage;
AtomSpace* _as;
public:
SQLPersistSCM(AtomSpace*);
~SQLPersistSCM();
void do_create(const std::string&);
void do_open(const std::string&);
void do_close(void);
void do_stats(void);
void do_clear_cache(void);
void do_clear_stats(void);
void do_set_hilo(int, int);
void do_set_stall(bool);
};
}
extern "C" {
void opencog_persist_sql_init(void);
};
#endif
#endif