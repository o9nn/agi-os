#ifndef _OPENCOG_SQL_ATOM_STORAGE_H
#define _OPENCOG_SQL_ATOM_STORAGE_H
#include <atomic>
#include <mutex>
#include <set>
#include <vector>
#include <opencog/util/async_buffer.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/api/StorageNode.h>
#include <opencog/persist/tlb/TLB.h>
#include "llapi.h"
#define NUM_OMP_THREADS 8
namespace opencog
{
class SQLAtomStorage : public StorageNode
{
private:
concurrent_stack<LLConnection*> conn_pool;
int _initial_conn_pool_size;
void enlarge_conn_pool(int, const char*);
void close_conn_pool(void);
class Response;
bool _is_open;
bool _use_libpq;
bool _use_odbc;
int _server_version;
void get_server_version(void);
void connect(const char *);
std::set<UUID> table_id_cache;
void store_atomtable_id(const AtomSpace&);
struct PseudoAtom
: public std::enable_shared_from_this<PseudoAtom>
{
Type type;
UUID uuid;
std::string name;
std::vector<UUID> oset;
};
typedef std::shared_ptr<PseudoAtom> PseudoPtr;
#define createPseudo std::make_shared<PseudoAtom>
PseudoPtr makeAtom(Response&, UUID);
PseudoPtr getAtom(const char *, int);
PseudoPtr petAtom(UUID);
Handle get_recursive_if_not_exists(PseudoPtr);
Handle doGetNode(Type, const char *);
Handle doGetLink(Type, const HandleSeq&);
int getMaxObservedHeight(void);
int max_height;
void getIncoming(AtomSpace&, const char *);
std::mutex _store_mutex;
int do_store_atom(const Handle&);
void vdo_store_atom(const Handle&);
void do_store_single_atom(const Handle&, int);
bool not_yet_stored(const Handle&);
std::string oset_to_string(const HandleSeq&);
bool bulk_load;
bool bulk_store;
time_t bulk_start;
void removeAtom(Response&, UUID, bool recursive);
void deleteSingleAtom(Response&, UUID);
void rename_tables(void);
void create_tables(void);
#define NUMVMUT 16
std::mutex _value_mutex[NUMVMUT];
void store_atom_values(const Handle &);
void get_atom_values(Handle &);
typedef unsigned long VUID;
ValuePtr doUnpackValue(Response&);
ValuePtr doGetValue(const char *);
VUID storeValue(const ValuePtr&);
ValuePtr getValue(VUID);
void deleteValue(VUID);
std::mutex _valuation_mutex;
void storeValuation(const Handle&, const Handle&, const ValuePtr&);
void deleteValuation(const Handle&, const Handle&);
void deleteValuation(Response&, UUID, UUID);
void deleteAllValuations(Response&, UUID);
std::string float_to_string(const FloatValuePtr&);
std::string string_to_string(const StringValuePtr&);
std::string link_to_string(const LinkValuePtr&);
Handle tvpred;
UUID check_uuid(const Handle&);
UUID get_uuid(const Handle&);
UUID getMaxObservedUUID(void);
VUID getMaxObservedVUID(void);
TLB _tlbuf;
struct UUID_manager : public uuid_pool
{
const std::string poolname;
UUID_manager(const std::string& n) : poolname(n) {}
SQLAtomStorage* that;
void reset_uuid_pool(UUID);
void refill_uuid_pool(void);
int _uuid_pool_increment;
std::atomic<UUID> _uuid_pool_top;
std::atomic<UUID> _next_unused_uuid;
UUID get_uuid(void);
};
UUID_manager _uuid_manager;
UUID_manager _vuid_manager;
void registerWith(AtomSpace*);
void unregisterWith(AtomSpace*);
virtual void setAtomSpace(AtomSpace *);
std::atomic<size_t> _num_get_nodes;
std::atomic<size_t> _num_got_nodes;
std::atomic<size_t> _num_rec_nodes;
std::atomic<size_t> _num_get_links;
std::atomic<size_t> _num_got_links;
std::atomic<size_t> _num_rec_links;
std::atomic<size_t> _num_get_insets;
std::atomic<size_t> _num_get_inlinks;
std::atomic<size_t> _num_node_inserts;
std::atomic<size_t> _num_link_inserts;
std::atomic<size_t> _num_atom_removes;
std::atomic<size_t> _num_atom_deletes;
std::atomic<size_t> _load_count;
std::atomic<size_t> _store_count;
std::atomic<size_t> _valuation_stores;
std::atomic<size_t> _value_stores;
time_t _stats_time;
static_assert(2 == sizeof(Type),
"*** Typemap needs to be redesigned to handle larger types! ***");
#define TYPEMAP_SZ (1 << (8 * sizeof(Type)))
int storing_typemap[TYPEMAP_SZ];
Type loading_typemap[TYPEMAP_SZ];
char * db_typename[TYPEMAP_SZ];
bool type_map_was_loaded;
void load_typemap(void);
void setup_typemap(void);
void set_typemap(int, const char *);
std::mutex _typemap_mutex;
async_buffer<SQLAtomStorage, Handle> _write_queue;
std::exception_ptr _async_write_queue_exception;
void rethrow(void);
public:
SQLAtomStorage(std::string uri);
virtual ~SQLAtomStorage();
void open(void);
void close(void);
void connect(void);
bool connected(void);
void create_database(void);
void kill_data(void);
void clear_cache(void);
void create(void) { create_database(); }
void destroy(void) { kill_data(); }
void erase(void) { kill_data(); }
void extract_callback(const AtomPtr&);
int _extract_sig;
Handle getNode(Type, const char *);
Handle getLink(Type, const HandleSeq&);
void fetchIncomingSet(AtomSpace*, const Handle&);
void fetchIncomingByType(AtomSpace*, const Handle&, Type t);
void storeAtom(const Handle&, bool synchronous = false);
void removeAtom(AtomSpace*, const Handle&, bool recursive);
void storeValue(const Handle&, const Handle&);
void loadValue(const Handle&, const Handle&);
void loadType(AtomSpace*, Type);
void barrier(AtomSpace* = nullptr);
void flushStoreQueue();
void loadAtomSpace(AtomSpace*);
void storeAtomSpace(const AtomSpace*);
void print_stats(void);
void clear_stats(void);
void set_hilo_watermarks(int, int);
void set_stall_writers(bool);
};
class PostgresStorageNode : public SQLAtomStorage
{
public:
PostgresStorageNode(Type t, const std::string&& uri) :
SQLAtomStorage(std::move(uri))
{}
PostgresStorageNode(const std::string&& uri) :
SQLAtomStorage(std::move(uri))
{}
static Handle factory(const Handle&);
};
typedef std::shared_ptr<PostgresStorageNode> PostgresStorageNodePtr;
static inline PostgresStorageNodePtr PostgresStorageNodeCast(const Handle& h)
{ return std::dynamic_pointer_cast<PostgresStorageNode>(h); }
static inline PostgresStorageNodePtr PostgresStorageNodeCast(AtomPtr a)
{ return std::dynamic_pointer_cast<PostgresStorageNode>(a); }
#define createPostgresStorageNode std::make_shared<PostgresStorageNode>
}
#endif