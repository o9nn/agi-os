#ifndef _OPENCOG_IPFS_ATOM_STORAGE_H
#define _OPENCOG_IPFS_ATOM_STORAGE_H
#include <atomic>
#include <condition_variable>
#include <mutex>
#include <set>
#include <vector>
#include <ipfs/client.h>
#include <opencog/util/async_buffer.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/base/Valuation.h>
#include <opencog/atomspace/AtomTable.h>
#include <opencog/atomspace/BackingStore.h>
namespace opencog
{
#define NUM_OMP_THREADS 1
class IPFSAtomStorage : public BackingStore
{
private:
void init(const char *);
std::string _uri;
std::string _hostname;
int _port;
concurrent_stack<ipfs::Client*> conn_pool;
int _initial_conn_pool_size;
Handle tvpred;
std::condition_variable _publish_cv;
bool _publish_keep_going;
static void publish_thread(IPFSAtomStorage*);
std::string _keyname;
std::string _key_cid;
std::mutex _atomspace_cid_mutex;
std::string _atomspace_cid;
void update_atom_in_atomspace(const Handle&,
const std::string&);
std::mutex _json_mutex;
std::map<Handle, ipfs::Json> _json_map;
ipfs::Json get_atom_json(const Handle&);
ipfs::Json fetch_atom_dag(const std::string&);
Handle decodeStrAtom(const std::string&);
Handle decodeJSONAtom(const ipfs::Json&);
Handle do_fetch_atom(Handle&);
std::string encodeValueToStr(const ValuePtr&);
std::string encodeAtomToStr(const Handle& h) {
return h->to_short_string(); }
ipfs::Json encodeAtomToJSON(const Handle&);
std::mutex _guid_mutex;
std::unordered_map<Handle, std::string> _guid_map;
std::mutex _inv_mutex;
std::unordered_map<std::string, Handle> _guid_inv_map;
std::mutex _atom_cid_mutex;
std::unordered_map<Handle, std::string> _atom_cid_map;
void do_store_atom(const Handle&);
void vdo_store_atom(const Handle&);
void do_store_single_atom(const Handle&);
bool guid_not_yet_stored(const Handle&);
bool bulk_load;
bool bulk_store;
time_t bulk_start;
void load_as_from_cid(AtomSpace*, const std::string&);
void store_atom_values(const Handle &);
void get_atom_values(Handle &, const ipfs::Json&);
ipfs::Json encodeValuesToJSON(const Handle&);
ValuePtr decodeStrValue(const std::string&);
void store_incoming_of(const Handle &, const Handle&);
void remove_incoming_of(const Handle &, const std::string&);
std::atomic<size_t> _num_get_atoms;
std::atomic<size_t> _num_got_nodes;
std::atomic<size_t> _num_got_links;
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
async_buffer<IPFSAtomStorage, Handle> _write_queue;
std::exception_ptr _async_write_queue_exception;
void rethrow(void);
public:
IPFSAtomStorage(std::string uri);
IPFSAtomStorage(const IPFSAtomStorage&) = delete;
IPFSAtomStorage& operator=(const IPFSAtomStorage&) = delete;
virtual ~IPFSAtomStorage();
bool connected(void);
std::string get_ipfs_cid(void);
std::string get_ipns_key(void);
void publish_atomspace(void);
void resolve_atomspace(void);
std::string get_atom_guid(const Handle&);
Handle fetch_atom(const std::string&);
void load_atomspace(AtomSpace*, const std::string&);
void kill_data(void);
void registerWith(AtomSpace*);
void unregisterWith(AtomSpace*);
void extract_callback(const AtomPtr&);
int _extract_sig;
Handle getNode(Type, const char *);
Handle getLink(Type, const HandleSeq&);
void getIncomingSet(AtomTable&, const Handle&);
void getIncomingByType(AtomTable&, const Handle&, Type t);
void storeAtom(const Handle&, bool synchronous = false);
void removeAtom(const Handle&, bool recursive);
void loadType(AtomTable&, Type);
void loadAtomSpace(AtomTable&);
void storeAtomSpace(const AtomTable&);
void barrier();
void flushStoreQueue();
void print_stats(void);
void clear_stats(void);
void set_hilo_watermarks(int, int);
void set_stall_writers(bool);
};
}
#endif