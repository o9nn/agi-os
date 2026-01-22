#ifndef _ATOMSPACE_ROCKS_STORAGE_H
#define _ATOMSPACE_ROCKS_STORAGE_H
#include <atomic>
#include <map>
#include <mutex>
#include "rocksdb/db.h"
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/rocks-types/atom_types.h>
#include <opencog/persist/api/StorageNode.h>
namespace opencog
{
class RocksStorage : public StorageNode
{
private:
void init(const char *);
std::string _uri;
rocksdb::DB* _rfile;
std::string get_version(void);
bool _multi_space;
std::unordered_map<Handle, const std::string> _frame_map;
std::unordered_map<std::string, Handle> _fid_map;
void makeOrder(Handle, std::map<uint64_t, Handle>&);
std::mutex _mtx_frame;
std::string encodeFrame(const Handle&);
std::string writeFrame(const Handle&);
std::string writeFrame(AtomSpace* as) {
return writeFrame(HandleCast(as));
}
Handle decodeFrame(const std::string&);
Handle getFrame(const std::string&);
bool checkFrames(void);
void scrubFrames(void);
HandleSeq topFrames(void);
void convertForFrames(const Handle&);
std::atomic_uint64_t _next_aid;
uint64_t strtoaid(const std::string&) const;
std::string aidtostr(uint64_t) const;
void write_aid(void);
std::string get_new_aid(void);
std::string tv_pred_sid;
std::mutex _mtx_sid;
size_t getHeight(const Handle&);
std::string findAtom(const Handle&);
std::string writeAtom(const Handle&, bool = true);
void appendToSidList(const std::string&, const std::string&);
void remFromSidList(const std::string&, const std::string&);
void storeValue(const std::string& skid,
const ValuePtr& vp);
void storeMissingAtom(AtomSpace*, const Handle&);
void doRemoveAtom(const Handle&, bool recursive);
ValuePtr getValue(const std::string&);
Handle getAtom(const std::string&);
Handle findAlpha(const Handle&, const std::string&, std::string&);
void getKeysMonospace(AtomSpace*, const std::string&, const Handle&);
void getKeysMulti(AtomSpace*, const std::string&, const Handle&);
void loadAtoms(AtomSpace*);
size_t loadAtomsPfx(const std::map<uint64_t, Handle>&,
const std::string&);
size_t loadAtomsHeight(const std::map<uint64_t, Handle>&,
size_t);
void loadAtomsAllFrames(AtomSpace*);
void loadTypeMonospace(AtomSpace*, Type);
void loadTypeAllFrames(AtomSpace*, Type);
void loadInset(AtomSpace*, const std::string& ist);
void appendToInset(const std::string&, const std::string&);
void remFromInset(const std::string&, const std::string&);
void removeSatom(const std::string&, const std::string&, bool, bool);
void remIncoming(const std::string&, const std::string&,
const std::string&);
size_t count_records(const std::string&);
public:
RocksStorage(std::string uri);
RocksStorage(const RocksStorage&) = delete;
RocksStorage& operator=(const RocksStorage&) = delete;
virtual ~RocksStorage();
void open(void);
void close(void);
bool connected(void);
void create(void) {}
void destroy(void) { kill_data(); }
void erase(void) { kill_data(); }
void kill_data(void);
void print_range(const std::string&);
void getAtom(const Handle&);
Handle getLink(Type, const HandleSeq&);
void fetchIncomingSet(AtomSpace*, const Handle&);
void fetchIncomingByType(AtomSpace*, const Handle&, Type t);
void storeAtom(const Handle&, bool synchronous = false);
void removeAtom(AtomSpace*, const Handle&, bool recursive);
void storeValue(const Handle& atom, const Handle& key);
void updateValue(const Handle&, const Handle&, const ValuePtr&);
void loadValue(const Handle& atom, const Handle& key);
void loadType(AtomSpace*, Type);
void loadAtomSpace(AtomSpace*);
void storeAtomSpace(const AtomSpace*);
HandleSeq loadFrameDAG(void);
void storeFrameDAG(AtomSpace*);
void deleteFrame(AtomSpace*);
void barrier(AtomSpace* = nullptr);
std::string monitor();
void print_stats(void);
void clear_stats(void);
void checkdb(void);
void scrubdb(void);
};
class RocksStorageNode : public RocksStorage
{
public:
RocksStorageNode(Type t, const std::string&& uri) :
RocksStorage(std::move(uri))
{}
RocksStorageNode(const std::string&& uri) :
RocksStorage(std::move(uri))
{}
void setAtomSpace(AtomSpace* as)
{
if (nullptr == as) close();
Atom::setAtomSpace(as);
}
static Handle factory(const Handle&);
};
NODE_PTR_DECL(RocksStorageNode)
#define createRocksStorageNode CREATE_DECL(RocksStorageNode)
}
#endif