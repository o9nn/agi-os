#ifndef _ATOMSPACE_MONO_STORAGE_H
#define _ATOMSPACE_MONO_STORAGE_H
#include <atomic>
#include <mutex>
#include "rocksdb/db.h"
#include <opencog/persist/api/StorageNode.h>
#include <opencog/persist/rocks-types/atom_types.h>
namespace opencog
{
class MonoSatisfyingSet;
class MonoStorage : public StorageNode
{
friend class MonoImplicator;
friend class MonoSatisfyingSet;
friend class MonoJoinCallback;
private:
void init(const char *);
std::string _uri;
rocksdb::DB* _rfile;
std::atomic_uint64_t _next_aid;
uint64_t strtoaid(const std::string&) const;
std::string aidtostr(uint64_t) const;
void write_aid(void);
std::string tv_pred_sid;
std::mutex _mtx_sid;
#ifdef NEED_LIST_LOCK
std::recursive_mutex _mtx_list;
#endif
std::string findAtom(const Handle&);
std::string writeAtom(const Handle&);
void appendToSidList(const std::string&, const std::string&);
void remFromSidList(const std::string&, const std::string&);
void storeValue(const std::string& skid,
const ValuePtr& vp);
ValuePtr getValue(const std::string&);
Handle getAtom(const std::string&);
Handle findAlpha(const Handle&, const std::string&, std::string&);
void getKeys(AtomSpace*, const std::string&, const Handle&);
void loadAtoms(AtomSpace*, const std::string& pfx);
void loadInset(AtomSpace*, const std::string& ist);
void appendToInset(const std::string&, const std::string&);
void remFromInset(const std::string&, const std::string&);
void removeSatom(const std::string&, const std::string&, bool, bool);
void remIncoming(const std::string&, const std::string&,
const std::string&);
size_t count_records(const std::string&);
public:
MonoStorage(std::string uri);
MonoStorage(const MonoStorage&) = delete;
MonoStorage& operator=(const MonoStorage&) = delete;
virtual ~MonoStorage();
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
void barrier();
std::string monitor();
void print_stats(void);
void clear_stats(void);
void checkdb(void);
};
class MonoStorageNode : public MonoStorage
{
public:
MonoStorageNode(Type t, const std::string&& uri) :
MonoStorage(std::move(uri))
{}
MonoStorageNode(const std::string&& uri) :
MonoStorage(std::move(uri))
{}
void setAtomSpace(AtomSpace* as)
{
if (nullptr == as) close();
Atom::setAtomSpace(as);
}
static Handle factory(const Handle&);
};
NODE_PTR_DECL(MonoStorageNode)
#define createMonoStorageNode CREATE_DECL(MonoStorageNode)
}
#endif