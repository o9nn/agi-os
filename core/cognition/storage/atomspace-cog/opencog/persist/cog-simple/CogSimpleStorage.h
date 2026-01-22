#ifndef _SIMPLE_COG_STORAGE_H
#define _SIMPLE_COG_STORAGE_H
#include <opencog/persist/api/StorageNode.h>
#include <opencog/persist/cog-types/atom_types.h>
namespace opencog
{
class CogSimpleStorage : public StorageNode
{
private:
void init(const char *);
std::string _uri;
std::mutex _mtx;
int _sockfd;
void do_send(const std::string&);
std::string do_recv(bool=false);
void decode_atom_list(AtomSpace*);
void ro_decode_alist(AtomSpace*, const Handle&, const std::string&);
bool _multi_space;
std::unordered_map<Handle, const std::string> _frame_map;
std::unordered_map<std::string, Handle> _fid_map;
std::mutex _mtx_frame;
void cacheFrame(const Handle&);
std::string writeFrame(const Handle&);
std::string writeFrame(AtomSpace* as) {
return writeFrame(HandleCast(as));
}
Handle getFrame(const std::string&);
public:
CogSimpleStorage(std::string uri);
CogSimpleStorage(const CogSimpleStorage&) = delete;
CogSimpleStorage& operator=(const CogSimpleStorage&) = delete;
virtual ~CogSimpleStorage();
void open(void);
void close(void);
bool connected(void);
void create(void) {}
void destroy(void) { kill_data(); }
void erase(void) { kill_data(); }
void kill_data(void);
void set_proxy(const Handle&);
void proxy_open(void);
void proxy_close(void);
void getAtom(const Handle&);
void fetchIncomingSet(AtomSpace*, const Handle&);
void fetchIncomingByType(AtomSpace*, const Handle&, Type t);
void storeAtom(const Handle&, bool synchronous = false);
void removeAtom(AtomSpace*, const Handle&, bool recursive);
void storeValue(const Handle& atom, const Handle& key);
void updateValue(const Handle&, const Handle&, const ValuePtr&);
void loadValue(const Handle& atom, const Handle& key);
void runQuery(const Handle&, const Handle&,
const Handle&, bool);
void loadType(AtomSpace*, Type);
void loadAtomSpace(AtomSpace*);
void storeAtomSpace(const AtomSpace*);
HandleSeq loadFrameDAG(void);
void storeFrameDAG(AtomSpace*);
void barrier(AtomSpace* = nullptr);
std::string monitor(void);
};
class CogSimpleStorageNode : public CogSimpleStorage
{
public:
CogSimpleStorageNode(Type t, const std::string&& uri) :
CogSimpleStorage(std::move(uri))
{}
CogSimpleStorageNode(const std::string&& uri) :
CogSimpleStorage(std::move(uri))
{}
static Handle factory(const Handle&);
};
NODE_PTR_DECL(CogSimpleStorageNode)
#define createCogSimpleStorageNode CREATE_DECL(CogSimpleStorageNode)
}
#endif