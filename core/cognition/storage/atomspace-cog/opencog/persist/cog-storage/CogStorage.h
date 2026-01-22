#ifndef _OPENCOG_COG_STORAGE_H
#define _OPENCOG_COG_STORAGE_H
#include <opencog/persist/api/StorageNode.h>
#include <opencog/persist/cog-types/atom_types.h>
#include <opencog/persist/cog-storage/CogChannel.h>
namespace opencog
{
class CogStorage : public StorageNode
{
private:
void init(const char *);
std::string _uri;
struct Pkt
{
AtomSpace* table;
Handle h;
Handle key;
};
CogChannel<CogStorage, Pkt> _io_queue;
void noop_const(const std::string&, const Pkt&) {}
void noop(const std::string&, Pkt&) {}
void decode_atom_list(const std::string&, const Pkt&);
void decode_value(const std::string&, const Pkt&);
void decode_kvp_list_const(const std::string&, const Pkt&);
void decode_kvp_list(const std::string& s, Pkt& p)
{ decode_kvp_list_const(s, p); }
void is_ok(const std::string&, Pkt&);
void ro_decode_alist(AtomSpace*, const Handle&, const std::string&);
public:
CogStorage(std::string uri);
CogStorage(const CogStorage&) = delete;
CogStorage& operator=(const CogStorage&) = delete;
virtual ~CogStorage();
void open(void);
void close(void);
bool connected(void);
void create(void) {}
void destroy(void) { kill_data(); }
void erase(void) { kill_data(); }
void kill_data(void);
void proxy_open(void);
void proxy_close(void);
void set_proxy(const Handle&);
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
void barrier(AtomSpace* = nullptr);
std::string monitor(void);
};
class CogStorageNode : public CogStorage
{
public:
CogStorageNode(Type t, const std::string&& uri) :
CogStorage(std::move(uri))
{}
CogStorageNode(const std::string&& uri) :
CogStorage(std::move(uri))
{}
static Handle factory(const Handle&);
};
NODE_PTR_DECL(CogStorageNode)
#define createCogStorageNode CREATE_DECL(CogStorageNode)
}
#endif