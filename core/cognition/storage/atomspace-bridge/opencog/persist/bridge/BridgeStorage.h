#ifndef _ATOMSPACE_FOREIGN_STORAGE_H
#define _ATOMSPACE_FOREIGN_STORAGE_H
#include <atomic>
#include <map>
#include <mutex>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/api/StorageNode.h>
#include "llapi.h"
namespace opencog
{
class BridgeStorage : public StorageNode
{
private:
std::string _uri;
concurrent_stack<LLConnection*> conn_pool;
int _initial_conn_pool_size;
void enlarge_conn_pool(int, const char*);
void close_conn_pool(void);
class Response;
bool _is_open;
int _server_version;
void get_server_version(void);
size_t _num_queries;
size_t _num_tables;
size_t _num_rows;
Handle load_one_table(const std::string&);
Handle get_row_desc(const Handle&);
std::string make_select(const Handle&);
void load_selected_rows(const Handle&, const std::string&);
void load_table_data(const Handle&);
void load_column(const Handle&);
void select_where(const Handle&, const Handle&, const Handle&);
void load_join(const Handle&, const Handle&);
void load_joined_rows(const Handle&);
public:
BridgeStorage(std::string uri);
BridgeStorage(const BridgeStorage&) = delete;
BridgeStorage& operator=(const BridgeStorage&) = delete;
virtual ~BridgeStorage();
void open(void);
void close(void);
bool connected(void);
void create(void) {}
void destroy(void) { kill_data();  }
void erase(void) { kill_data(); }
void kill_data(void) {}
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
HandleSeq loadFrameDAG(void) {return HandleSeq(); }
void storeFrameDAG(AtomSpace*) {}
void deleteFrame(AtomSpace*) {}
void barrier(AtomSpace* = nullptr);
std::string monitor();
void print_stats(void);
void clear_stats(void);
HandleSeq load_tables(void);
HandleSeq load_rows(const Handle&, const Handle&, const Handle&);
};
class BridgeStorageNode : public BridgeStorage
{
public:
BridgeStorageNode(Type t, const std::string&& uri) :
BridgeStorage(std::move(uri))
{}
BridgeStorageNode(const std::string&& uri) :
BridgeStorage(std::move(uri))
{}
void setAtomSpace(AtomSpace* as)
{
if (nullptr == as) close();
Atom::setAtomSpace(as);
}
static Handle factory(const Handle&);
};
NODE_PTR_DECL(BridgeStorageNode)
#define createBridgeStorageNode CREATE_DECL(BridgeStorageNode)
}
#endif