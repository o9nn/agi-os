#ifndef _OPENCOG_STORAGE_NODE_H
#define _OPENCOG_STORAGE_NODE_H
#include <opencog/atoms/base/Node.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/persist/api/BackingStore.h>
#include <opencog/persist/storage/storage_types.h>
namespace opencog
{
class StorageNode : public Node, protected BackingStore
{
friend class WriteThruProxy;
friend class ReadWriteProxy;
protected:
Handle add_nocheck(AtomSpace* as, const Handle& h) const
{ return as->add(h); }
void get_absent_atoms(const AtomSpace* as, HandleSeq& missing) const
{ as->get_absent_atoms(missing); }
void get_atoms_in_frame(const AtomSpace* as, HandleSeq& fseq) const
{ as->get_atoms_in_frame(fseq); }
void remove_msg(const ValuePtr&, bool recursive=false);
void load_atoms_of_type_msg(const ValuePtr&);
void fetch_query_msg(const ValuePtr&);
public:
StorageNode(Type, std::string);
virtual ~StorageNode();
virtual void setValue(const Handle& key, const ValuePtr& value);
virtual ValuePtr getValue(const Handle& key) const;
virtual void open(void) = 0;
virtual void close(void) = 0;
virtual bool connected(void) = 0;
virtual void create(void) = 0;
virtual void destroy(void) = 0;
virtual void erase(void) = 0;
virtual void proxy_open(void);
virtual void proxy_close(void);
virtual void set_proxy(const Handle&);
virtual std::string monitor(void);
void barrier(AtomSpace* = nullptr);
Handle fetch_atom(const Handle&, AtomSpace* = nullptr);
Handle fetch_value(const Handle& atom, const Handle& key,
AtomSpace* = nullptr);
void fetch_all_atoms_of_type(Type t, AtomSpace* = nullptr);
void load_atomspace(AtomSpace* = nullptr);
void store_atomspace(AtomSpace* = nullptr);
HandleSeq load_frames(void);
void store_frames(const Handle&);
void delete_frame(const Handle&);
Handle fetch_incoming_set(const Handle&, bool = false, AtomSpace* = nullptr);
Handle fetch_incoming_by_type(const Handle&, Type, AtomSpace* = nullptr);
Handle fetch_query(const Handle& query, const Handle& key,
const Handle& metadata_key = Handle::UNDEFINED,
bool fresh = false,
AtomSpace* = nullptr);
void store_atom(const Handle& h);
void store_value(const Handle& atom, const Handle& key);
void update_value(const Handle& atom, const Handle& key,
const ValuePtr& delta);
bool remove_atom(AtomSpace*, Handle, bool recursive=false);
bool remove_atom(const AtomSpacePtr& as, Handle h, bool recursive=false)
{ return remove_atom(as.get(), h, recursive); }
};
NODE_PTR_DECL(StorageNode)
typedef std::vector<StorageNodePtr> StorageNodeSeq;
}
#endif