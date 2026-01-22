#ifndef _OPENCOG_READ_WRITE_PROXY_H
#define _OPENCOG_READ_WRITE_PROXY_H
#include <opencog/persist/proxy/ProxyNode.h>
namespace opencog
{
class ReadWriteProxy : public ProxyNode
{
private:
StorageNodePtr _reader;
StorageNodePtr _writer;
void init(void);
public:
ReadWriteProxy(const std::string&&);
ReadWriteProxy(Type t, const std::string&&);
virtual ~ReadWriteProxy();
virtual void open(void);
virtual void close(void);
virtual bool connected(void) { return _reader != nullptr; }
protected:
virtual void getAtom(const Handle&);
virtual void fetchIncomingSet(AtomSpace*, const Handle&);
virtual void fetchIncomingByType(AtomSpace*, const Handle&, Type);
virtual void storeAtom(const Handle&, bool synchronous = false);
virtual void preRemoveAtom(AtomSpace*, const Handle&, bool recursive);
virtual void postRemoveAtom(AtomSpace*, const Handle&,
bool recursive, bool extracted_ok);
virtual void storeValue(const Handle& atom, const Handle& key);
virtual void updateValue(const Handle& atom, const Handle& key,
const ValuePtr& delta);
virtual void loadValue(const Handle& atom, const Handle& key);
virtual void loadType(AtomSpace*, Type);
virtual void loadAtomSpace(AtomSpace*) {}
virtual void storeAtomSpace(const AtomSpace*) {}
virtual void barrier(AtomSpace* = nullptr);
virtual std::string monitor(void);
public:
static Handle factory(const Handle&);
};
NODE_PTR_DECL(ReadWriteProxy)
#define createReadWriteProxy CREATE_DECL(ReadWriteProxy)
}
#endif