#ifndef _OPENCOG_SEQUENTIAL_READ_PROXY_H
#define _OPENCOG_SEQUENTIAL_READ_PROXY_H
#include <opencog/persist/proxy/ProxyNode.h>
namespace opencog
{
class SequentialReadProxy : public ProxyNode
{
private:
StorageNodeSeq _readers;
void init(void);
public:
SequentialReadProxy(const std::string&&);
SequentialReadProxy(Type t, const std::string&&);
virtual ~SequentialReadProxy();
virtual void open(void);
virtual void close(void);
virtual bool connected(void) { return  0 < _readers.size(); }
protected:
virtual void getAtom(const Handle&);
virtual void fetchIncomingSet(AtomSpace*, const Handle&);
virtual void fetchIncomingByType(AtomSpace*, const Handle&, Type);
virtual void storeAtom(const Handle&, bool synchronous = false) {}
virtual void removeAtom(AtomSpace*, const Handle&, bool recursive) {}
virtual void storeValue(const Handle& atom, const Handle& key) {}
virtual void updateValue(const Handle& atom, const Handle& key,
const ValuePtr& delta) {}
virtual void loadValue(const Handle& atom, const Handle& key);
virtual void loadType(AtomSpace*, Type);
virtual void loadAtomSpace(AtomSpace*) {}
virtual void storeAtomSpace(const AtomSpace*) {}
virtual void barrier(AtomSpace* = nullptr);
public:
static Handle factory(const Handle&);
};
NODE_PTR_DECL(SequentialReadProxy)
#define createSequentialReadProxy CREATE_DECL(SequentialReadProxy)
}
#endif