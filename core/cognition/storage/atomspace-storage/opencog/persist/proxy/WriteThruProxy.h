#ifndef _OPENCOG_WRITE_THRU_PROXY_H
#define _OPENCOG_WRITE_THRU_PROXY_H
#include <opencog/persist/proxy/ProxyNode.h>
namespace opencog
{
class WriteThruProxy : public ProxyNode
{
protected:
StorageNodeSeq _targets;
private:
void init(void);
public:
WriteThruProxy(const std::string&&);
WriteThruProxy(Type, const std::string&&);
virtual ~WriteThruProxy();
virtual void open(void);
virtual void close(void);
virtual bool connected(void) { return 0 < _targets.size(); }
protected:
virtual void getAtom(const Handle&) {}
virtual void fetchIncomingSet(AtomSpace*, const Handle&) {}
virtual void fetchIncomingByType(AtomSpace*, const Handle&, Type) {}
virtual void storeAtom(const Handle&, bool synchronous = false);
virtual void preRemoveAtom(AtomSpace*, const Handle&, bool recursive);
virtual void postRemoveAtom(AtomSpace*, const Handle&,
bool recursive, bool exok);
virtual void storeValue(const Handle& atom, const Handle& key);
virtual void updateValue(const Handle& atom, const Handle& key,
const ValuePtr& delta);
virtual void loadValue(const Handle& atom, const Handle& key) {}
virtual void loadType(AtomSpace*, Type) {}
virtual void loadAtomSpace(AtomSpace*) {}
virtual void storeAtomSpace(const AtomSpace*) {}
virtual void barrier(AtomSpace* = nullptr);
public:
static Handle factory(const Handle&);
};
NODE_PTR_DECL(WriteThruProxy)
#define createWriteThruProxy CREATE_DECL(WriteThruProxy)
}
#endif