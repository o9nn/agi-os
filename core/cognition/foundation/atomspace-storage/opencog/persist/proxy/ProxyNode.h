#ifndef _OPENCOG_PROXY_NODE_H
#define _OPENCOG_PROXY_NODE_H
#include <opencog/persist/api/StorageNode.h>
namespace opencog
{
class ProxyNode : public StorageNode
{
private:
void init(void);
protected:
StorageNodeSeq _parts;
public:
ProxyNode(const std::string&&);
ProxyNode(Type t, const std::string&&);
virtual ~ProxyNode();
virtual void setValue(const Handle& key, const ValuePtr& value);
StorageNodeSeq setup() { return _parts; }
bool have_getAtom;
bool have_fetchIncomingSet;
bool have_fetchIncomingByType;
bool have_storeAtom;
bool have_removeAtom;
bool have_storeValue;
bool have_updateValue;
bool have_loadValue;
bool have_loadType;
bool have_loadAtomSpace;
bool have_storeAtomSpace;
virtual void create(void) {}
virtual void destroy(void);
virtual void erase(void);
virtual void proxy_open(void);
virtual void proxy_close(void);
virtual void set_proxy(const Handle&);
virtual std::string monitor(void);
protected:
virtual HandleSeq loadFrameDAG(void);
virtual void storeFrameDAG(AtomSpace*) {}
virtual void deleteFrame(AtomSpace*) {}
virtual Handle getLink(Type, const HandleSeq&);
};
NODE_PTR_DECL(ProxyNode)
#define createProxyNode CREATE_DECL(ProxyNode)
}
extern "C" {
void opencog_persist_proxy_init(void);
};
#endif