#ifndef _OPENCOG_CACHING_PROXY_H
#define _OPENCOG_CACHING_PROXY_H
#include <opencog/persist/proxy/ReadThruProxy.h>
namespace opencog
{
class CachingProxy : public ReadThruProxy
{
private:
void init(void);
size_t _nhits;
size_t _nmisses;
public:
CachingProxy(const std::string&&);
CachingProxy(Type t, const std::string&&);
virtual ~CachingProxy();
virtual void open(void);
virtual void close(void);
protected:
virtual void getAtom(const Handle&);
virtual void fetchIncomingSet(AtomSpace*, const Handle&);
virtual void fetchIncomingByType(AtomSpace*, const Handle&, Type);
virtual void loadValue(const Handle& atom, const Handle& key);
virtual void loadType(AtomSpace*, Type);
virtual std::string monitor(void);
public:
static Handle factory(const Handle&);
};
NODE_PTR_DECL(CachingProxy)
#define createCachingProxy CREATE_DECL(CachingProxy)
}
#endif