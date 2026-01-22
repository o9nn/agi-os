#ifndef _OPENCOG_WRITE_BUFFER_PROXY_H
#define _OPENCOG_WRITE_BUFFER_PROXY_H
#include <thread>
#include <opencog/util/concurrent_set.h>
#include <opencog/persist/proxy/WriteThruProxy.h>
namespace opencog
{
class WriteBufferProxy : public WriteThruProxy
{
private:
void reset_stats(void);
size_t _nstalls;
size_t _novertime;
size_t _nbars;
size_t _ndumps;
size_t _astore;
size_t _vstore;
double _mavg_in_atoms;
double _mavg_in_values;
double _mavg_buf_atoms;
double _mavg_buf_values;
double _mavg_out_atoms;
double _mavg_out_values;
double _mavg_load;
protected:
double _decay;
double _ticker;
size_t _high_water_mark;
concurrent_set<Handle> _atom_queue;
concurrent_set<std::pair<Handle,Handle>> _value_queue;
std::thread _drain_thread;
bool _stop;
void drain_loop();
void erase_recursive(const Handle&);
private:
void init(void);
public:
WriteBufferProxy(const std::string&&);
WriteBufferProxy(Type, const std::string&&);
virtual ~WriteBufferProxy();
virtual void setValue(const Handle& key, const ValuePtr& value);
virtual void open(void);
virtual void close(void);
protected:
virtual void storeAtom(const Handle&, bool synchronous = false);
virtual void preRemoveAtom(AtomSpace*, const Handle&, bool recursive);
virtual void postRemoveAtom(AtomSpace*, const Handle&,
bool recursive, bool exok);
virtual void storeValue(const Handle& atom, const Handle& key);
virtual void updateValue(const Handle& atom, const Handle& key,
const ValuePtr& delta);
virtual void barrier(AtomSpace* = nullptr);
virtual std::string monitor(void);
public:
static Handle factory(const Handle&);
};
NODE_PTR_DECL(WriteBufferProxy)
#define createWriteBufferProxy CREATE_DECL(WriteBufferProxy)
}
#endif