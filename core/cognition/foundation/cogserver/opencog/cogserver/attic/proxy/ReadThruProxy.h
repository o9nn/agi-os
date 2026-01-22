#ifndef _OPENCOG_READ_THRU_PROXY_H
#define _OPENCOG_READ_THRU_PROXY_H
#include <vector>
#include <opencog/cogserver/proxy/Proxy.h>
#include <opencog/cogserver/proxy/ThruCommands.h>
namespace opencog {
class ReadThru : public ThruCommands
{
public:
ReadThru(void);
~ReadThru();
void setup(SexprEval*);
void get_atoms_cb(Type, bool);
void incoming_by_type_cb(const Handle&, Type);
void incoming_set_cb(const Handle&);
void keys_alist_cb(const Handle&);
void node_cb(const Handle&);
void link_cb(const Handle&);
void value_cb(const Handle&, const Handle&);
};
class ReadThruProxy : public Proxy
{
protected:
ReadThru _rthru_wrap;
public:
ReadThruProxy(CogServer&);
virtual ~ReadThruProxy();
static const char *id(void);
virtual void init(void);
virtual bool config(const char*);
virtual void setup(SexprEval*);
};
}
#endif