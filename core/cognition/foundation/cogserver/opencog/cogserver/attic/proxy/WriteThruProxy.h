#ifndef _OPENCOG_WRITE_THRU_PROXY_H
#define _OPENCOG_WRITE_THRU_PROXY_H
#include <vector>
#include <opencog/cogserver/proxy/Proxy.h>
#include <opencog/cogserver/proxy/ThruCommands.h>
namespace opencog {
class WriteThru : public ThruCommands
{
public:
WriteThru(void);
~WriteThru();
void setup(SexprEval*);
void extract_cb(const Handle&, bool);
void set_value_cb(const Handle&, const Handle&, const ValuePtr&);
void set_values_cb(const Handle&);
void set_tv_cb(const Handle&, const TruthValuePtr&);
void update_value_cb(const Handle&, const Handle&, const ValuePtr&);
};
class WriteThruProxy : public Proxy
{
protected:
WriteThru _wthru_wrap;
public:
WriteThruProxy(CogServer&);
virtual ~WriteThruProxy();
static const char *id(void);
virtual void init(void);
virtual bool config(const char*);
virtual void setup(SexprEval*);
};
}
#endif