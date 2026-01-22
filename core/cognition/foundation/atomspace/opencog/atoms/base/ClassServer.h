#ifndef _OPENCOG_CLASS_SERVER_H
#define _OPENCOG_CLASS_SERVER_H
#include <mutex>
#include <set>
#include <unordered_map>
#include <vector>
#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/base/Handle.h>
class ClassServerUTest;
namespace opencog
{
class ClassServer
{
friend class opencog::NameServer;
friend class ::ClassServerUTest;
public:
typedef Handle (AtomFactory)(const Handle&);
typedef bool (Validator)(const Handle&);
private:
ClassServer(const NameServer &);
mutable std::mutex factory_mutex;
mutable std::vector<AtomFactory*> _atomFactory;
mutable std::vector<Validator*> _validator;
template<typename T>
void splice(std::vector<T>&, Type, T);
template<typename T>
void update(std::vector<T>&, Type);
void update_factories();
const NameServer & _nameServer;
AtomFactory* getFactory(Type) const;
public:
friend ClassServer& classserver();
void addFactory(Type, AtomFactory*);
void addValidator(Type, Validator*);
Validator* getValidator(Type) const;
Handle factory(const Handle&) const;
};
ClassServer& classserver();
#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)
#define DEFINE_LINK_FACTORY(CNAME,CTYPE)                          \
\
Handle CNAME::factory(const Handle& base)                         \
{                                                                 \
\
if (CNAME##Cast(base)) return base;                            \
\
Handle h(create##CNAME(std::move(base->getOutgoingSet()),      \
base->get_type()));                     \
return h;                                                      \
}                                                                 \
\
\
\
static __attribute__ ((constructor (110))) void                   \
TOKENPASTE2(init, __COUNTER__)(void)                           \
{                                                                 \
classserver().addFactory(CTYPE, &CNAME::factory);              \
}
#define DEFINE_NODE_FACTORY(CNAME,CTYPE)                          \
\
Handle CNAME::factory(const Handle& base)                         \
{                                                                 \
if (CNAME##Cast(base)) return base;                            \
Handle h(create##CNAME(base->get_type(),                       \
std::move(base->get_name())));          \
return h;                                                      \
}                                                                 \
\
\
\
static __attribute__ ((constructor (110))) void                   \
TOKENPASTE2(init, __COUNTER__)(void)                           \
{                                                                 \
classserver().addFactory(CTYPE, &CNAME::factory);              \
}
}
#endif