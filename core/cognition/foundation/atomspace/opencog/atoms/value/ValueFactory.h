#ifndef _VALUE_FACTORY_H_
#define _VALUE_FACTORY_H_
#include "Value.h"
#include <opencog/util/exceptions.h>
#include <map>
#include <typeinfo>
#include <typeindex>
#include <vector>
namespace opencog
{
using ValueFactory = ValuePtr (*) (...);
using ValueCaster = ValuePtr (*) (const ValuePtr&);
template<typename... Types>
static std::vector<std::type_index> to_list_of_type_indexes()
{
return std::vector<std::type_index>{ std::type_index(typeid(Types))... };
}
class ValueServer
{
friend ValueServer& valueserver();
private:
ValueServer() {}
static std::string demangle(std::type_index);
struct ProtoFactory
{
ValueFactory func;
std::vector<std::type_index> args;
};
std::map<Type, std::vector<ProtoFactory>> _factories;
std::map<Type, ValueCaster> _vcasters;
public:
void addFactory(Type vtype, ValueFactory func,
std::vector<std::type_index> args);
void addCaster(Type vtype, ValueCaster caster);
ValuePtr recast(const ValuePtr& ptr) const;
template <typename TYP, typename... ARG>
ValuePtr create(TYP vtype, ARG&&... arg) const
{
static std::vector<ValueFactory> fax;
static std::mutex mtx;
ValueFactory fptr = nullptr;
try
{
fptr = fax.at(vtype);
}
catch(...) {}
if (nullptr == fptr)
{
try
{
std::vector<ProtoFactory> func_vec = _factories.at(vtype);
std::vector<std::type_index> expected_args =
to_list_of_type_indexes<ARG...>();
for (const ProtoFactory& fr : func_vec)
{
if (fr.args == expected_args)
{
fptr = fr.func;
std::lock_guard<std::mutex> lck(mtx);
std::vector<ValueFactory> newfax(fax);
if (newfax.size() <= vtype)
newfax.resize(vtype+1);
newfax[vtype] = fr.func;
fax.swap(newfax);
break;
}
}
}
catch(...) {}
}
if (fptr)
return (*fptr)(&arg...);
std::vector<std::type_index> expected_args =
to_list_of_type_indexes<ARG...>();
std::string argnames;
for (auto t : expected_args)
argnames += demangle(t) + " ";
throw IndexErrorException(TRACE_INFO,
"No factory found for Value type %d - %s(%s)",
vtype, nameserver().getTypeName(vtype).c_str(),
argnames.c_str());
}
};
ValueServer& valueserver();
#define TOKENPASTE(x, y) x ## y
#define TOKENPASTE2(x, y) TOKENPASTE(x, y)
#define DEFINE_VALUE_FACTORY(CTYPE,CREATE,...) \
\
\
\
static __attribute__ ((constructor (110))) void \
TOKENPASTE2(init, __COUNTER__)(void) \
{ \
valueserver().addFactory(CTYPE, (ValueFactory) & (CREATE<__VA_ARGS__>), \
to_list_of_type_indexes<__VA_ARGS__>()); \
}
}
#endif