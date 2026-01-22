#ifndef _OPENCOG_FACTORY_H
#define _OPENCOG_FACTORY_H
#include <string>
namespace opencog
{
class CogServer;
struct ClassInfo
{
std::string id;
ClassInfo() {};
ClassInfo(const char* s) : id(s) {};
ClassInfo(const std::string& s) : id(s) {};
};
template< typename _BaseType >
class AbstractFactory
{
public:
explicit AbstractFactory() {};
virtual ~AbstractFactory() {}
virtual _BaseType* create(CogServer&) const = 0;
virtual const ClassInfo& info() const = 0;
};
template< typename _Type, typename _BaseType >
class Factory : public AbstractFactory<_BaseType>
{
public:
explicit Factory() : AbstractFactory<_BaseType>() {}
virtual ~Factory() {}
virtual _BaseType* create(CogServer& cs) const { return new _Type(cs); }
virtual const ClassInfo& info() const { return _Type::info(); }
};
}
#endif