#ifndef _OPENCOG_CONTAINER_VALUE_H
#define _OPENCOG_CONTAINER_VALUE_H
#include <opencog/atoms/value/LinkValue.h>
#include <opencog/atoms/atom_types/atom_types.h>
namespace opencog
{
class ContainerValue
: public LinkValue
{
protected:
ContainerValue(Type t) : LinkValue(t) {}
public:
ContainerValue(void) : LinkValue(CONTAINER_VALUE) {}
virtual ~ContainerValue() {}
virtual void open(void) = 0;
virtual void close(void) = 0;
virtual bool is_closed(void) const = 0;
virtual void add(const ValuePtr&) = 0;
virtual void add(ValuePtr&&) = 0;
virtual ValuePtr remove(void) = 0;
virtual void clear(void) = 0;
virtual bool operator==(const Value&) const;
};
VALUE_PTR_DECL(ContainerValue);
}
#endif