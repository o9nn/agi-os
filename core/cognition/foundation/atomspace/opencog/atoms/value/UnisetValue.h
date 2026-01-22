#ifndef _OPENCOG_UNISET_VALUE_H
#define _OPENCOG_UNISET_VALUE_H
#include <opencog/util/concurrent_set.h>
#include <opencog/atoms/value/ContainerValue.h>
#include <opencog/atoms/atom_types/atom_types.h>
namespace opencog
{
class UnisetValue
: public ContainerValue, protected concurrent_set<ValuePtr>
{
protected:
UnisetValue(Type t) : ContainerValue(t) {}
virtual void update() const;
public:
UnisetValue(void) : ContainerValue(UNISET_VALUE) {}
UnisetValue(const ValueSeq&);
virtual ~UnisetValue() {}
virtual void open(void);
virtual void close(void);
virtual bool is_closed(void) const;
virtual void add(const ValuePtr&);
virtual void add(ValuePtr&&);
virtual ValuePtr remove(void);
virtual size_t size(void) const;
virtual void clear(void);
virtual bool operator==(const Value&) const;
};
VALUE_PTR_DECL(UnisetValue);
CREATE_VALUE_DECL(UnisetValue);
}
#endif