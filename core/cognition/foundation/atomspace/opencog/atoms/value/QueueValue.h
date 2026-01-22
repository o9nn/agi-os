#ifndef _OPENCOG_QUEUE_VALUE_H
#define _OPENCOG_QUEUE_VALUE_H
#include <opencog/util/concurrent_queue.h>
#include <opencog/atoms/value/ContainerValue.h>
#include <opencog/atoms/atom_types/atom_types.h>
namespace opencog
{
class QueueValue
: public ContainerValue, protected concurrent_queue<ValuePtr>
{
protected:
QueueValue(Type t) : ContainerValue(t) {}
virtual void update() const;
public:
QueueValue(void) : ContainerValue(QUEUE_VALUE) {}
QueueValue(const ValueSeq&);
virtual ~QueueValue() {}
virtual void open(void);
virtual void close(void);
virtual bool is_closed(void) const;
virtual void add(const ValuePtr&);
virtual void add(ValuePtr&&);
virtual ValuePtr remove(void);
virtual size_t size(void) const;
virtual void clear(void);
virtual std::string to_string(const std::string& = "") const;
virtual bool operator==(const Value&) const;
};
VALUE_PTR_DECL(QueueValue);
CREATE_VALUE_DECL(QueueValue);
}
#endif