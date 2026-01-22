#ifndef _OC_CONCURRENT_STACK_H
#define _OC_CONCURRENT_STACK_H
#include <condition_variable>
#include <stack>
#include <exception>
#include <mutex>
template<typename Element>
class concurrent_stack
{
private:
std::stack<Element> the_stack;
mutable std::mutex the_mutex;
std::condition_variable the_cond;
bool is_canceled;
concurrent_stack(const concurrent_stack&) = delete;
concurrent_stack& operator=(const concurrent_stack&) = delete;
public:
concurrent_stack(void)
: the_stack(), the_mutex(), the_cond(), is_canceled(false)
{}
~concurrent_stack()
{ if (not is_canceled) cancel(); }
struct Canceled : public std::exception
{
const char * what() { return "Cancellation of wait on concurrent_stack"; }
};
void push(const Element& item)
{
std::unique_lock<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
the_stack.push(item);
lock.unlock();
the_cond.notify_one();
}
void push(Element&& item)
{
std::unique_lock<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
the_stack.push(std::move(item));
lock.unlock();
the_cond.notify_one();
}
bool is_empty() const
{
std::lock_guard<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
return the_stack.empty();
}
bool is_full() const noexcept { return false; }
size_t size() const
{
std::lock_guard<std::mutex> lock(the_mutex);
return the_stack.size();
}
bool try_pop(Element& value)
{
std::lock_guard<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
if (the_stack.empty())
{
return false;
}
value = the_stack.top();
the_stack.pop();
return true;
}
void pop(Element& value)
{
std::unique_lock<std::mutex> lock(the_mutex);
do
{
while (the_stack.empty() and not is_canceled)
{
the_cond.wait(lock);
}
if (is_canceled) throw Canceled();
}
while (the_stack.empty());
value = the_stack.top();
the_stack.pop();
}
void wait_pop(Element& value) { pop(value); }
Element value_pop()
{
Element value;
pop(value);
return value;
}
std::stack<Element> wait_and_take_all()
{
std::unique_lock<std::mutex> lock(the_mutex);
do
{
while (the_stack.empty() and not is_canceled)
{
the_cond.wait(lock);
}
if (is_canceled) break;
}
while (the_stack.empty());
std::stack<Element> retval;
the_stack.swap(retval);
return retval;
}
void barrier()
{
std::unique_lock<std::mutex> lock(the_mutex);
while (the_stack.empty() and not is_canceled)
{
the_cond.wait(lock);
}
if (is_canceled) throw Canceled();
}
void cancel_reset()
{
std::lock_guard<std::mutex> lock(the_mutex);
is_canceled = false;
}
void open() { cancel_reset(); }
void cancel()
{
std::unique_lock<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
is_canceled = true;
lock.unlock();
the_cond.notify_all();
}
void close() { cancel(); }
bool is_closed() const noexcept { return is_canceled; }
static bool is_lock_free() noexcept { return false; }
};
#endif