#ifndef _OC_CONCURRENT_QUEUE_H
#define _OC_CONCURRENT_QUEUE_H
#include <condition_variable>
#include <queue>
#include <exception>
#include <mutex>
template<typename Element>
class concurrent_queue
{
private:
std::queue<Element> the_queue;
mutable std::mutex the_mutex;
std::condition_variable the_cond;
bool is_canceled;
concurrent_queue(const concurrent_queue&) = delete;
concurrent_queue& operator=(const concurrent_queue&) = delete;
public:
concurrent_queue(void)
: the_queue(), the_mutex(), the_cond(), is_canceled(false)
{}
~concurrent_queue()
{ if (not is_canceled) cancel(); }
struct Canceled : public std::exception
{
const char * what() { return "Cancellation of wait on concurrent_queue"; }
};
void push(const Element& item)
{
std::unique_lock<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
the_queue.push(item);
lock.unlock();
the_cond.notify_one();
}
void push(Element&& item)
{
std::unique_lock<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
the_queue.push(std::move(item));
lock.unlock();
the_cond.notify_one();
}
bool is_empty() const
{
std::lock_guard<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
return the_queue.empty();
}
bool is_full() const noexcept { return false; }
size_t size() const
{
std::lock_guard<std::mutex> lock(the_mutex);
return the_queue.size();
}
bool try_get(Element& value)
{
std::lock_guard<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
if (the_queue.empty())
{
return false;
}
value = the_queue.front();
the_queue.pop();
return true;
}
bool try_pop(Element& value) { return try_get(value); }
void pop(Element& value)
{
std::unique_lock<std::mutex> lock(the_mutex);
do
{
while (the_queue.empty() and not is_canceled)
{
the_cond.wait(lock);
}
if (is_canceled) throw Canceled();
}
while (the_queue.empty());
value = the_queue.front();
the_queue.pop();
}
void wait_pop(Element& value) { pop(value); }
Element value_pop()
{
Element value;
pop(value);
return value;
}
std::queue<Element> wait_and_take_all()
{
std::unique_lock<std::mutex> lock(the_mutex);
do
{
while (the_queue.empty() and not is_canceled)
{
the_cond.wait(lock);
}
if (is_canceled) break;
}
while (the_queue.empty());
std::queue<Element> retval;
std::swap(retval, the_queue);
return retval;
}
void barrier()
{
std::unique_lock<std::mutex> lock(the_mutex);
while (the_queue.empty() and not is_canceled)
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