#ifndef _OC_CONCURRENT_SET_H
#define _OC_CONCURRENT_SET_H
#include <condition_variable>
#include <set>
#include <exception>
#include <mutex>
#include <vector>
template<typename Element>
class concurrent_set
{
private:
std::set<Element> the_set;
mutable std::mutex the_mutex;
std::condition_variable the_cond;
bool is_canceled;
concurrent_set(const concurrent_set&) = delete;
concurrent_set& operator=(const concurrent_set&) = delete;
public:
concurrent_set(void)
: the_set(), the_mutex(), the_cond(), is_canceled(false)
{}
~concurrent_set()
{ if (not is_canceled) cancel(); }
struct Canceled : public std::exception
{
const char * what() { return "Cancellation of wait on concurrent_set"; }
};
bool insert(const Element& item)
{
std::unique_lock<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
size_t before = the_set.size();
the_set.insert(item);
size_t after = the_set.size();
lock.unlock();
the_cond.notify_one();
return before < after;
}
bool insert(Element&& item)
{
std::unique_lock<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
size_t before = the_set.size();
the_set.insert(std::move(item));
size_t after = the_set.size();
lock.unlock();
the_cond.notify_one();
return before < after;
}
size_t erase(const Element& item)
{
std::unique_lock<std::mutex> lock(the_mutex);
return the_set.erase(item);
}
bool is_empty() const
{
std::lock_guard<std::mutex> lock(the_mutex);
if (is_canceled) throw Canceled();
return the_set.empty();
}
bool is_full() const noexcept { return false; }
size_t size() const
{
std::lock_guard<std::mutex> lock(the_mutex);
return the_set.size();
}
void clear()
{
std::lock_guard<std::mutex> lock(the_mutex);
return the_set.clear();
}
bool try_get(Element& value, bool reverse = false)
{
std::lock_guard<std::mutex> lock(the_mutex);
if (the_set.empty())
return false;
if (reverse)
{
typename std::set<Element>::const_reverse_iterator it = the_set.crbegin();
value = *it;
the_set.erase(value);
}
else
{
typename std::set <Element>::const_iterator it = the_set.cbegin();
value = *it;
the_set.erase(it);
}
return true;
}
std::vector<Element> try_get(size_t nelt, bool reverse = false)
{
std::vector<Element> elvec;
std::lock_guard<std::mutex> lock(the_mutex);
if (the_set.empty())
return elvec;
if (the_set.size() < nelt) nelt = the_set.size();
if (reverse)
{
for (size_t j=0; j<nelt; j++)
{
typename std::set<Element>::const_reverse_iterator it = the_set.crbegin();
Element value = *it;
the_set.erase(value);
elvec.emplace_back(value);
}
}
else
{
for (size_t j=0; j<nelt; j++)
{
typename std::set <Element>::const_iterator it = the_set.cbegin();
Element value = *it;
the_set.erase(it);
elvec.emplace_back(value);
}
}
return elvec;
}
void get(Element& value)
{
std::unique_lock<std::mutex> lock(the_mutex);
do
{
while (the_set.empty() and not is_canceled)
{
the_cond.wait(lock);
}
if (is_canceled) throw Canceled();
}
while (the_set.empty());
auto it = the_set.begin();
value = *it;
the_set.erase(it);
}
void wait_get(Element& value) { get(value); }
Element value_get()
{
Element value;
get(value);
return value;
}
std::set<Element> wait_and_take_all()
{
std::unique_lock<std::mutex> lock(the_mutex);
do
{
while (the_set.empty() and not is_canceled)
{
the_cond.wait(lock);
}
if (is_canceled) break;
}
while (the_set.empty());
std::set<Element> retval;
std::swap(retval, the_set);
return retval;
}
void barrier()
{
std::unique_lock<std::mutex> lock(the_mutex);
while (the_set.empty() and not is_canceled)
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