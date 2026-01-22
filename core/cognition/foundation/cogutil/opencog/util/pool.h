#ifndef _OPENCOG_UTIL_POOL_H
#define _OPENCOG_UTIL_POOL_H
#include <queue>
#include <mutex>
#include <condition_variable>
namespace opencog {
template<typename Resource>
class pool
{
public:
Resource& borrow()
{
std::unique_lock<std::mutex> lock(mu);
while (objs.empty()) {
cond.wait(lock);
}
Resource& rv = objs.front();
objs.pop();
return rv;
}
void give_back(const Resource& obj)
{
std::lock_guard<std::mutex> lock(mu);
objs.push(obj);
cond.notify_one();
}
size_t available()
{
return objs.size();
}
private:
std::mutex mu;
std::condition_variable cond;
std::queue<Resource> objs;
};
}
#endif