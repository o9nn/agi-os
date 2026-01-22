#ifndef __COGUTIL_SIGSLOT_H__
#define __COGUTIL_SIGSLOT_H__
#include <functional>
#include <map>
#include <mutex>
template <typename... ARGS>
class SigSlot
{
public:
typedef std::function<void(ARGS...)> slot_type;
private:
mutable std::mutex _mtx;
mutable std::map<int, std::function<void(ARGS...)>> _slots;
mutable int _slot_id;
public:
SigSlot() : _slot_id(0) {}
int connect(std::function<void(ARGS...)> const& fn)
{
std::lock_guard<std::mutex> lck(_mtx);
_slot_id++;
_slots.insert(std::make_pair(_slot_id, fn));
return _slot_id;
}
template <typename Class>
int connect(void (Class::*fn)(ARGS...), Class* obj)
{
return connect([obj, fn](ARGS... args) { (obj->*fn)(args...); });
}
void disconnect(int slotid)
{
std::lock_guard<std::mutex> lck(_mtx);
auto it = _slots.find(slotid);
if (it != _slots.end())
_slots.erase(it);
}
void disconnect_all()
{
std::lock_guard<std::mutex> lck(_mtx);
_slots.clear();
}
void emit(ARGS... p)
{
if (0 == _slots.size()) return;
std::lock_guard<std::mutex> lck(_mtx);
for (auto it : _slots) it.second(p...);
}
size_t size() { return _slots.size(); }
};
#endif