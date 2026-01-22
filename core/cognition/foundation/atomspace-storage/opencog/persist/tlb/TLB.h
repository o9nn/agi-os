#ifndef _OPENCOG_TLB_H
#define _OPENCOG_TLB_H
#include <atomic>
#include <mutex>
#include <unordered_map>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
class uuid_pool
{
public:
virtual ~uuid_pool() {}
virtual UUID get_uuid(void) = 0;
};
class local_uuid_pool : public uuid_pool
{
private:
std::atomic<UUID> _brk_uuid;
public:
local_uuid_pool(void) : _brk_uuid(1) {}
UUID get_uuid(void)
{
return _brk_uuid.fetch_add(1, std::memory_order_relaxed);
};
};
class AtomSpace;
class TLB
{
private:
local_uuid_pool _local_pool;
uuid_pool* _uuid_pool;
std::mutex _mtx;
std::unordered_map<UUID, Handle> _uuid_map;
std::unordered_map<Handle, UUID,
std::hash<opencog::Handle>,
std::equal_to<opencog::Handle> > _handle_map;
std::vector<const AtomSpace*> _resolver;
Handle do_res(const Handle&);
public:
static const UUID INVALID_UUID = ULONG_MAX;
TLB(uuid_pool* = nullptr);
void set_resolver(const AtomSpace*);
void clear_resolver(const AtomSpace*);
size_t size() { return _uuid_map.size(); }
void clear();
UUID addAtom(const AtomPtr& a, UUID uuid) {
return addAtom(a->get_handle(), uuid);
}
UUID addAtom(const Handle&, UUID);
Handle getAtom(UUID);
UUID getUUID(const Handle&);
void removeAtom(const AtomPtr& a) {
return removeAtom(a->get_handle());
}
void removeAtom(const Handle&);
void removeAtom(UUID);
void purgeAtom(UUID);
};
}
#endif