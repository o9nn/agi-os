#ifndef _OPENCOG_TYPEINDEX_H
#define _OPENCOG_TYPEINDEX_H
#include <mutex>
#include <set>
#include <vector>
#if HAVE_FOLLY
#include <folly/container/F14Set.h>
#endif
#if HAVE_SPARSEHASH
#include <sparsehash/sparse_hash_set>
#endif
#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>
namespace opencog
{
#if USE_FOLLY
typedef folly::F14ValueSet<Handle> AtomHanSet;
#endif
#if USE_SPARSE_TYPESET
typedef google::sparse_hash_set<Handle> AtomHanSet;
#endif
#if not (USE_SPARSE_TYPESET || USE_FOLLY)
typedef std::unordered_set<Handle> AtomHanSet;
#endif
struct AtomSet : AtomHanSet
{
mutable std::shared_mutex _mtx;
#if USE_SPARSE_TYPESET
AtomSet() { set_deleted_key(Handle()); }
#else
AtomSet() = default;
#endif
AtomSet(AtomSet&& other) noexcept :
AtomHanSet(std::move(other))
{}
};
#define TYPE_INDEX_SHARED_LOCK(s) std::shared_lock<std::shared_mutex> lck(s._mtx);
#define TYPE_INDEX_UNIQUE_LOCK(s) std::unique_lock<std::shared_mutex> lck(s._mtx);
class TypeIndex
{
private:
mutable int _num_types;
mutable int _reserved;
int _offset_to_atom;
NameServer& _nameserver;
mutable std::vector<AtomSet> _idx;
static constexpr int TYPE_RESERVE_SIZE = 1024;
static constexpr int POOL_SIZE = 8;
static constexpr int VEC_SIZE = TYPE_RESERVE_SIZE * POOL_SIZE;
int get_bucket_start(Type t) const
{
OC_ASSERT(_offset_to_atom <= t, "BUG with type buckets!");
if (_reserved + _offset_to_atom <= t) resize();
return POOL_SIZE * (t - _offset_to_atom);
}
int get_bucket(const Handle& h) const
{
int ibu = h->get_hash() % POOL_SIZE;
Type hty = h->get_type();
if (_reserved + _offset_to_atom <= hty) resize();
ibu += POOL_SIZE * (hty - _offset_to_atom);
return ibu;
}
AtomSet& get_atom_set(const Handle& h)
{
return _idx[get_bucket(h)];
}
const AtomSet& get_atom_set_const(const Handle& h) const
{
return _idx[get_bucket(h)];
}
public:
TypeIndex(void);
void resize(void) const;
Handle insertAtom(const Handle& h)
{
AtomSet& s(get_atom_set(h));
TYPE_INDEX_UNIQUE_LOCK(s);
auto iter = s.find(h);
if (s.end() != iter) return *iter;
s.insert(h);
return Handle::UNDEFINED;
}
bool removeAtom(const Handle& h)
{
AtomSet& s(get_atom_set(h));
TYPE_INDEX_UNIQUE_LOCK(s);
return 1 == s.erase(h);
}
Handle findAtom(const Handle& h) const
{
const AtomSet& s(get_atom_set_const(h));
TYPE_INDEX_SHARED_LOCK(s);
auto iter = s.find(h);
if (s.end() == iter) return Handle::UNDEFINED;
return *iter;
}
size_t size(Type t) const
{
if (t < _offset_to_atom) return 0;
size_t cnt = 0;
int start = get_bucket_start(t);
for (int ibu = start; ibu < start + POOL_SIZE; ibu++)
{
const AtomSet& s(_idx[ibu]);
TYPE_INDEX_SHARED_LOCK(s);
cnt += s.size();
}
return cnt;
}
size_t size(void) const
{
size_t cnt = 0;
for (const auto& s : _idx)
{
TYPE_INDEX_SHARED_LOCK(s);
cnt += s.size();
}
return cnt;
}
size_t size(Type type, bool subclass) const
{
size_t result = 0;
if (_offset_to_atom <= type)
result = size(type);
if (not subclass) return result;
for (Type t = type+1; t<_num_types; t++)
{
if (t != type and _nameserver.isA(t, type))
result += size(t);
}
return result;
}
void clear(void);
void get_handles_by_type(HandleSeq&, Type, bool subclass) const;
void get_handles_by_type(UnorderedHandleSet&, Type, bool subclass) const;
void get_rootset_by_type(HandleSeq&, Type, bool subclass,
const AtomSpace*) const;
};
}
#endif