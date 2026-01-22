#ifndef _OPENCOG_INCOMEINDEX_H
#define _OPENCOG_INCOMEINDEX_H
#include <mutex>
#include <set>
#include <vector>
#include <opencog/util/oc_assert.h>
#include <opencog/atoms/base/Atom.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/atom_types/types.h>
namespace opencog
{
#if USE_SPARSE_IIDX
typedef google::sparse_hash_map<Handle, InSetMap> InSetIdx;
#else
typedef std::map<Handle, InSetMap> InSetIdx;
#endif
struct InSet : InSetIdx
{
mutable std::shared_mutex _mtx;
#if USE_SPARSE_IIDX
InSet(void) { set_deleted_key(Handle()); }
#endif
};
#define INCOME_INDEX_SHARED_LOCK(s) std::shared_lock<std::shared_mutex> lck(s._mtx);
#define INCOME_INDEX_UNIQUE_LOCK(s) std::unique_lock<std::shared_mutex> lck(s._mtx);
class IncomeIndex
{
private:
mutable std::vector<InSet> _idx;
static constexpr int POOL_SIZE = 32;
InSet& get_inset(const Handle& h) const
{
return _idx[h->get_hash() % POOL_SIZE];
}
public:
IncomeIndex(void);
void removeInset(const Handle& h)
{
InSet& s(get_inset(h));
INCOME_INDEX_UNIQUE_LOCK(s);
s.erase(h);
}
bool haveInset(const Handle& h) const
{
InSet& s(get_inset(h));
INCOME_INDEX_SHARED_LOCK(s);
const auto inset = s.find(h);
return s.end() != inset;
}
InSetMap& getInset(const Handle& h)
{
InSet& s(get_inset(h));
INCOME_INDEX_UNIQUE_LOCK(s);
InSetMap iset;
auto iter = s.find(h);
if (s.end() == iter)
{
s.insert({h, InSetMap()});
iter = s.find(h);
}
return iter->second;
}
void swapInset(const Handle& oldh, const Handle& newh)
{
InSet& s(get_inset(oldh));
INCOME_INDEX_UNIQUE_LOCK(s);
InSetMap iset;
auto iter = s.find(oldh);
if (s.end() == iter)
return;
s.erase(oldh);
s.insert({newh, iter->second});
}
size_t size(void) const
{
size_t cnt = 0;
for (const InSet& s : _idx)
{
INCOME_INDEX_SHARED_LOCK(s);
cnt += s.size();
}
return cnt;
}
void clear(void);
};
}
#endif