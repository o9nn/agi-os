#ifndef _OPENCOG_ATOM_BINS_H
#define _OPENCOG_ATOM_BINS_H
#include <vector>
#include <mutex>
#include <atomic>
#include <memory>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
class AtomBins
{
private:
mutable std::mutex _mtx;
HandleSetSeq _idx;
public:
AtomBins(size_t sz)
{
_idx.resize(sz);
}
void insert(size_t i, const Handle& a)
{
std::lock_guard<std::mutex> lck(_mtx);
_idx.at(i).insert(a);
}
void remove(size_t i, const Handle& a)
{
std::lock_guard<std::mutex> lck(_mtx);
_idx.at(i).erase(a);
}
size_t size(size_t i) const
{
std::lock_guard<std::mutex> lck(_mtx);
return _idx.at(i).size();
}
Handle getRandomAtom(void) const;
size_t size() const;
template <typename OutputIterator> OutputIterator
getContent(size_t i, OutputIterator out) const
{
std::lock_guard<std::mutex> lck(_mtx);
const HandleSet& s(_idx.at(i));
return std::copy(s.begin(), s.end(), out);
}
template <typename OutputIterator> OutputIterator
getContentIf(size_t i,
OutputIterator out,
std::function<bool(const Handle&)> pred) const
{
std::lock_guard<std::mutex> lck(_mtx);
const HandleSet& s(_idx.at(i));
return std::copy_if(s.begin(), s.end(), out, pred);
}
};
}
#endif