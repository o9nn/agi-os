#ifndef _OPENCOG_IMPORTANCEINDEX_H
#define _OPENCOG_IMPORTANCEINDEX_H
#include <mutex>
#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/attentionbank/bank/AtomBins.h>
#include <opencog/attentionbank/bank/AVUtils.h>
namespace opencog
{
template<class ValueType> struct recent_val {
ValueType val;
float recent;
float decay;
recent_val(ValueType x): val(x), recent((float)x), decay(0.5f) {}
recent_val(): val(0), recent(0.0f), decay(0.5f) {}
inline void update(ValueType x) {
val = x;
recent = ((decay) * val) + ((1.0f - decay) * recent);
}
};
using HandleSTIPair = std::pair<Handle, AttentionValue::sti_t>;
namespace ecan {
class StochasticDiffusionAmountCalculator;
};
class ImportanceIndex
{
friend class ecan::StochasticDiffusionAmountCalculator;
private:
mutable std::mutex _mtx;
AtomBins _index;
opencog::recent_val<AttentionValue::sti_t> _maxSTI;
opencog::recent_val<AttentionValue::sti_t> _minSTI;
static size_t importanceBin(AttentionValue::sti_t);
public:
ImportanceIndex();
void removeAtom(const Handle&);
void update(void);
AttentionValue::sti_t getMaxSTI(bool average=true) const;
AttentionValue::sti_t getMinSTI(bool average=true) const;
void updateImportance(const Handle&,
const AttentionValuePtr& oldav,
const AttentionValuePtr& newav);
UnorderedHandleSet getHandleSet(AttentionValue::sti_t lowerBound,
AttentionValue::sti_t upperBound =
AttentionValue::MAXSTI) const;
template <typename OutputIterator> OutputIterator
getContent(size_t i,OutputIterator out) const
{
return _index.getContent(i,out);
}
Handle getRandomAtom(void) const;
UnorderedHandleSet getMaxBinContents();
UnorderedHandleSet getMinBinContents();
size_t bin_size(void) const;
size_t size(int) const;
};
}
#endif