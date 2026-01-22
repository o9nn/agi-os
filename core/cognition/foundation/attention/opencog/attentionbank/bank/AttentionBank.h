#ifndef _OPENCOG_ATTENTION_BANK_H
#define _OPENCOG_ATTENTION_BANK_H
#include <mutex>
#include <unordered_map>
#include <opencog/util/sigslot.h>
#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/attentionbank/bank/ImportanceIndex.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
typedef SigSlot<const Handle&,
const AttentionValuePtr&,
const AttentionValuePtr&> AVCHSigl;
typedef SigSlot<const Handle&,
const AttentionValuePtr&,
const AttentionValuePtr&> AFCHSigl;
class AtomSpace;
class AttentionBank
{
std::mutex _mtx;
std::mutex AFMutex;
unsigned int maxAFSize;
struct compare_sti_less {
bool operator()(const std::pair<Handle, AttentionValuePtr>& h1,
const std::pair<Handle, AttentionValuePtr>& h2) const
{
return  (h1.second)->getSTI() < (h2.second)->getSTI();
}
};
std::multiset<std::pair<Handle, AttentionValuePtr>, compare_sti_less> attentionalFocus;
void updateAttentionalFocus(const Handle&, const AttentionValuePtr&,
const AttentionValuePtr&);
void AVChanged(const Handle&, const AttentionValuePtr&, const AttentionValuePtr&);
AFCHSigl _AddAFSignal;
AFCHSigl _RemoveAFSignal;
AttentionValue::sti_t fundsSTI;
AttentionValue::lti_t fundsLTI;
AttentionValue::sti_t startingFundsSTI;
AttentionValue::lti_t startingFundsLTI;
AttentionValue::sti_t stiFundsBuffer;
AttentionValue::lti_t ltiFundsBuffer;
AttentionValue::sti_t targetSTI;
AttentionValue::lti_t targetLTI;
AttentionValue::sti_t STIAtomWage;
AttentionValue::lti_t LTIAtomWage;
ImportanceIndex _importanceIndex;
AVCHSigl _AVChangedSignal;
AtomSpace* _as;
void change_vlti(const Handle&, int);
void remove_atom_from_bank(const AtomPtr& atom);
public:
AttentionBank(AtomSpace*);
~AttentionBank();
#ifdef ECAN_EXPERIMENT
std::map<Handle, AttentionValue::sti_t> stimulusRec;
#endif
AFCHSigl& AddAFSignal() { return _AddAFSignal; }
AFCHSigl& RemoveAFSignal() { return _RemoveAFSignal; }
AVCHSigl& getAVChangedSignal() { return _AVChangedSignal; }
AttentionValue::sti_t get_af_max_sti(void) const
{
if (attentionalFocus.rbegin() != attentionalFocus.rend())
return (attentionalFocus.rbegin()->second)->getSTI();
else
return 0;
}
AttentionValue::sti_t get_af_min_sti(void) const
{
if (attentionalFocus.rbegin() != attentionalFocus.rend())
return (attentionalFocus.begin()->second)->getSTI();
else
return 0;
}
void set_af_size(int size) {
maxAFSize = size;
}
int get_af_size(void) {
return maxAFSize;
}
void change_av(const Handle&, const AttentionValuePtr& new_av);
void set_sti(const Handle&, AttentionValue::sti_t);
void set_lti(const Handle&, AttentionValue::lti_t);
void inc_vlti(const Handle& h) { change_vlti(h, +1); }
void dec_vlti(const Handle& h) { change_vlti(h, -1); }
void stimulate(const Handle&, double stimulus);
AttentionValue::sti_t getTotalSTI() const {
return startingFundsSTI - (AttentionValue::sti_t)fundsSTI;
}
AttentionValue::lti_t getTotalLTI() const {
return startingFundsLTI - (AttentionValue::lti_t)fundsLTI;
}
AttentionValue::sti_t getSTIFunds() const { return fundsSTI; }
AttentionValue::lti_t getLTIFunds() const { return fundsLTI; }
AttentionValue::sti_t getSTIFundsBuffer(){ return stiFundsBuffer;}
AttentionValue::lti_t getLTIFundsBuffer(){ return ltiFundsBuffer;}
AttentionValue::sti_t calculateSTIWage(void);
AttentionValue::lti_t calculateLTIWage(void);
double getNormalisedSTI(AttentionValuePtr, bool average, bool clip) const;
double getNormalisedSTI(AttentionValuePtr) const;
double getNormalisedZeroToOneSTI(AttentionValuePtr, bool average, bool clip) const;
bool atom_is_in_AF(const Handle&);
template <typename OutputIterator> OutputIterator
get_handle_set_in_attentional_focus(OutputIterator result)
{
std::lock_guard<std::mutex> lock(AFMutex);
for (const auto& p : attentionalFocus) {
*result++ = p.first;
}
return result;
}
ImportanceIndex& getImportance()
{
return _importanceIndex;
}
Handle getRandomAtomNotInAF(void);
AttentionValue::sti_t getMinSTI(bool average=true) const
{
return _importanceIndex.getMinSTI(average);
}
AttentionValue::sti_t getMaxSTI(bool average=true) const
{
return _importanceIndex.getMaxSTI(average);
}
UnorderedHandleSet getHandlesByAV(AttentionValue::sti_t lowerBound,
AttentionValue::sti_t upperBound = AttentionValue::MAXSTI) const
{
return _importanceIndex.getHandleSet(lowerBound, upperBound);
}
template <typename OutputIterator> OutputIterator
get_handles_by_AV(OutputIterator result,
AttentionValue::sti_t lowerBound,
AttentionValue::sti_t upperBound = AttentionValue::MAXSTI) const
{
UnorderedHandleSet hs = getHandlesByAV(lowerBound, upperBound);
return std::copy(hs.begin(), hs.end(), result);
}
};
AttentionBank& attentionbank(AtomSpace*);
}
#endif