#ifndef _OPENCOG_PATTERN_TERM_H
#define _OPENCOG_PATTERN_TERM_H
#include <vector>
#include <opencog/util/Logger.h>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/core/Quotation.h>
namespace opencog {
class PatternTerm;
typedef std::shared_ptr<PatternTerm> PatternTermPtr;
typedef std::weak_ptr<PatternTerm> PatternTermWPtr;
typedef std::vector<PatternTermPtr> PatternTermSeq;
typedef std::vector<PatternTermWPtr> PatternTermWSeq;
typedef std::set<PatternTermPtr> PatternTermSet;
class PatternTerm
: public std::enable_shared_from_this<PatternTerm>
{
protected:
Handle _handle;
Handle _quote;
PatternTermPtr _parent;
PatternTermWSeq _outgoing;
Quotation _quotation;
bool _has_any_bound_var;
bool _has_bound_var;
bool _is_bound_var;
bool _has_any_globby_var;
bool _has_globby_var;
bool _is_globby_var;
bool _has_any_anon_var;
bool _has_anon_var;
bool _is_anon_var;
bool _has_any_evaluatable;
bool _has_evaluatable;
bool _is_virtual;
bool _is_identical;
bool _has_any_unordered_link;
bool _has_unordered_below;
bool _is_literal;
bool _is_present;
bool _is_absent;
bool _is_choice;
bool _has_choice;
bool _is_always;
bool _is_grouping;
void addAnyBoundVar();
void addAnyGlobbyVar();
void addAnyAnonVar();
void addAnyEvaluatable();
void addUnorderedBelow();
public:
static const PatternTermPtr UNDEFINED;
PatternTerm(void);
PatternTerm(const PatternTermPtr& parent, const Handle& h);
const Handle& getHandle() const noexcept { return _handle; }
PatternTermPtr getParent() const noexcept { return _parent; }
bool isDescendant(const PatternTermPtr&) const;
PatternTermPtr getRoot() noexcept {
PatternTermPtr root = shared_from_this();
while (root->_parent->_handle) root = _parent;
return root;
}
PatternTermPtr addOutgoingTerm(const Handle&);
PatternTermSeq getOutgoingSet() const;
Arity getArity() const { return _outgoing.size(); }
PatternTermPtr getOutgoingTerm(Arity pos) const;
const Handle& getQuote() const noexcept {
return (isQuoted() and nullptr != _quote) ? _quote : _handle; }
Quotation& getQuotation() { return _quotation; };
const Quotation& getQuotation() const noexcept { return _quotation; }
bool isQuoted() const { return _quotation.is_quoted(); }
void markLiteral();
bool isLiteral() const noexcept { return _is_literal; }
void markPresent();
bool isPresent() const noexcept { return _is_present; }
void markAbsent();
bool isAbsent() const noexcept { return _is_absent; }
void markChoice();
bool isChoice() const noexcept { return _is_choice; }
bool hasChoice() const noexcept { return _has_choice; }
void markAlways();
bool isAlways() const noexcept { return _is_always; }
void markGrouping();
bool isGrouping() const noexcept { return _is_grouping; }
void addBoundVariable();
bool hasAnyBoundVariable() const noexcept { return _has_any_bound_var; }
bool hasBoundVariable() const noexcept { return _has_bound_var; }
bool isBoundVariable() const noexcept { return _is_bound_var; }
void addGlobbyVar();
bool hasAnyGlobbyVar() const noexcept { return _has_any_globby_var; }
bool hasGlobbyVar() const noexcept { return _has_globby_var; }
bool isGlobbyVar() const noexcept { return _is_globby_var; }
void addAnonVar();
bool hasAnyAnonVar() const noexcept { return _has_any_anon_var; }
bool hasAnonVar() const noexcept { return _has_anon_var; }
bool isAnonVar() const noexcept { return _is_anon_var; }
void addEvaluatable();
bool hasAnyEvaluatable() const noexcept { return _has_any_evaluatable; }
bool hasEvaluatable() const noexcept { return _has_evaluatable; }
void markVirtual();
bool isVirtual() const noexcept { return _is_virtual; }
void markIdentical();
bool isIdentical() const noexcept { return _is_identical; }
void addUnorderedLink();
bool hasUnorderedLink() const noexcept { return _has_any_unordered_link; }
bool hasUnorderedBelow() const noexcept { return _has_unordered_below; }
bool isUnorderedLink() const noexcept { return _handle->is_unordered_link(); }
bool isLink() const noexcept { return _handle->is_link(); }
bool contained_in(const std::vector<PatternTermPtr>& vect) {
for (const PatternTermPtr& itm : vect)
if (itm->_handle == _handle) return true;
return false;
}
bool operator==(const PatternTerm&);
std::string to_string() const;
std::string to_string(const std::string& indent) const;
std::string to_short_string() const;
std::string to_short_string(const std::string& indent) const;
std::string to_full_string() const;
std::string to_full_string(const std::string& indent) const;
std::string flag_string() const;
};
#define createPatternTerm std::make_shared<PatternTerm>
std::string oc_to_string(const PatternTerm& pt,
const std::string& indent=empty_string);
std::string oc_to_string(const PatternTermPtr& pt,
const std::string& indent=empty_string);
std::string oc_to_string(const PatternTermSeq& pt,
const std::string& indent=empty_string);
std::string oc_to_string(const PatternTermSet& pt,
const std::string& indent=empty_string);
}
namespace std {
template<>
struct less<opencog::PatternTermPtr>
{
bool operator()(const opencog::PatternTermPtr& lhs, const opencog::PatternTermPtr& rhs) const
{
const opencog::Handle& lHandle = lhs->getHandle();
const opencog::Handle& rHandle = rhs->getHandle();
if (lHandle == rHandle)
{
if (not lHandle) return false;
return lhs->getParent() < rhs->getParent();
}
return lHandle < rHandle;
}
};
template<>
struct hash<opencog::PatternTermPtr>
{
std::size_t
operator()(const opencog::PatternTermPtr& ptm) const noexcept
{ return ptm->getHandle()->get_hash(); }
};
template<>
struct equal_to<opencog::PatternTermPtr>
{
bool
operator()(const opencog::PatternTermPtr& lptm,
const opencog::PatternTermPtr& rptm) const noexcept
{ return lptm->operator==(*rptm); }
};
template<>
struct hash<opencog::PatternTermSeq>
{
std::size_t
operator()(const opencog::PatternTermSeq& seq) const noexcept
{
std::size_t hash = 0;
for (const opencog::PatternTermPtr& ptm : seq)
hash += ptm->getHandle()->get_hash();
return hash;
}
};
template<>
struct equal_to<opencog::PatternTermSeq>
{
bool
operator()(const opencog::PatternTermSeq& lseq,
const opencog::PatternTermSeq& rseq) const noexcept
{
size_t lsz = lseq.size();
if (lsz != rseq.size()) return false;
for (size_t i=0; i<lsz; i++)
{
const opencog::PatternTermPtr& lptm(lseq[i]);
const opencog::PatternTermPtr& rptm(rseq[i]);
if (not lptm->operator==(*rptm)) return false;
}
return true;
}
};
};
#endif