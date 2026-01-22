#ifndef _OPENCOG_TRUTH_VALUE_OF_LINK_H
#define _OPENCOG_TRUTH_VALUE_OF_LINK_H
#include <opencog/atoms/flow/ValueOfLink.h>
namespace opencog
{
class TruthValueOfLink : public ValueOfLink
{
public:
TruthValueOfLink(const HandleSeq&&, Type=TRUTH_VALUE_OF_LINK);
TruthValueOfLink(const TruthValueOfLink &) = delete;
TruthValueOfLink operator=(const TruthValueOfLink &) = delete;
virtual bool is_evaluatable() const { return true; }
virtual TruthValuePtr evaluate(AtomSpace*, bool);
virtual ValuePtr execute(AtomSpace* as, bool silent) {
return ValueCast(evaluate(as, silent));
}
static Handle factory(const Handle&);
};
LINK_PTR_DECL(TruthValueOfLink)
#define createTruthValueOfLink CREATE_DECL(TruthValueOfLink)
class StrengthOfLink : public ValueOfLink
{
public:
StrengthOfLink(const HandleSeq&&, Type=STRENGTH_OF_LINK);
StrengthOfLink(const StrengthOfLink&) = delete;
StrengthOfLink& operator=(const StrengthOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(StrengthOfLink)
#define createStrengthOfLink CREATE_DECL(StrengthOfLink)
class ConfidenceOfLink : public ValueOfLink
{
public:
ConfidenceOfLink(const HandleSeq&&, Type=CONFIDENCE_OF_LINK);
ConfidenceOfLink(const ConfidenceOfLink&) = delete;
ConfidenceOfLink operator=(const ConfidenceOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(ConfidenceOfLink)
#define createConfidenceOfLink CREATE_DECL(ConfidenceOfLink)
class CountOfLink : public ValueOfLink
{
public:
CountOfLink(const HandleSeq&&, Type=COUNT_OF_LINK);
CountOfLink(const CountOfLink&) = delete;
CountOfLink& operator=(const CountOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(CountOfLink)
#define createCountOfLink CREATE_DECL(CountOfLink)
}
#endif