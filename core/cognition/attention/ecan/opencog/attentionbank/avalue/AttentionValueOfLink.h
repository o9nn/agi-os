#ifndef _OPENCOG_ATTENTION_VALUE_OF_LINK_H
#define _OPENCOG_ATTENTION_VALUE_OF_LINK_H
#include <opencog/atoms/flow/ValueOfLink.h>
#include <opencog/attentionbank/types/atom_types.h>
namespace opencog
{
class AttentionValueOfLink : public ValueOfLink
{
public:
AttentionValueOfLink(const HandleSeq&&, Type=ATTENTION_VALUE_OF_LINK);
AttentionValueOfLink(const AttentionValueOfLink&) = delete;
AttentionValueOfLink& operator=(const AttentionValueOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
typedef std::shared_ptr<AttentionValueOfLink> AttentionValueOfLinkPtr;
static inline AttentionValueOfLinkPtr AttentionValueOfLinkCast(const Handle& h)
{ return std::dynamic_pointer_cast<AttentionValueOfLink>(h); }
static inline AttentionValueOfLinkPtr AttentionValueOfLinkCast(AtomPtr a)
{ return std::dynamic_pointer_cast<AttentionValueOfLink>(a); }
#define createAttentionValueOfLink std::make_shared<AttentionValueOfLink>
class StiOfLink : public ValueOfLink
{
public:
StiOfLink(const HandleSeq&&, Type=STRENGTH_OF_LINK);
StiOfLink(const StiOfLink&) = delete;
StiOfLink& operator=(const StiOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
typedef std::shared_ptr<StiOfLink> StiOfLinkPtr;
static inline StiOfLinkPtr StiOfLinkCast(const Handle& h)
{ return std::dynamic_pointer_cast<StiOfLink>(h); }
static inline StiOfLinkPtr StiOfLinkCast(AtomPtr a)
{ return std::dynamic_pointer_cast<StiOfLink>(a); }
#define createStiOfLink std::make_shared<StiOfLink>
class LtiOfLink : public ValueOfLink
{
public:
LtiOfLink(const HandleSeq&&, Type=CONFIDENCE_OF_LINK);
LtiOfLink(const LtiOfLink&) = delete;
LtiOfLink& operator=(const LtiOfLink&) = delete;
virtual ValuePtr execute(AtomSpace*, bool);
static Handle factory(const Handle&);
};
typedef std::shared_ptr<LtiOfLink> LtiOfLinkPtr;
static inline LtiOfLinkPtr LtiOfLinkCast(const Handle& h)
{ return std::dynamic_pointer_cast<LtiOfLink>(h); }
static inline LtiOfLinkPtr LtiOfLinkCast(AtomPtr a)
{ return std::dynamic_pointer_cast<LtiOfLink>(a); }
#define createLtiOfLink std::make_shared<LtiOfLink>
}
#endif