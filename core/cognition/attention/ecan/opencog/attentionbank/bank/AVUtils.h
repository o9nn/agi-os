#ifndef _OPENCOG_AVUTILS_H
#define _OPENCOG_AVUTILS_H
#include <opencog/attentionbank/avalue/AttentionValue.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>
namespace opencog
{
AttentionValuePtr get_av(const Handle&);
void set_av(AtomSpace*, const Handle&, const AttentionValuePtr&);
static inline AttentionValue::sti_t get_sti(const Handle& h)
{
return get_av(h)->getSTI();
}
static inline AttentionValue::lti_t get_lti(const Handle& h)
{
return get_av(h)->getLTI();
}
static inline AttentionValue::vlti_t get_vlti(const Handle& h)
{
return get_av(h)->getVLTI();
}
}
#endif