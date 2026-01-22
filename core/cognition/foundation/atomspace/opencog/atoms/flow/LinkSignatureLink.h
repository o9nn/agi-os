#ifndef _OPENCOG_LINK_SIGNATURE_LINK_H
#define _OPENCOG_LINK_SIGNATURE_LINK_H
#include <opencog/atoms/base/Link.h>
namespace opencog
{
class LinkSignatureLink : public Link
{
private:
Type _kind;
public:
LinkSignatureLink(const HandleSeq&&, Type = LINK_SIGNATURE_LINK);
LinkSignatureLink(const LinkSignatureLink&) = delete;
LinkSignatureLink& operator=(const LinkSignatureLink&) = delete;
Type get_kind(void) const { return _kind; }
virtual bool is_executable(void) const { return true; }
virtual ValuePtr execute(AtomSpace*, bool);
ValuePtr construct(const ValueSeq&&);
ValuePtr construct(const HandleSeq&&);
static Handle factory(const Handle&);
};
LINK_PTR_DECL(LinkSignatureLink)
#define createLinkSignatureLink CREATE_DECL(LinkSignatureLink)
}
#endif