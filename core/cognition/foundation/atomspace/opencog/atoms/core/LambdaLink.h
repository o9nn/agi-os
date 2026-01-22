#ifndef _OPENCOG_LAMBDA_LINK_H
#define _OPENCOG_LAMBDA_LINK_H
#include <opencog/atoms/core/PrenexLink.h>
namespace opencog
{
class LambdaLink : public PrenexLink
{
public:
LambdaLink(const HandleSeq&&, Type=LAMBDA_LINK);
LambdaLink(const Handle& varcdecls, const Handle& body);
LambdaLink(const LambdaLink &) = delete;
LambdaLink& operator=(const LambdaLink &) = delete;
static Handle factory(const Handle&);
};
LINK_PTR_DECL(LambdaLink)
#define createLambdaLink CREATE_DECL(LambdaLink)
}
#endif