#ifndef _OPENCOG_ARITHMETIC_LINK_H
#define _OPENCOG_ARITHMETIC_LINK_H
#include <opencog/atoms/reduct/FoldLink.h>
namespace opencog
{
class ArithmeticLink : public FoldLink
{
protected:
void init(void);
virtual Handle reorder(void) const;
bool _commutative;
public:
ArithmeticLink(const HandleSeq&&, Type);
ArithmeticLink(const ArithmeticLink&) = delete;
ArithmeticLink& operator=(const ArithmeticLink&) = delete;
virtual ValuePtr delta_reduce(AtomSpace*, bool) const;
virtual ValuePtr execute(AtomSpace*, bool);
};
LINK_PTR_DECL(ArithmeticLink)
#define createArithmeticLink CREATE_DECL(ArithmeticLink)
}
#endif