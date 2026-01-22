#ifndef _COMBO_ARGUMENT_H
#define _COMBO_ARGUMENT_H
#include <opencog/asmoses/combo/combo/common_def.h>
#include <boost/operators.hpp>
namespace opencog { namespace combo {
class argument
: boost::less_than_comparable<argument>,
boost::equality_comparable<argument>
{
public:
arity_t idx;
explicit argument(arity_t i) : idx(i) {
OC_ASSERT(idx != 0, "idx should be different than zero.");
}
void negate() {
idx = -idx;
}
bool is_negated() const {
return idx < 0;
}
bool operator<(const argument& rhs) const {
static opencog::absolute_value_order<int> comp;
return comp(idx, rhs.idx);
}
bool operator==(const argument& rhs) const {
return idx == rhs.idx;
}
arity_t abs_idx() const {
return idx < 0 ? -idx : idx;
}
const static arity_t idx_to_abs_idx_from_zero(arity_t other_idx) {
return (other_idx < 0 ? -other_idx : other_idx) - 1;
}
const static arity_t idx_from_zero_to_idx(arity_t idx_from_zero) {
return idx_from_zero + 1;;
}
arity_t abs_idx_from_zero() const {
return idx_to_abs_idx_from_zero(idx);
}
bool is_idx_valid(arity_t a) const {
return (idx == 0 ? false : (a > 0 ? abs_idx() <= a : abs_idx() < -a));
}
};
}
}
#endif