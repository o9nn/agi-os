#ifndef _OPENCOG_SIMPLE_TRUTH_VALUE_H_
#define _OPENCOG_SIMPLE_TRUTH_VALUE_H_
#include <opencog/atoms/truthvalue/TruthValue.h>
namespace opencog
{
class SimpleTruthValue : public TruthValue
{
protected:
enum {
MEAN,
CONFIDENCE
};
public:
static count_t DEFAULT_K;
SimpleTruthValue(const std::vector<double>&);
SimpleTruthValue(strength_t, confidence_t);
SimpleTruthValue(const TruthValue&);
SimpleTruthValue(const SimpleTruthValue&);
SimpleTruthValue(const ValuePtr&);
virtual bool operator==(const Value& rhs) const;
std::string to_string(const std::string& = "") const;
virtual strength_t get_mean() const;
virtual count_t get_count() const;
virtual confidence_t get_confidence() const;
};
VALUE_PTR_DECL(SimpleTruthValue);
CAST_TV_DECL(SimpleTruthValue);
CREATE_VALUE_DECL(SimpleTruthValue);
}
#endif