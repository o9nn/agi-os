#ifndef _OPENCOG_COUNT_TRUTH_VALUE_H_
#define _OPENCOG_COUNT_TRUTH_VALUE_H_
#include <opencog/atoms/truthvalue/TruthValue.h>
namespace opencog
{
class CountTruthValue : public TruthValue
{
protected:
enum {
MEAN,
CONFIDENCE,
COUNT
};
public:
CountTruthValue(const std::vector<double>&);
CountTruthValue(strength_t, confidence_t, count_t);
CountTruthValue(const TruthValue&);
CountTruthValue(CountTruthValue const&);
CountTruthValue(const ValuePtr&);
virtual ValuePtr incrementCount(const std::vector<double>&) const;
virtual ValuePtr incrementCount(size_t, double) const;
virtual bool operator==(const Value& rhs) const;
virtual std::string to_string(const std::string& = "") const;
strength_t get_mean() const;
count_t get_count() const;
confidence_t get_confidence() const;
};
VALUE_PTR_DECL(CountTruthValue);
CAST_TV_DECL(CountTruthValue);
CREATE_VALUE_DECL(CountTruthValue);
}
#endif