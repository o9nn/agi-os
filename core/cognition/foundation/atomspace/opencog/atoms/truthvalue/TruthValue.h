#ifndef _OPENCOG_TRUTH_VALUE_H
#define _OPENCOG_TRUTH_VALUE_H
#include <memory>
#include <string>
#include <vector>
#include <opencog/util/empty_string.h>
#include <opencog/util/exceptions.h>
#include <opencog/atoms/value/FloatValue.h>
class TruthValueUTest;
namespace opencog
{
typedef double strength_t;
typedef double confidence_t;
typedef double count_t;
class TruthValue;
typedef std::shared_ptr<TruthValue> TruthValuePtr;
class TruthValue
: public FloatValue
{
friend class Atom;
friend class ::TruthValueUTest;
TruthValue& operator=(const TruthValue& rhs) {
throw RuntimeException(TRACE_INFO, "Cannot modify truth values!");
}
protected:
TruthValue(Type t) : FloatValue(t) {}
static bool nearly_equal(double, double);
public:
virtual ~TruthValue() {}
virtual bool operator==(const Value&) const = 0;
static TruthValuePtr factory(Type, const std::vector<double>&);
static TruthValuePtr factory(const ValuePtr&);
virtual std::string to_short_string(const std::string&) const;
static TruthValuePtr TRUE_TV();
static TruthValuePtr DEFAULT_TV();
static TruthValuePtr FALSE_TV();
static TruthValuePtr TRIVIAL_TV();
virtual strength_t get_mean() const = 0;
virtual confidence_t get_confidence() const = 0;
virtual count_t get_count() const = 0;
virtual bool isDefaultTV() const;
virtual bool isDefinedTV() const;
};
static inline TruthValuePtr TruthValueCast(const ValuePtr& pa)
{ return std::dynamic_pointer_cast<TruthValue>(pa); }
static inline ValuePtr ValueCast(const TruthValuePtr& tv)
{ return std::shared_ptr<Value>(tv, (Value*) tv.get()); }
#define CAST_TV_DECL(CNAME) \
static inline CNAME##Ptr CNAME##Cast(const TruthValuePtr& a) \
{ return std::dynamic_pointer_cast<CNAME>(a); }
typedef std::vector<TruthValuePtr> TruthValueSeq;
std::string oc_to_string(TruthValuePtr tv,
const std::string& indent=empty_string);
std::string oc_to_string(const TruthValueSeq& tvs,
const std::string& indent=empty_string);
}
#endif