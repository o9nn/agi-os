#ifndef _OPENCOG_ATTENTION_VALUE_H
#define _OPENCOG_ATTENTION_VALUE_H
#include <limits.h>
#include <memory>
#include <opencog/atoms/value/FloatValue.h>
namespace opencog
{
class AttentionValue;
typedef std::shared_ptr<const AttentionValue> AttentionValuePtr;
class AttentionValue
: public FloatValue
{
protected:
enum {
STI,
LTI,
VLTI,
};
public:
typedef double sti_t;
typedef double lti_t;
typedef double vlti_t;
static const int DISPOSABLE = 0;
static const sti_t DEFAULTATOMSTI;
static const lti_t DEFAULTATOMLTI;
static const vlti_t DEFAULTATOMVLTI;
static constexpr sti_t MAXSTI = SHRT_MAX;
static constexpr lti_t MAXLTI = SHRT_MAX;
static constexpr sti_t MINSTI = SHRT_MIN;
static constexpr lti_t MINLTI = SHRT_MIN;
static AttentionValuePtr DEFAULT_AV() {
static AttentionValuePtr instance =
std::make_shared<AttentionValue>();
return instance;
}
public:
AttentionValue(const std::vector<double>&);
AttentionValue(sti_t = DEFAULTATOMSTI,
lti_t = DEFAULTATOMLTI,
vlti_t = DEFAULTATOMVLTI);
AttentionValue(const AttentionValue&);
AttentionValue(const ValuePtr&);
sti_t getSTI() const;
double getScaledSTI() const
{
return (getSTI() + MAXSTI) / (MAXSTI - MINSTI);
}
lti_t getLTI() const;
vlti_t getVLTI() const;
virtual std::string to_string(const std::string& = "") const;
static AttentionValuePtr createAV(const std::vector<double>& v)
{
return std::make_shared<const AttentionValue>(v);
}
static AttentionValuePtr createAV(sti_t s = DEFAULTATOMSTI,
lti_t l = DEFAULTATOMLTI,
vlti_t v = DEFAULTATOMVLTI)
{
return std::make_shared<const AttentionValue>(s, l, v);
}
AttentionValuePtr clone() const
{
return std::make_shared<AttentionValue>(*this);
}
bool operator==(const AttentionValue& av) const
{
return getSTI() == av.getSTI() and
getLTI() == av.getLTI() and
getVLTI() == av.getVLTI();
}
inline bool operator!=(const AttentionValue& rhs) const
{ return !(*this == rhs); }
bool isDefaultAV() const {
if (this == DEFAULT_AV().get()) return true;
if (*this == *DEFAULT_AV()) return true;
return false;
}
};
static inline AttentionValuePtr AttentionValueCast(const ValuePtr& pa)
{ return std::dynamic_pointer_cast<const AttentionValue>(pa); }
static inline ValuePtr ValueCast(const AttentionValuePtr& av)
{
return std::shared_ptr<Value>(av, (Value*) av.get());
}
template<typename ... Type>
static inline AttentionValuePtr createAttentionValue(Type&&... args) {
return AttentionValue::createAV(std::forward<Type>(args)...);
}
}
#endif