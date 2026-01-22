#ifndef  _OPENCOG_OCTOMAP_VALUE_H_
#define  _OPENCOG_OCTOMAP_VALUE_H_
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/atom_types/atom_types.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <typeindex>
#include "OctoMapNode.h"
#include "TimeOctomap.h"
namespace opencog
{
class OctoValue : public FloatValue
{
private:
std::shared_ptr<TimeOctomap<Handle>>   _om;
Handle _item;
Handle _octo_node;
protected:
void update() const;
public:
OctoValue(const HandleSeq& hseq);
virtual ~OctoValue()
{
}
bool operator==(const Value& other) const;
std::string to_string(const std::string& indent = "") const;
};
typedef std::shared_ptr<const OctoValue> OctoValuePtr;
static inline OctoValuePtr OctoValueCast(const ValuePtr& a)
{
return std::dynamic_pointer_cast<const OctoValue>(a);
}
template<typename ... Type>
static inline std::shared_ptr<OctoValue> createOctoValue(Type&&... args) {
return std::make_shared<OctoValue>(std::forward<Type>(args)...);
}
}
#endif