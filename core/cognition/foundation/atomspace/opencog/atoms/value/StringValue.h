#ifndef _OPENCOG_STRING_VALUE_H
#define _OPENCOG_STRING_VALUE_H
#include <string>
#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>
namespace opencog
{
class StringValue
: public Value
{
friend class TransposeColumn;
protected:
mutable std::vector<std::string> _value;
public:
StringValue(const std::string& v)
: Value(STRING_VALUE) { _value.push_back(v); }
StringValue(const std::vector<std::string>& v)
: Value(STRING_VALUE), _value(v) {}
StringValue(Type t, const std::vector<std::string>& v)
: Value(t), _value(v) {}
virtual ~StringValue() {}
const std::vector<std::string>& value() const { return _value; }
size_t size() const {return _value.size(); }
virtual std::string to_string(const std::string& indent = "") const;
virtual bool operator==(const Value&) const;
};
VALUE_PTR_DECL(StringValue);
CREATE_VALUE_DECL(StringValue);
}
#endif