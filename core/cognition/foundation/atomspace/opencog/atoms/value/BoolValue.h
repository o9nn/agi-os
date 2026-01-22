#ifndef _OPENCOG_BOOL_VALUE_H
#define _OPENCOG_BOOL_VALUE_H
#include <vector>
#include <opencog/atoms/value/Value.h>
#include <opencog/atoms/atom_types/atom_types.h>
namespace opencog
{
class BoolValue
: public Value
{
protected:
mutable std::vector<uint64_t> _packed_bits;
mutable size_t _bit_count;
virtual void update() const {}
BoolValue(Type t) : Value(t), _bit_count(0) {}
void set_bit(size_t index, bool value) const;
bool get_bit(size_t index) const;
void pack_vector(const std::vector<bool>& v);
std::vector<bool> unpack_vector() const;
public:
BoolValue(bool v);
BoolValue(const std::vector<bool>& v);
BoolValue(Type t, const std::vector<bool>& v);
virtual ~BoolValue() {}
std::vector<bool> value() const;
size_t size() const { return _bit_count; }
virtual std::string to_string(const std::string& indent = "") const;
std::string to_string(const std::string& indent, Type) const;
virtual bool operator==(const Value&) const;
const std::vector<uint64_t>& get_packed_bits() const { return _packed_bits; }
size_t get_bit_count() const { return _bit_count; }
void set_packed_data(std::vector<uint64_t>&& bits, size_t count) {
_packed_bits = std::move(bits);
_bit_count = count;
}
};
VALUE_PTR_DECL(BoolValue);
CREATE_VALUE_DECL(BoolValue);
ValuePtr bool_and(bool f, const BoolValuePtr& fvp);
ValuePtr bool_or(bool f, const BoolValuePtr& fvp);
ValuePtr bool_not(const BoolValuePtr& fvp);
ValuePtr bool_and(const BoolValuePtr& fvpa, const BoolValuePtr& fvpb);
ValuePtr bool_or(const BoolValuePtr& fvpa, const BoolValuePtr& fvpb);
}
#endif