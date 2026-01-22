#ifndef _JSON_ECODE_H
#define _JSON_ECODE_H
#include <string>
#include <opencog/atoms/base/Handle.h>
namespace opencog
{
class Json
{
public:
static Handle decode_atom(const std::string& s, size_t& pos)
{
size_t start = pos;
size_t end = s.length();
return decode_atom(s, start, end);
}
static Handle decode_atom(const std::string& s) {
size_t junk = 0;
return decode_atom(s, junk);
}
static Handle decode_atom(const std::string& s,
size_t& l, size_t& r);
static std::string get_node_name(const std::string&, size_t& l, size_t& r);
static std::string get_node_name_arg(const std::string& s, size_t& pos, size_t& r);
static ValuePtr decode_value(const std::string&, size_t&, size_t&);
static Type decode_type(const std::string& s, size_t& pos);
static Type decode_type_arg(const std::string& s, size_t& pos);
static std::string encode_atom(const Handle&, const std::string& = "");
static std::string encode_value(const ValuePtr&, const std::string& = "");
static std::string encode_atom_values(const Handle&);
static std::string encode_type_list(const std::vector<Type>&);
};
}
#endif