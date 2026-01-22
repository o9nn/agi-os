#ifndef _ANT_OPERATOR_H
#define _ANT_OPERATOR_H
#include <opencog/asmoses/combo/combo/operator_base.h>
#include <opencog/asmoses/combo/type_checker/type_tree.h>
namespace opencog { namespace combo {
using namespace std;
template<typename OPERATOR_ENUM, OPERATOR_ENUM enum_count>
class ant_operator : public operator_base {
public:
struct basic_description {
OPERATOR_ENUM operator_enum;
string name;
string type;
};
protected:
OPERATOR_ENUM _enum;
std::string _name;
type_tree _type_tree;
arity_t _arity;
type_tree _output_type;
type_tree_seq _arg_type_tree;
ant_operator();
virtual const basic_description* get_basic_description_array() const = 0;
virtual unsigned int get_basic_description_array_count() const = 0;
void set_basic_description(OPERATOR_ENUM oe);
public:
OPERATOR_ENUM get_enum() const;
};
template<typename OPERATOR_ENUM, OPERATOR_ENUM enum_count>
ant_operator<OPERATOR_ENUM, enum_count>::ant_operator() {
_enum = enum_count;
_name = "UNDEFINED_OPERATOR";
_arity = 0;
_output_type = type_tree(id::ill_formed_type);
}
template<typename OPERATOR_ENUM, OPERATOR_ENUM enum_count>
void ant_operator<OPERATOR_ENUM, enum_count>::set_basic_description(OPERATOR_ENUM oe) {
const basic_description* bd = get_basic_description_array();
unsigned int bd_count = get_basic_description_array_count();
OC_ASSERT(bd_count==(unsigned int)enum_count,
"there must be entries for all perceptions.");
bool found = false;
for(unsigned int i = 0; i < bd_count && !found; ++i) {
if(bd[i].operator_enum==oe) {
found = true;
_name = bd[i].name;
std::istringstream is(bd[i].type);
try {
is >> _type_tree;
}
catch(opencog::InconsistenceException& ie) {
std::cout << "WARNING : there must be a problem with the type description of " << _name << ", as the interpretation of the type string : " << "\"" << is.str() << "\"" << " has raised the following exception : " << ie.get_message() << std::endl;
}
_arity = type_tree_arity(_type_tree);
_output_type = get_signature_output(_type_tree);
_arg_type_tree = get_signature_inputs(_type_tree);
}
}
OC_ASSERT(found,
"ant_perception with enum %d has not been found in pbd", oe);
}
template<typename OPERATOR_ENUM, OPERATOR_ENUM enum_count>
OPERATOR_ENUM ant_operator<OPERATOR_ENUM, enum_count>::get_enum() const {
return _enum;
}
}}
#endif