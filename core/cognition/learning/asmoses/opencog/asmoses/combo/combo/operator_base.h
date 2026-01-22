#ifndef _COMBO_OPERATOR_BASE_H
#define _COMBO_OPERATOR_BASE_H
#include <opencog/asmoses/combo/combo/common_def.h>
#include <opencog/asmoses/combo/type_checker/type_tree_def.h>
namespace opencog { namespace combo {
class operator_base {
public:
virtual ~operator_base() {}
virtual const std::string& get_name() const = 0;
virtual const type_tree& get_type_tree() const = 0;
virtual arity_t arity() const = 0;
virtual type_tree get_output_type_tree() const = 0;
virtual const type_tree& get_input_type_tree(arity_t i) const = 0;
static const operator_base* get_instance(const std::string& name);
};
}}
#endif