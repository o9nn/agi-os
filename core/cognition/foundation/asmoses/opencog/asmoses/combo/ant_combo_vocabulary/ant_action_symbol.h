#ifndef _ANT_ACTION_SYMBOL_H
#define _ANT_ACTION_SYMBOL_H
#include <opencog/util/numeric.h>
#include <opencog/asmoses/combo/combo/action_symbol.h>
#include "ant_operator.h"
namespace opencog { namespace combo {
namespace id {
enum ant_action_symbol_enum {
ant_action_symbol_count
};
}
typedef id::ant_action_symbol_enum ant_action_symbol_enum;
namespace ant_action_symbol_properties {
typedef ant_operator<ant_action_symbol_enum,id::ant_action_symbol_count>::basic_description action_symbol_basic_description;
static const action_symbol_basic_description asbd[] = {
};
}
class ant_action_symbol : public ant_operator<ant_action_symbol_enum, id::ant_action_symbol_count>, public action_symbol_base {
private:
ant_action_symbol();
const basic_description * get_basic_description_array() const;
unsigned int get_basic_description_array_count() const;
static const ant_action_symbol* init_action_symbol();
void set_action_symbol(ant_action_symbol_enum);
public:
const std::string& get_name() const;
const type_tree& get_type_tree() const;
arity_t arity() const;
type_tree get_output_type_tree() const;
const type_tree& get_input_type_tree(arity_t index) const;
static action_symbol get_instance(const std::string& name);
static action_symbol get_instance(ant_action_symbol_enum);
};
}}
#endif