#ifndef _ANT_BUILTIN_ACTION_H
#define _ANT_BUILTIN_ACTION_H
#include <opencog/util/numeric.h>
#include <opencog/asmoses/combo/combo/builtin_action.h>
#include "ant_operator.h"
namespace opencog { namespace combo {
namespace id {
enum ant_builtin_action_enum {
move_forward,
turn_left,
turn_right,
ant_builtin_action_count
};
}
typedef id::ant_builtin_action_enum ant_builtin_action_enum;
namespace ant_builtin_action_properties {
typedef opencog::combo::ant_operator<ant_builtin_action_enum, id::ant_builtin_action_count>::basic_description action_basic_description;
struct action_property_description {
ant_builtin_action_enum action;
bool idempotent;
bool always_succeeds;
bool reversible;
ant_builtin_action_enum reversal;
};
struct action_argument_property_description {
ant_builtin_action_enum action;
unsigned char argument_index;
bool additive;
bool zero_neutral;
bool modular;
double min_value;
double max_value;
};
static const action_basic_description abd[] = {
{ id::move_forward,      "move_forward",      "action_result" },
{ id::turn_left,         "turn_left",         "action_result" },
{ id::turn_right,        "turn_right",        "action_result" }
};
static const action_property_description apd[] = {
{ id::move_forward,   false,     true,           false,          (ant_builtin_action_enum)0 },
{ id::turn_left,      false,     true,           true,           id::turn_right },
{ id::turn_right,     false,     true,           true,           id::turn_left }
};
static const action_argument_property_description aapd[] = {
};
}
class ant_builtin_action : public builtin_action_base, public ant_operator<ant_builtin_action_enum, id::ant_builtin_action_count> {
private:
bool _always_succeeds;
bool _reversible;
const ant_builtin_action* _reversal;
bool _idempotent;
std::vector<bool> _arg_additive;
bool _exists_additive_argument;
std::vector<bool> _arg_zero_neutral;
bool _exists_zero_neutral_argument;
std::vector<bool> _arg_modulo;
std::vector<double> _arg_modulo_min;
std::vector<double> _arg_modulo_max;
ant_builtin_action();
const basic_description* get_basic_description_array() const;
unsigned int get_basic_description_array_count() const;
static const ant_builtin_action* init_actions();
void set_action(ant_builtin_action_enum, ant_builtin_action*);
public:
static builtin_action get_instance(const std::string& name);
static builtin_action get_instance(ant_builtin_action_enum);
const std::string& get_name() const;
const type_tree& get_type_tree() const;
arity_t arity() const;
type_tree get_output_type_tree() const;
const type_tree& get_input_type_tree(arity_t i) const;
bool is_reversible() const;
bool always_succeeds() const;
builtin_action get_reversal() const;
bool is_idempotent() const;
bool is_additive(arity_t index) const;
bool exists_additive_argument() const;
bool is_zero_neutral(arity_t index) const;
bool exists_zero_neutral_argument() const;
bool is_modulo(arity_t index) const;
double modulo_min(arity_t index) const;
double modulo_max(arity_t index) const;
const std::set<builtin_action> preconditions() const;
};
}}
#endif