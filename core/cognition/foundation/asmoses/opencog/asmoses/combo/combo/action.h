#ifndef _COMBO_ACTION_H
#define _COMBO_ACTION_H
#include <opencog/util/exceptions.h>
#include <opencog/asmoses/combo/type_checker/type_tree_def.h>
#include <iostream>
#include <vector>
#include "common_def.h"
namespace opencog { namespace combo {
namespace id {
enum action {
sequential_and, sequential_or, sequential_exec,
action_not,
action_if,
action_boolean_if = action_if,
boolean_action_if,
contin_action_if,
action_action_if,
action_success, action_failure,
action_while, boolean_while, return_success, repeat_n,
action_count
};
}
typedef id::action action;
arity_t get_arity(action aa);
std::ostream& operator<<(std::ostream&, const action&);
}
}
#endif