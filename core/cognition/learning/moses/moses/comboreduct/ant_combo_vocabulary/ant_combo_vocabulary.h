#ifndef _ANT_COMBO_VOCABULARY_H
#define _ANT_COMBO_VOCABULARY_H
#include "ant_builtin_action.h"
#include "ant_perception.h"
#include "ant_action_symbol.h"
#include "ant_indefinite_object.h"
#include <moses/comboreduct/combo/combo.h>
namespace ant_combo {
using namespace opencog::combo;
builtin_action get_instance(ant_builtin_action_enum);
perception get_instance(ant_perception_enum);
action_symbol get_instance(ant_action_symbol_enum);
indefinite_object get_instance(ant_indefinite_object_enum);
ant_builtin_action_enum get_enum(builtin_action);
ant_perception_enum get_enum(perception);
ant_action_symbol_enum get_enum(action_symbol);
ant_indefinite_object_enum get_enum(indefinite_object);
bool operator==(builtin_action, ant_builtin_action_enum);
bool operator==(ant_builtin_action_enum, builtin_action);
bool operator!=(builtin_action, ant_builtin_action_enum);
bool operator!=(ant_builtin_action_enum, builtin_action);
bool operator==(perception, ant_perception_enum);
bool operator==(ant_perception_enum, perception);
bool operator!=(perception, ant_perception_enum);
bool operator!=(ant_perception_enum, perception);
bool operator==(action_symbol, ant_action_symbol_enum);
bool operator==(ant_action_symbol_enum, action_symbol);
bool operator!=(action_symbol, ant_action_symbol_enum);
bool operator!=(ant_action_symbol_enum, action_symbol);
bool operator==(indefinite_object, ant_indefinite_object_enum);
bool operator==(ant_indefinite_object_enum, indefinite_object);
bool operator!=(indefinite_object, ant_indefinite_object_enum);
bool operator!=(ant_indefinite_object_enum, indefinite_object);
}
#endif