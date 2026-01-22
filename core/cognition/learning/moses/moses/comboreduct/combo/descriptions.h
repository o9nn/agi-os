#ifndef _COMBO_DESCRIPTIONS_H
#define _COMBO_DESCRIPTIONS_H
#include <opencog/util/numeric.h>
#include <moses/comboreduct/combo/action.h>
#include <moses/comboreduct/type_checker/type_tree.h>
#include <opencog/util/tree.h>
namespace opencog { namespace combo {
namespace builtin_properties {
const char maximal_builtin_arity=3;
class builtins_properties
{
public:
static builtins_properties& instance() {
static builtins_properties singleton;
return singleton;
}
char builtin_arity(builtin b) { return arity[b]; }
type_tree builtin_argument(builtin b, unsigned char i) { return argument[b][i]; }
type_tree type_tree_of_builtin(builtin b) { return builtin_type_tree[b]; }
id::type_node output_type_of_builtin(builtin b) { return output_type[b]; }
private:
char arity[id::builtin_count];
id::type_node output_type[id::builtin_count];
type_tree builtin_type_tree[id::builtin_count];
type_tree argument[id::builtin_count][maximal_builtin_arity];
builtins_properties();
builtins_properties(const builtins_properties&);
builtins_properties& operator=(const builtins_properties&);
};
}
namespace action_properties {
const char maximal_action_arity=3;
class actions_properties
{
public:
static actions_properties& instance() {
static actions_properties singleton;
return singleton;
}
char action_arity(action a) { return arity[a]; }
type_tree action_argument(action a, unsigned char i) { return argument[a][i]; }
type_tree type_tree_of_action(action a) { return action_type_tree[a]; }
id::type_node output_type_of_action(action a) { return output_type[a]; }
private:
char arity[id::action_count];
id::type_node output_type[id::action_count];
type_tree action_type_tree[id::action_count];
type_tree argument[id::action_count][maximal_action_arity];
actions_properties();
actions_properties(const actions_properties&);
actions_properties& operator=(const actions_properties&);
};
}
}}
#endif