#ifndef _COMBO_TYPE_TREE_DEF_H
#define _COMBO_TYPE_TREE_DEF_H
#include <opencog/util/tree.h>
namespace opencog { namespace combo {
namespace id {
enum type_node {
lambda_type,
application_type,
union_type,
arg_list_type,
boolean_type,
contin_type,
enum_type,
list_type,
action_result_type,
definite_object_type,
action_definite_object_type,
indefinite_object_type,
message_type,
action_symbol_type,
wild_card_type,
ann_type,
unknown_type,
ill_formed_type,
argument_type
};
}
typedef id::type_node type_node;
typedef std::vector<type_node> type_node_seq;
typedef type_node_seq::iterator type_node_seq_it;
typedef type_node_seq::const_iterator type_node_seq_cit;
typedef opencog::tree<type_node> type_tree;
typedef type_tree::iterator type_tree_pre_it;
typedef type_tree::sibling_iterator type_tree_sib_it;
typedef std::vector<type_tree> type_tree_seq;
typedef type_tree_seq::iterator type_tree_seq_it;
typedef type_tree_seq::const_iterator type_tree_seq_cit;
const static type_tree_seq empty_tts;
bool is_argument_type(type_node n);
unsigned int arg_to_idx(type_node n);
}}
#endif