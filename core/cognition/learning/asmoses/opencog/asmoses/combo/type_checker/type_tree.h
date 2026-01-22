#ifndef _COMBO_TYPE_TREE_H
#define _COMBO_TYPE_TREE_H
#include <exception>
#include <opencog/asmoses/utils/tree.h>
#include <opencog/util/numeric.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/asmoses/combo/type_checker/type_tree_def.h>
#include <opencog/asmoses/combo/crutil/exception.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/asmoses/combo/combo/common_def.h>
#include <opencog/asmoses/combo/combo/perception.h>
#include <opencog/asmoses/combo/combo/procedure_call.h>
namespace opencog { namespace combo {
template<typename T>
inline type_node type_node_of() { return id::ill_formed_type; }
template<>
inline type_node type_node_of<bool>() { return id::boolean_type; }
template<>
inline type_node type_node_of<contin_t>() { return id::contin_type; }
vertex default_vertex_value(type_node tn);
char get_arity(builtin b);
type_tree get_type_tree(builtin b);
type_tree get_output_type_tree(builtin b);
type_tree get_input_type_tree(builtin b, arity_t i);
type_tree get_type_tree(action a);
type_tree get_output_type_tree(action a);
type_tree get_input_type_tree(action a, arity_t i);
type_tree get_type_tree(builtin_action a);
type_tree get_output_type_tree(builtin_action a);
type_tree get_input_type_tree(builtin_action a, arity_t i);
type_tree get_type_tree(perception p);
type_tree get_output_type_tree(perception p);
type_tree get_input_type_tree(perception p, arity_t i);
type_tree get_type_tree(const argument& a);
type_tree get_type_tree(contin_t t);
type_tree get_type_tree(const definite_object& d);
type_tree get_type_tree(indefinite_object i);
type_tree get_type_tree(const message& m);
type_tree get_type_tree(action_symbol as);
type_tree get_type_tree(wild_card wc);
type_tree get_output_type_tree(const vertex& v);
type_tree get_input_type_tree(const vertex& v, arity_t i);
type_tree get_type_tree(const vertex& v);
bool equal_type_tree(const type_tree& ty1, const type_tree& ty2);
struct equal_to_type_tree
{
private:
const type_tree& _tt;
public:
equal_to_type_tree(const type_tree& tt) : _tt(tt) {}
bool operator()(const type_tree& tt) {
return equal_type_tree(_tt, tt);
}
};
bool inherit_type_tree(const type_tree& ty1, const type_tree& ty2);
struct inherit_from_type_tree
{
private:
const type_tree& _tt;
public:
inherit_from_type_tree(const type_tree& tt) : _tt(tt) {}
bool operator()(const type_tree& tt) {
return inherit_type_tree(tt, _tt);
}
};
bool inherit_type_tree(const type_tree& ty1, type_tree_pre_it it1,
const type_tree& ty2, type_tree_pre_it it2);
void reduce_type_tree(type_tree& tt,
const type_tree_seq& arg_types,
const combo_tree& tr = combo_tree(),
const std::string& proc_name = std::string("PROCEDURE NAME UNKNOWN"));
void reduce_type_tree(type_tree& tt,
const combo_tree& tr = combo_tree(),
const std::string& proc_name = std::string("PROCEDURE NAME UNKNOWN"));
void reduce_type_tree(type_tree& tt, type_tree_pre_it it,
const type_tree_seq& arg_types,
const combo_tree& tr, combo_tree::iterator ct_it,
const std::string& proc_name);
type_tree get_intersection(const type_tree& tt1, const type_tree& tt2);
type_tree get_intersection(const type_tree& tt1, type_tree_pre_it it1,
const type_tree& tt2, type_tree_pre_it it2);
type_tree infer_vertex_type(const combo_tree& tr, combo_tree::iterator it,
const type_tree_seq& atl = empty_tts);
arity_set get_argument_abs_idx_set(const combo_tree& tr);
arity_set get_argument_abs_idx_from_zero_set(const combo_tree& tr);
void infer_arg_type_tree(const combo_tree& tr, type_tree_seq& arg_types);
type_node_seq type_tree_to_tyn_seq(const type_tree& tt);
void insert_arg_type_tree(const type_tree_seq& arg_types,
type_tree& tt2);
void set_arg_type(const type_tree& tt, const argument& arg,
type_tree_seq& arg_types);
void set_arg_type(const type_tree& tt, type_node arg,
type_tree_seq& arg_types);
void set_arg_type(const type_tree& tt, unsigned int idx,
type_tree_seq& arg_types);
const type_tree& get_arg_type(const argument& arg,
const type_tree_seq& arg_types);
const type_tree& get_arg_type(type_node arg,
const type_tree_seq& arg_types);
const type_tree& get_arg_type(unsigned int idx,
const type_tree_seq& arg_types);
type_tree get_type_tree(const combo_tree& tr);
type_tree get_type_tree(const combo_tree& tr, combo_tree::iterator it);
type_tree infer_type_tree(const combo_tree& tr);
Handle infer_atomese_type(const Handle& handle);
arity_t type_tree_arity(const type_tree& ty);
arity_t contin_arity(const type_tree& ty);
arity_t boolean_arity(const type_tree& ty);
arity_t action_result_arity(const type_tree& ty);
type_node get_type_node(const type_tree& tt);
arity_t convert_index(arity_t arity, arity_t index);
arity_t abs_min_arity(arity_t arity);
const type_tree& argument_type_list_input_type(const type_tree_seq& atl,
arity_t arity,
arity_t index);
arity_t get_arity(const vertex& v);
bool is_well_formed(const type_tree& tt);
bool does_contain_all_arg_up_to(const combo_tree& tr, arity_t n);
arity_t infer_arity(const combo_tree& tr);
arity_t explicit_arity(const combo_tree& tr);
type_tree get_signature_output(const type_tree& ty);
type_tree_seq get_signature_inputs(const type_tree& ty);
type_tree gen_signature(const type_node_seq& itypes, type_node otype);
type_tree gen_signature(const type_tree_seq& inputs, const type_tree& output);
type_tree gen_signature(type_node itype, type_node otype, arity_t arity);
type_tree gen_signature(const type_tree& itype, const type_tree& otype,
arity_t arity);
type_tree gen_signature(type_node iotype, arity_t arity);
type_tree gen_signature(const type_tree& iotype, arity_t arity);
}
std::string oc_to_string(const combo::type_tree& tt,
const std::string& indent=empty_string);
}
namespace std {
std::ostream& operator<<(std::ostream&, const opencog::combo::type_node&);
std::istream& operator>>(std::istream&, opencog::combo::type_node&);
}
#endif