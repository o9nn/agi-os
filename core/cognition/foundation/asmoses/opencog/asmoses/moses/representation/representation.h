#ifndef _MOSES_REPRESENTATION_H
#define _MOSES_REPRESENTATION_H
#include <opencog/asmoses/reduct/reduct/reduct.h>
#include <opencog/asmoses/combo/type_checker/type_tree.h>
#include <boost/operators.hpp>
#include "knob_mapper.h"
#include "representation_parameters.h"
namespace opencog { namespace moses {
void set_stepsize(double new_ss);
void set_expansion(double new_ex);
void set_depth(int new_depth);
combo_tree type_to_exemplar(type_node type);
struct representation : public knob_mapper, boost::noncopyable, boost::equality_comparable<representation>
{
typedef std::set<combo::vertex> operator_set;
typedef std::set<combo::combo_tree, size_tree_order<combo::vertex>> combo_tree_ns_set;
representation(const combo_tree& exemplar,
const combo::type_tree& tt,
const representation_parameters& rp=representation_parameters());
void transform(const instance&);
combo_tree get_clean_exemplar(bool reduce, bool knob_building=false) const;
void clean_combo_tree(combo_tree &tr, bool reduce,
bool knob_building=false) const;
combo_tree get_candidate_lock(const instance& inst, bool reduce);
combo_tree get_candidate(const instance& inst, bool reduce) const;
void get_candidate_rec(const instance& inst,
combo_tree::iterator src,
combo_tree::iterator parent_dst,
combo_tree& candidate) const;
bool operator==(const representation& other) const {
return this->_exemplar == other.exemplar();
}
/
template<typename Out>
Out& ostream_prototype(Out& out, combo_tree::iterator it) const
{
typedef combo_tree::sibling_iterator sib_it;
if (is_contin(*it)) {
contin_map_cit c_cit = find_contin_knob(it);
out << (c_cit == contin.end() ? *it : c_cit->second.toStr());
} else {
disc_map_cit d_cit = find_disc_knob(it);
out << (d_cit == disc.end() ? *it : d_cit->second->toStr());
if (d_cit != disc.end()) {
if (d_cit->second.type() == typeid(action_subtree_knob)) {
return out;
}
}
}
if (*it == id::null_vertex) {
OC_ASSERT(it.has_one_child());
it = it.begin();
}
if (not it.is_childless()) {
out << "(";
for (sib_it sib = it.begin(); sib != it.end();) {
ostream_prototype(out, sib);
if (++sib != it.end()) out << " ";
}
out << ")";
}
return out;
}
template<typename Out>
Out& ostream_prototype(Out& out) const
{
return ostream_prototype(out, _exemplar.begin());
}
protected:
combo_tree _exemplar;
representation_parameters _params;
#ifdef EXEMPLAR_INST_IS_UNDEAD
void set_exemplar_inst();
instance _exemplar_inst;
#endif
field_set _fields;
mutable std::mutex tranform_mutex;
};
inline std::ostream& operator<<(std::ostream& out,
const opencog::moses::representation& r)
{
return r.ostream_prototype(out);
}
}
std::string oc_to_string(const moses::representation& rep,
const std::string& indent=empty_string);
}
#endif