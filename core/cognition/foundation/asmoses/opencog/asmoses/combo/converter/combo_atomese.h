#ifndef MOSES_COMBO_ATOMESE_H
#define MOSES_COMBO_ATOMESE_H
#include <opencog/util/empty_string.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/asmoses/atomese/atom_types/atom_types.h>
namespace opencog
{
namespace combo
{
namespace id
{
enum __attribute__((packed)) procedure_type
{
unknown = 0,
predicate,
schema
};
}
struct vertex_2_atom : boost::static_visitor<std::pair<Type, Handle>>
{
public:
vertex_2_atom(id::procedure_type *parent, AtomSpacePtr as=nullptr,
const string_seq &labels={},
type_node output_type=id::boolean_type);
std::pair<Type, Handle> operator()(const argument &a) const;
std::pair<Type, Handle> operator()(const builtin &b) const;
std::pair<Type, Handle> operator()(const enum_t &e) const;
std::pair<Type, Handle> operator()(const contin_t &c) const;
template<typename T>
std::pair<Type, Handle> operator()(const T &) const
{
OC_ASSERT(false, "Not Implemented Yet");
return std::pair<Type, Handle>();
}
private:
AtomSpacePtr _as;
const string_seq &_labels;
mutable id::procedure_type *_parent;
type_node _out_type;
};
class ComboToAtomese
{
public:
ComboToAtomese();
ComboToAtomese(AtomSpacePtr as);
ComboToAtomese(type_node output_type);
Handle operator()(const combo_tree &tr, const string_seq &labels={});
private:
AtomSpacePtr _as;
type_node _output_type;
protected:
template<typename Iter>
opencog::Handle atomese_combo_it(Iter it,
id::procedure_type &parent_procedure_type,
const string_seq &labels)
{
id::procedure_type procedure_type = parent_procedure_type;
combo_tree::iterator head = it;
std::pair<Type, Handle> atomese = boost::apply_visitor(vertex_2_atom(&procedure_type, _as, labels, _output_type), *head);
Type link_type = atomese.first;
Handle handle = atomese.second;
if (link_type != (unsigned short) -1) {
HandleSeq handle_seq;
for (auto sib = head.begin(); sib != head.end(); ++sib) {
handle_seq.push_back(atomese_combo_it(sib, procedure_type, labels));
}
if (link_type == GREATER_THAN_LINK) handle_seq.push_back(createNode(NUMBER_NODE, "0.0"));
handle = createLink(handle_seq, link_type);
}
return handle;
}
};
class AtomeseToCombo
{
public:
std::pair<combo_tree, string_seq> operator()(const Handle &h);
protected:
void atom2combo(const Handle &h, string_seq &labels, combo_tree &tr,
combo_tree::iterator &iter);
void link2combo(const Handle &h, string_seq &labels, combo_tree &tr,
combo_tree::iterator &iter);
void node2combo(const Handle &h, string_seq &labels, combo_tree &tr,
combo_tree::iterator &iter);
};
}
std::string oc_to_string(const std::pair<combo::combo_tree, combo::string_seq>& ctr_labels,
const std::string& indent=empty_string);
}
#endif