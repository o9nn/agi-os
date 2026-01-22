#ifndef ASMOSES_ATOMESETYPECHECKER_H
#define ASMOSES_ATOMESETYPECHECKER_H
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/asmoses/atomese/atom_types/atom_types.h>
#include <opencog/asmoses/combo/type_checker/type_tree_def.h>
#include <opencog/asmoses/combo/type_checker/type_tree.h>
using namespace opencog::combo;
namespace opencog
{
namespace atomese
{
class AtomeseTypeChecker
{
public:
Handle operator()(const type_tree &tt);
Handle convert_type_node(const type_node &tt);
protected:
template<typename Iter>
Handle type_tree_to_atomese_type(Iter it)
{
type_tree::iterator head = it;
Handle atomeseType = convert_type_node(*head);
if (atomeseType->get_type() == ARROW_LINK) {
HandleSeq handleSeq;
for (auto sib = head.begin(); sib != head.end(); ++sib)
handleSeq.push_back(type_tree_to_atomese_type(sib));
Handle lst = createLink(HandleSeq(handleSeq.begin(), handleSeq.end() - 1), LIST_LINK);
atomeseType = createLink(HandleSeq{lst, handleSeq.back()}, ARROW_LINK);
}
return atomeseType;
}
};
}
}
#endif