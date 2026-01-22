#ifndef _REDUCT_CONTIN_RULES_H
#define _REDUCT_CONTIN_RULES_H
#include <opencog/util/RandGen.h>
#include "opencog/asmoses/reduct/reduct/reduct.h"
#include <opencog/asmoses/combo/combo/simple_nn.h>
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/asmoses/combo/combo/convert_ann_combo.h>
namespace opencog { namespace reduct {
using namespace opencog::combo;
struct ann_rule : public crule<ann_rule> {
ann_rule() : crule<ann_rule>::crule("ann_rule") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const
{
tree_transform trans;
ann net = trans.decodify_tree(tr);
net.reduce();
tr = trans.encode_ann(net);
}
};
}
}
#endif