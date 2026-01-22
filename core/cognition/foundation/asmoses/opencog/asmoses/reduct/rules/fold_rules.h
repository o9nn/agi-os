#ifndef _REDUCT_FOLD_RULES_H
#define _REDUCT_FOLD_RULES_H
#include "opencog/asmoses/reduct/reduct/reduct.h"
namespace opencog { namespace reduct {
struct fold_unrolling : public crule<fold_unrolling> {
fold_unrolling() : crule<fold_unrolling>::crule("fold_unrolling") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
}
}
#endif