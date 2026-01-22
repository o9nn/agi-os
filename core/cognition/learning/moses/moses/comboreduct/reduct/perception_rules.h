#ifndef _REDUCT_PERCEPTION_RULES_H
#define _REDUCT_PERCEPTION_RULES_H
#include "reduct.h"
#include <moses/comboreduct/type_checker/type_tree.h>
namespace opencog { namespace reduct {
struct reduce_ultrametric : public crule<reduce_ultrametric> {
reduce_ultrametric() : crule<reduce_ultrametric>::crule("reduce_ultrametric") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_transitive : public crule<reduce_transitive> {
reduce_transitive() : crule<reduce_transitive>::crule("reduce_transitive") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_reflexive : public crule<reduce_reflexive> {
reduce_reflexive() : crule<reduce_reflexive>::crule("reduce_reflexive") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_irreflexive : public crule<reduce_irreflexive> {
reduce_irreflexive() : crule<reduce_irreflexive>::crule("reduce_irreflexive") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_symmetric : public crule<reduce_symmetric> {
reduce_symmetric() : crule<reduce_symmetric>::crule("reduce_symmetric") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
struct reduce_identity_of_indiscernibles : public crule<reduce_identity_of_indiscernibles> {
reduce_identity_of_indiscernibles() : crule<reduce_identity_of_indiscernibles>::crule("reduce_identity_of_indiscernibles") {}
void operator()(combo_tree& tr,combo_tree::iterator it) const;
};
}
}
#endif