#ifndef _COMBO_COMPLEXITY_H
#define _COMBO_COMPLEXITY_H
#include <moses/comboreduct/combo/vertex.h>
namespace opencog { namespace moses {
typedef unsigned complexity_t;
static const complexity_t least_complexity = 0;
complexity_t tree_complexity(combo::combo_tree::iterator,
bool (*)(const combo::combo_tree::iterator&) = NULL);
complexity_t tree_complexity(const combo::combo_tree&,
bool (*)(const combo::combo_tree::iterator&) = NULL);
}
}
#endif