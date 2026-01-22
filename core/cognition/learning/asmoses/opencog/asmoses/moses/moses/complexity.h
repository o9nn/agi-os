#ifndef _COMBO_COMPLEXITY_H
#define _COMBO_COMPLEXITY_H
#include <opencog/asmoses/combo/combo/vertex.h>
#include <opencog/atoms/base/Handle.h>
#include <opencog/atoms/base/Atom.h>
namespace opencog
{
namespace moses
{
typedef unsigned complexity_t;
static const complexity_t least_complexity = 0;
complexity_t tree_complexity(combo::combo_tree::iterator,
bool (*)(const combo::combo_tree::iterator &) = NULL);
complexity_t tree_complexity(const combo::combo_tree &,
bool (*)(const combo::combo_tree::iterator &) = NULL);
complexity_t atomese_complexity(const Handle &,
bool (*)(const Handle &) = nullptr);
}
}
#endif