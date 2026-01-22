#ifndef _COMBO_EVAL_H
#define _COMBO_EVAL_H
#include <exception>
#include <opencog/util/tree.h>
#include <opencog/util/numeric.h>
#include <opencog/util/exceptions.h>
#include <opencog/util/mt19937ar.h>
#include "../combo/vertex.h"
#include "../crutil/exception.h"
#include "../type_checker/type_tree.h"
namespace opencog { namespace combo {
combo_tree eval_procedure_tree(const vertex_seq& bmap, combo_tree::iterator it);
vertex eval_throws_binding(const vertex_seq& bmap,
combo_tree::iterator it);
vertex eval_throws_vertex(const vertex_seq& bmap,
combo_tree::iterator it);
vertex eval_throws_binding(const vertex_seq& bmap, const combo_tree& tr);
combo_tree eval_throws_tree(const vertex_seq& bmap, const combo_tree& tr);
combo_tree eval_throws_tree(const vertex_seq& bmap,
combo_tree::iterator it);
vertex eval_binding(const vertex_seq& bmap, combo_tree::iterator it);
vertex eval_binding(const vertex_seq& bmap, const combo_tree& tr);
template<typename T>
arity_t arity(const tree<T>& tr)
{
arity_t a = 0;
for (typename tree<T>::iterator it = tr.begin();
it != tr.end(); ++it)
if (is_argument(*it))
a = std::max(a, (arity_t)std::abs(get_argument(*it).idx));
return a;
}
}}
#endif