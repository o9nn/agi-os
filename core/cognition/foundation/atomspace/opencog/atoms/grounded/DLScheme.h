#ifndef _OPENCOG_DL_SCHEME_H
#define _OPENCOG_DL_SCHEME_H
#include <opencog/guile/SchemeEval.h>
namespace opencog
{
SchemeEval* get_evaluator_for_scheme(AtomSpace*);
}
#endif