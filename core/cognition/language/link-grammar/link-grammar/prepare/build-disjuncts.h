#ifndef _LINKGRAMMAR_BUILD_DISJUNCTS_H
#define _LINKGRAMMAR_BUILD_DISJUNCTS_H
#include "api-types.h"
#include "link-includes.h"
Disjunct *build_disjuncts_for_exp(Sentence sent, Exp *, const char *,
const gword_set *, float cost_cutoff,
Parse_Options opts);
#endif