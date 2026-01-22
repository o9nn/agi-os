#ifndef _REGEX_MORPH_H
#define _REGEX_MORPH_H
#include "dict-common.h"
LINK_BEGIN_DECLS
bool compile_regexs(Regex_node *, Dictionary);
const char *match_regex(const Regex_node *, const char *);
const char *matchspan_regex(Regex_node *, const char *, int *, int *);
void free_regexs(Regex_node *);
LINK_END_DECLS
#endif