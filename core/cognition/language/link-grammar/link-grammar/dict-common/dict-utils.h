#ifndef _DICT_UTILS_H_
#define _DICT_UTILS_H_
#include "dict-common.h"
#include "dict-structures.h"
void patch_subscript(char *);
int size_of_expression(Exp *);
Exp * copy_Exp(Exp *, Pool_desc *, Parse_Options);
bool is_exp_like_empty_word(Dictionary dict, Exp *);
const char *exp_stringify(const Exp *n);
#ifdef DEBUG
void prt_exp(Exp *, int);
void prt_exp_mem(Exp *);
#endif
bool word_contains(Dictionary dict, const char * word, const char * macro);
#endif