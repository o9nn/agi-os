#ifndef _LG_READ_DICT_H_
#define _LG_READ_DICT_H_
#include "dict-common/dict-structures.h"
Dictionary dictionary_six(const char *lang, const char *dict_name,
const char *pp_name, const char *cons_name,
const char *affix_name, const char *regex_name);
Dictionary dictionary_create_from_file(const char *lang);
bool read_dictionary(Dictionary dict, const char *input);
void insert_list(Dictionary dict, Dict_node * p, int l);
void free_insert_list(Dict_node *ilist);
#endif