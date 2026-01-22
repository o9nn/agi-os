#ifndef _LG_DICT_API_H_
#define  _LG_DICT_API_H_
#include "dict-structures.h"
#include "link-includes.h"
#ifndef SWIG
LINK_BEGIN_DECLS
#endif
typedef struct Disjunct_struct Disjunct;
link_public_api(Dict_node *)
dictionary_lookup_list(const Dictionary, const char *);
link_public_api(Dict_node *)
dictionary_lookup_wild(const Dictionary, const char *);
link_public_api(void)
free_lookup_list(const Dictionary, Dict_node *);
link_experimental_api(const Category *)
dictionary_get_categories(const Dictionary dict);
link_experimental_api(const Category_cost *)
linkage_get_categories(const Linkage linkage, WordIdx w);
link_experimental_api(Disjunct **)
sentence_unused_disjuncts(Sentence);
link_experimental_api(char *)
disjunct_expression(Disjunct *);
link_experimental_api(const Category_cost *)
disjunct_categories(Disjunct *);
link_public_api(bool)
dictionary_word_is_known(const Dictionary dict, const char *word);
#ifndef SWIG
LINK_END_DECLS
#endif
#endif