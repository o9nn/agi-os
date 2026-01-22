#include "dict-structures.h"
#include "link-includes.h"
Dict_node * dict_node_new(void);
void dict_node_noop(Dictionary dict);
void dict_node_free_list(Dict_node *llist);
void dict_node_free_lookup(Dictionary, Dict_node*);
void dict_lookup_noop(Dictionary, Sentence);