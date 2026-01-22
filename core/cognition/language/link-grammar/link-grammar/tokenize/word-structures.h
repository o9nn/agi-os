#ifndef _WORD_STRUCTURE_H_
#define _WORD_STRUCTURE_H_
#include <inttypes.h>
#include "stdbool.h"
#include "api-types.h"
typedef struct X_node_struct X_node;
struct X_node_struct
{
const char * string;
Exp * exp;
X_node *next;
const Gword *word;
};
struct Word_struct
{
const char *unsplit_word;
X_node * x;
Disjunct * d;
uint32_t num_disjuncts;
bool optional;
const char **alternatives;
Gword **gwords;
};
#endif