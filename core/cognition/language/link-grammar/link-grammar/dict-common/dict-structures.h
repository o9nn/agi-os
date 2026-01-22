#ifndef _LG_DICT_STRUCTURES_H_
#define _LG_DICT_STRUCTURES_H_
#include <stdint.h>
#include "link-includes.h"
#ifndef SWIG
LINK_BEGIN_DECLS
#endif
typedef struct Dict_node_struct Dict_node;
typedef struct Exp_struct Exp;
typedef struct condesc_struct condesc_t;
typedef enum
{
OR_type = 1,
AND_type,
CONNECTOR_type
} Exp_type;
#ifndef SWIG
#define COST_MAX_DEC_PLACES 3
static const float cost_epsilon = 1E-7f;
#define EXPTAG_SZ 100
typedef enum { Exptag_none=0, Exptag_dialect, Exptag_macro } Exptag_type;
struct Exp_struct
{
Exp_type type:8;
unsigned int unsued:8;
unsigned int pos:16;
union
{
struct
{
Exptag_type tag_type:8;
unsigned int tag_id:24;
};
struct
{
bool multi;
char dir;
unsigned char farthest_word;
};
};
float cost;
union
{
Exp *operand_first;
condesc_t *condesc;
};
Exp *operand_next;
};
#endif
typedef struct
{
unsigned int num_words;
const char* name;
Exp *exp;
char const ** word;
} Category;
typedef struct
{
unsigned int num;
float cost;
} Category_cost;
#ifndef SWIG
bool cost_eq(float cost1, float cost2);
const char *cost_stringify(float cost);
uint64_t count_clause(const Exp *);
#endif
static inline Exp_type lg_exp_get_type(const Exp* exp) { return exp->type; }
static inline char lg_exp_get_dir(const Exp* exp) { return exp->dir; }
static inline bool lg_exp_get_multi(const Exp* exp) { return exp->multi; }
static inline double lg_exp_get_cost(const Exp* exp) { return exp->cost; }
static inline const Exp* lg_exp_operand_first(const Exp* exp)
{ return exp->operand_first; }
static inline const Exp* lg_exp_operand_next(const Exp* exp)
{ return exp->operand_next; }
link_public_api(const char *)
lg_exp_get_string(const Exp*);
link_public_api(char *)
lg_exp_stringify(const Exp *);
link_experimental_api(Exp *)
lg_exp_resolve(Dictionary, const Exp *, Parse_Options);
struct Dict_node_struct
{
const char * string;
Exp * exp;
Dict_node *left, *right;
const char * file;
unsigned long use_count;
};
#ifndef SWIG
LINK_END_DECLS
#endif
#endif