#ifndef _FAST_MATCH_H_
#define _FAST_MATCH_H_
#include <stddef.h>
#include "api-types.h"
#include "disjunct-utils.h"
#include "error.h"
#include "link-includes.h"
#include "memory-pool.h"
typedef struct
{
Disjunct *d;
Count_bin count;
} match_list_cache;
typedef struct Match_node_struct Match_node;
struct Match_node_struct
{
Match_node * next;
Disjunct * d;
};
typedef struct fast_matcher_s fast_matcher_t;
struct fast_matcher_s
{
size_t size;
unsigned int *l_table_size;
unsigned int *r_table_size;
Match_node *** l_table;
Match_node *** r_table;
Disjunct ** match_list;
size_t match_list_end;
size_t match_list_size;
};
fast_matcher_t* alloc_fast_matcher(const Sentence, unsigned int *[]);
void free_fast_matcher(Sentence sent, fast_matcher_t*);
size_t form_match_list(fast_matcher_t *, int, Connector *, int, Connector *,
int, match_list_cache *, match_list_cache *);
static inline Disjunct *get_match_list_element(fast_matcher_t *ctxt, size_t mli)
{
return ctxt->match_list[mli];
}
static inline void pop_match_list(fast_matcher_t *ctxt, size_t match_list_last)
{
ctxt->match_list_end = match_list_last;
#ifdef VERIFY_MATCH_LIST
if (verbosity_level(9))
{
if (get_match_list_element(ctxt, match_list_last) != NULL)
lgdebug(+9, "MATCH_LIST %9d pop\n",
get_match_list_element(ctxt, match_list_last)->match_id);
}
#endif
}
static inline bool is_no_match_list(fast_matcher_t *ctxt, size_t match_list_start)
{
return ctxt->match_list_end == match_list_start;
}
static inline size_t get_match_list_position(fast_matcher_t *ctxt)
{
return ctxt->match_list_end;
}
static inline Disjunct **get_match_list(fast_matcher_t *ctxt, size_t pos)
{
return &ctxt->match_list[pos];
}
#endif