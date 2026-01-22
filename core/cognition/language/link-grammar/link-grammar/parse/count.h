#ifndef _COUNT_H
#define _COUNT_H
#include "fast-match.h"
#include "histogram.h"
#include "connectors.h"
typedef struct count_context_s count_context_t;
Count_bin *table_lookup(count_context_t *, int, int,
const Connector *, const Connector *,
unsigned int, size_t *);
int do_parse(Sentence, fast_matcher_t*, count_context_t*, Parse_Options);
bool no_count(count_context_t *, int, Connector *, unsigned int, unsigned int);
match_list_cache *get_cached_match_list(count_context_t *, int, int, Connector *);
count_context_t *alloc_count_context(Sentence, Tracon_sharing*);
void free_count_context(count_context_t*, Sentence);
static inline bool valid_nearest_words(const Connector *le, const Connector *re,
int lw, int rw)
{
int r_limit;
if (likely(re != NULL))
{
if (unlikely(re->nearest_word < lw)) return false;
r_limit = re->nearest_word;
}
else
{
r_limit = rw;
}
if (likely(le != NULL) && unlikely(le->nearest_word > r_limit)) return false;
return true;
}
#endif