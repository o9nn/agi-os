#ifndef _EXTRACT_LINKS_H
#define _EXTRACT_LINKS_H
#include "api-structures.h"
#include "link-includes.h"
typedef struct extractor_s extractor_t;
extractor_t* extractor_new(Sentence);
void free_extractor(extractor_t*);
bool build_parse_set(extractor_t*, Sentence,
fast_matcher_t*, count_context_t*,
unsigned int null_count, Parse_Options);
void extract_links(extractor_t*, Linkage);
void mark_used_disjuncts(extractor_t *, bool *);
#ifdef PC_DISPLAY
void display_parse_choice(extractor_t *);
#endif
#endif