#ifndef _LINKAGE_H
#define _LINKAGE_H
#include <stdbool.h>
#include "api-types.h"
#include "link-includes.h"
struct Linkage_info_struct
{
const char *pp_violation_msg;
int index;
float disjunct_cost;
short N_violations;
short unused_word_cost;
short link_cost;
};
struct Linkage_s
{
WordIdx num_words;
const char * * word;
Link * link_array;
uint32_t num_links;
uint32_t lasz;
Disjunct ** chosen_disjuncts;
size_t cdsz;
const char ** disjunct_list_str;
Gword **wg_path;
Gword **wg_path_display;
Linkage_info lifo;
bool is_sent_long;
bool dupe;
PP_domains * pp_domains;
Sentence sent;
};
struct Link_s
{
uint16_t lw;
uint16_t rw;
Connector * lc;
Connector * rc;
const char * link_name;
};
void compute_generated_words(Sentence, Linkage);
void partial_init_linkage(Sentence, Linkage, unsigned int N_words);
void remove_empty_words(Linkage);
void free_linkage(Linkage);
void linkage_array_free(Linkage);
void free_linkages(Sentence);
#endif