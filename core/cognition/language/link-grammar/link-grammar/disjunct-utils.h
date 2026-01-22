#ifndef _LINK_GRAMMAR_DISJUNCT_UTILS_H_
#define _LINK_GRAMMAR_DISJUNCT_UTILS_H_
#include <stdbool.h>
#include "tracon-set.h"
#include "connectors.h"
#include "api-types.h"
#include "api-structures.h"
#include "parse/histogram.h"
struct Disjunct_struct
{
Disjunct *next;
Connector *left, *right;
gword_set *originating_gword;
union
{
struct
{
unsigned int is_category;
float cost;
const char *word_string;
};
struct
{
unsigned int num_categories;
unsigned int num_categories_alloced;
Category_cost *category;
};
};
union
{
Disjunct *dup_table_next;
struct
{
count_t lrcount;
uint32_t rcount_index;
};
};
union
{
connector_hash_t dup_hash;
int32_t ordinal;
};
struct
{
bool match_left, match_right;
#ifdef VERIFY_MATCH_LIST
uint16_t match_id;
#endif
};
};
#ifdef USE_SAT_SOLVER
void free_disjuncts(Disjunct *);
#endif
void free_sentence_disjuncts(Sentence, bool);
void free_categories(Sentence);
void free_categories_from_disjunct_array(Disjunct *, unsigned int);
unsigned int count_disjuncts(Disjunct *);
Disjunct * catenate_disjuncts(Disjunct *, Disjunct *);
unsigned int eliminate_duplicate_disjuncts(Disjunct *, bool);
int left_connector_count(Disjunct *);
int right_connector_count(Disjunct *);
Tracon_sharing *pack_sentence_for_pruning(Sentence);
Tracon_sharing *pack_sentence_for_parsing(Sentence);
void free_tracon_sharing(Tracon_sharing *);
void free_tracon_memblock(Tracon_sharing *);
void free_saved_memblock(void *);
void count_disjuncts_and_connectors(Sentence, unsigned int *, unsigned int *);
char *print_one_disjunct_str(const Disjunct *);
char *print_one_connector_str(const Connector *, const char *);
char *print_connector_list_str(const Connector *, const char *);
void print_one_connector(const Connector *, const char *);
void print_connector_list(const Connector *, const char *);
void print_disjunct_list(const Disjunct *, const char *);
void print_all_disjuncts(Sentence);
typedef struct
{
Pool_desc *Disjunct_pool;
Pool_desc *Connector_pool;
Disjunct **disjuncts;
} Disjuncts_desc_t;
typedef struct
{
uint32_t *table[2];
size_t entries[2];
size_t table_size[2];
} Tracon_list;
struct tracon_sharing_s
{
union
{
void *memblock;
Disjunct *dblock_base;
};
size_t memblock_sz;
Connector *cblock_base;
Connector *cblock;
Disjunct *dblock;
Disjunct **d;
unsigned int num_connectors;
unsigned int num_disjuncts;
Tracon_set *csid[2];
int next_id[2];
uintptr_t last_token;
int word_offset;
bool is_pruning;
Tracon_list *tracon_list;
uint8_t *uc_seen[2];
unsigned int *num_cnctrs_per_word[2];
};
void *save_disjuncts(Sentence, Tracon_sharing *);
void restore_disjuncts(Sentence, void *, Tracon_sharing *);
void free_saved_disjuncts(Sentence);
static inline Connector *get_tracon(Tracon_sharing *ts, int dir, int id)
{
return &ts->cblock_base[ts->tracon_list->table[dir][id]];
}
#endif