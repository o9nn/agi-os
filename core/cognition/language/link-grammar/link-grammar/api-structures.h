#ifndef _API_STRUCTURESH_
#define _API_STRUCTURESH_
#include <stdint.h>
#ifdef HAVE_LOCALE_T_IN_LOCALE_H
#include <locale.h>
#endif
#ifdef HAVE_LOCALE_T_IN_XLOCALE_H
#include <xlocale.h>
#endif
#include "api-types.h"
#include "dict-common/dialect.h"
#include "tracon-set.h"
#include "memory-pool.h"
#include "string-set.h"
#define SENTENCE_MIN_LENGTH_TRAILING_HASH 0
#define SENTENCE_MIN_LENGTH_MULTI_PRUNING 30
typedef struct Cost_Model_s Cost_Model;
struct Cost_Model_s
{
Cost_Model_type type;
int (*compare_fn)(Linkage, Linkage);
};
struct Resources_s
{
int    max_parse_time;
size_t max_memory;
double time_when_parse_started;
size_t space_when_parse_started;
double when_created;
double when_last_called;
double cumulative_time;
bool   memory_exhausted;
bool   timer_expired;
};
struct Parse_Options_s
{
short verbosity;
char * debug;
char * test;
Resources resources;
short use_spell_guess;
#if USE_SAT_SOLVER
bool use_sat_solver;
#endif
int max_disjuncts;
float disjunct_cost;
short min_null_count;
short max_null_count;
bool islands_ok;
size_t short_length;
bool all_short;
bool repeatable_rand;
bool perform_pp_prune;
size_t twopass_length;
Cost_Model cost_model;
size_t linkage_limit;
bool display_morphology;
dialect_info dialect;
};
typedef struct word_queue_s word_queue_t;
struct word_queue_s
{
Gword *word;
word_queue_t *next;
};
struct Sentence_s
{
Dictionary  dict;
const char *orig_sentence;
size_t length;
Word  *word;
String_set *   string_set;
Pool_desc * Match_node_pool;
Pool_desc * Table_tracon_pool;
Pool_desc * wordvec_pool;
Pool_desc * Exp_pool;
Pool_desc * X_node_pool;
Pool_desc * Disjunct_pool;
Pool_desc * Connector_pool;
Pool_desc * Clause_pool;
Pool_desc * Tconnector_pool;
size_t min_len_encoding;
void *dc_memblock;
unsigned int num_disjuncts;
Gword *wordgraph;
Gword *last_word;
word_queue_t *word_queue;
word_queue_t *word_queue_last;
size_t gword_node_num;
size_t min_len_multi_pruning;
int    num_linkages_found;
bool   overflowed;
size_t num_linkages_alloced;
size_t num_linkages_post_processed;
size_t num_valid_linkages;
unsigned int null_count;
Linkage        lnkages;
Postprocessor * postprocessor;
Postprocessor * constituent_pp;
unsigned int rand_state;
#ifdef USE_SAT_SOLVER
void *hook;
#endif
bool *disjunct_used;
void *wildcard_word_dc_memblock;
unsigned int wildcard_word_dc_memblock_sz;
unsigned int wildcard_word_num_disjuncts;
};
#endif