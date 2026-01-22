#ifndef _LG_DICT_COMMON_H_
#define _LG_DICT_COMMON_H_
#include <limits.h>
#include "api-types.h"
#include "connectors.h"
#include "dict-defines.h"
#include "dict-structures.h"
#include "dict-ram/dict-ram.h"
#include "memory-pool.h"
#include "utilities.h"
#define EMPTY_CONNECTOR "empty-connector"
#define UNLIMITED_CONNECTORS_WORD ("UNLIMITED-CONNECTORS")
#define LIMITED_CONNECTORS_WORD ("LENGTH-LIMIT-")
#define IS_GENERATION(dict) (dict->category != NULL)
static const float UNINITIALIZED_MAX_DISJUNCT_COST = -10000.0f;
static const float DEFAULT_MAX_DISJUNCT_COST = 2.7f;
static const int UNINITIALIZED_MAX_DISJUNCTS = INT_MAX;
#define LG_DICTIONARY_VERSION_NUMBER "dictionary-version-number"
#define LG_DICTIONARY_LOCALE "dictionary-locale"
#define LG_DISABLE_DOWNCASING "disable-downcasing"
#define LG_DISJUNCT_COST "max-disjunct-cost"
#define LG_MAX_DISJUNCTS "max-disjuncts"
typedef struct Afdict_class_struct Afdict_class;
typedef struct Regex_node_s Regex_node;
struct Regex_node_s
{
const char *name;
char *pattern;
void *re;
Regex_node *next;
bool neg;
int capture_group;
};
static inline Regex_node *regex_new(const char *name, const char *pattern)
{
Regex_node *rn = (Regex_node *)malloc(sizeof(Regex_node));
rn->name = name;
rn->pattern = strdup(pattern);
rn->re = NULL;
rn->neg = false;
rn->capture_group = -1;
rn->next = NULL;
return rn;
}
struct Afdict_class_struct
{
uint16_t mem_elems;
uint16_t length;
uint16_t Nregexes;
const char ** string;
Regex_node ** regex;
};
#define MAX_TOKEN_LENGTH 250
#define IDIOM_LINK_SZ 16
#if defined HAVE_SQLITE3 || defined HAVE_ATOMESE
#define IS_DYNAMIC_DICT(dict) dict->dynamic_lookup
#else
#define IS_DYNAMIC_DICT(dict) false
#endif
#ifdef HAVE_SQLITE3
#define IS_SQL_DICT(dict) (NULL != dict->db_handle)
#else
#define IS_SQL_DICT(dict) false
#endif
typedef struct
{
String_id *set;
const char **name;
const char **value;
unsigned int size;
} dfine_s;
typedef struct
{
String_id *set;
const char **name;
unsigned int num;
unsigned int size;
} expression_tag;
struct Dictionary_s
{
Dict_node * root;
Regex_node * regex_root;
const char * name;
const char * lang;
const char * version;
const char * locale;
locale_t lctype;
int num_entries;
dfine_s dfine;
float default_max_disjunct_cost;
int default_max_disjuncts;
const char * zzz_connector;
bool use_unknown_word;
bool unknown_word_defined;
bool left_wall_defined;
bool right_wall_defined;
bool shuffle_linkages;
bool dynamic_lookup;
bool disable_downcasing;
int8_t allow_duplicate_words;
int8_t allow_duplicate_idioms;
Dialect *dialect;
expression_tag dialect_tag;
expression_tag *macro_tag;
void *cached_dialect;
Dictionary affix_table;
Afdict_class * afdict_class;
bool pre_suf_class_exists;
struct anysplit_params * anysplit;
void * spell_checker;
#ifdef HAVE_SQLITE3
void * db_handle;
#endif
#ifdef HAVE_ATOMESE
void * as_server;
#endif
void (*insert_entry)(Dictionary, Dict_node *, int);
void (*start_lookup)(Dictionary, Sentence);
void (*end_lookup)(Dictionary, Sentence);
Dict_node* (*lookup_list)(Dictionary, const char*);
Dict_node* (*lookup_wild)(Dictionary, const char*);
void (*free_lookup)(Dictionary, Dict_node*);
bool (*exists_lookup)(Dictionary, const char*);
void (*clear_cache)(Dictionary);
void (*close)(Dictionary);
String_set * string_set;
Word_file * word_file_header;
ConTable contable;
Pool_desc * Exp_pool;
pp_knowledge * base_knowledge;
pp_knowledge * hpsg_knowledge;
unsigned int num_categories;
unsigned int num_categories_alloced;
Category * category;
bool generate_walls;
int line_number;
char current_idiom[IDIOM_LINK_SZ];
};
bool is_stem(const char *);
bool is_wall(const char *);
bool is_macro(const char *);
bool dictionary_generation_request(const Dictionary);
bool dict_has_word(const Dictionary dict, const char *);
static inline const char *subscript_mark_str(void)
{
static const char sm[] = { SUBSCRIPT_MARK, '\0' };
return sm;
}
static inline char *get_word_subscript(const char *word)
{
return (char *)strrchr(word, SUBSCRIPT_MARK);
}
#define get_word_subscript(word) _Generic((word), \
const char * : (const char *)(get_word_subscript)((word)), \
char * : (get_word_subscript)((word)))
#endif