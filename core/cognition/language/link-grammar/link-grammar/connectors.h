#ifndef _LINK_GRAMMAR_CONNECTORS_H_
#define _LINK_GRAMMAR_CONNECTORS_H_
#include <ctype.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include "api-types.h"
#include "error.h"
#include "memory-pool.h"
#include "string-set.h"
#define MAX_SENTENCE 254
#define UNLIMITED_LEN 255
#define NULL_TRACON_BLOCK 256
#define LC_BITS 7
#define LC_MASK ((1<<LC_BITS)-1)
#define MAX_CONNECTOR_LC_LENGTH 9
#define MAX_LINK_NAME_LENGTH 12
typedef uint64_t lc_enc_t;
typedef uint32_t connector_uc_hash_t;
#define CD_HEAD_DEPENDENT    (1<<0)
#define CD_HEAD              (1<<1)
#define CD_PERMANENT         (1<<2)
static inline bool is_connector_name_char(unsigned char c)
{
if (isupper(c)) return true;
if (unlikely(c == '_')) return true;
return false;
}
static inline bool is_connector_subscript_char(unsigned char c)
{
if (islower(c)) return true;
if (unlikely(isdigit(c))) return true;
if (unlikely(c == '*')) return true;
return false;
}
typedef struct condesc_struct condesc_t;
typedef struct hdesc
{
condesc_t *desc;
} hdesc_t;
typedef struct
{
const char *string;
connector_uc_hash_t str_hash;
uint8_t length_limit;
uint8_t flags;
uint8_t uc_length;
uint8_t uc_start;
} condesc_more_t;
struct condesc_struct
{
lc_enc_t lc_letters;
lc_enc_t lc_mask;
condesc_more_t *more;
connector_uc_hash_t uc_num;
uint32_t con_num;
};
typedef struct length_limit_def
{
const char *defword;
const Exp *defexp;
struct length_limit_def *next;
int length_limit;
} length_limit_def_t;
typedef struct
{
hdesc_t *hdesc;
condesc_t **sdesc;
size_t size;
size_t num_con;
size_t num_uc;
size_t last_num;
Pool_desc *desc_pool;
Pool_desc *more_pool;
length_limit_def_t *length_limit_def;
length_limit_def_t **length_limit_def_next;
} ConTable;
struct Connector_struct
{
uint8_t farthest_word;
uint8_t nearest_word;
uint8_t prune_pass;
bool multi;
int32_t tracon_id;
const condesc_t *desc;
Connector *next;
union
{
const gword_set *originating_gword;
struct
{
int32_t refcount;
uint16_t exp_pos;
bool shallow;
};
};
};
void condesc_init(Dictionary, size_t);
void condesc_reset(Dictionary);
void condesc_setup(Dictionary);
condesc_t *condesc_add(ConTable *ct, const char *);
void condesc_delete(Dictionary);
void condesc_reuse(Dictionary);
static inline const char * connector_string(const Connector *c)
{
return c->desc->more->string;
}
static inline unsigned int connector_uc_start(const Connector *c)
{
return c->desc->more->uc_start;
}
static inline unsigned int connector_uc_length(const Connector *c)
{
return c->desc->more->uc_length;
}
static inline const condesc_t *connector_desc(const Connector *c)
{
return c->desc;
}
static inline unsigned int connector_uc_hash(const Connector * c)
{
return c->desc->uc_num;
}
static inline unsigned int connector_uc_num(const Connector * c)
{
return c->desc->uc_num;
}
static inline unsigned int connector_num(const Connector * c)
{
return 2 * c->desc->con_num + c->multi;
}
Connector * connector_new(Pool_desc *, const condesc_t *);
void set_connector_farthest_word(Exp *, int, int, Parse_Options);
void free_connectors(Connector *);
void calculate_connector_info(condesc_t *);
int condesc_by_uc_constring(const void *, const void *);
static inline bool connector_uc_eq(const Connector *c1, const Connector *c2)
{
return (connector_uc_num(c1) == connector_uc_num(c2));
}
static inline Connector *connector_deepest(const Connector *c)
{
for (; c->next != NULL; c = c->next)
;
return (Connector *)c;
}
static inline bool easy_match(const char * s, const char * t)
{
char is = 0, it = 0;
if (islower((int) *s)) { is = *s; s++; }
if (islower((int) *t)) { it = *t; t++; }
if (is != 0 && it != 0 && is == it) return false;
while (isupper((int)*s) || isupper((int)*t))
{
if (*s != *t) return false;
s++;
t++;
}
while ((*s!='\0') && (*t!='\0'))
{
if ((*s == '*') || (*t == '*') || (*s == *t))
{
s++;
t++;
}
else
return false;
}
return true;
}
static inline bool lc_easy_match(const condesc_t *c1, const condesc_t *c2)
{
return (((c1->lc_letters ^ c2->lc_letters) & c1->lc_mask & c2->lc_mask) ==
(c1->lc_mask & c2->lc_mask & 1));
}
static inline bool easy_match_desc(const condesc_t *c1, const condesc_t *c2)
{
if (c1->uc_num != c2->uc_num) return false;
return lc_easy_match(c1, c2);
}
static inline uint32_t string_hash(const char *s)
{
unsigned int i;
i = 5381;
while (*s)
{
i = ((i << 5) + i) + *s;
s++;
}
return i;
}
typedef uint32_t connector_hash_t;
static const connector_hash_t FIBONACCI_MULT = 0x9E3779B9;
static inline connector_hash_t connector_hash(const Connector *c)
{
return connector_num(c);
}
#define FEEDBACK_HASH 1
static inline connector_hash_t connector_list_hash(const Connector *c)
{
connector_hash_t accum = connector_hash(c);
for (c = c->next; c != NULL; c = c->next)
#if FEEDBACK_HASH
accum = (accum<<7) + (accum<<14) + (accum >> 16) - connector_hash(c);
#else
accum = (19 * accum) + connector_hash(c);
#endif
return accum;
}
static inline size_t pair_hash(int lw, int rw,
int l_id, const int r_id,
unsigned int null_count)
{
size_t i;
#if 0
i = 1 << cost;
i += 1 << (lw % (log2_table_size-1));
i += 1 << (rw % (log2_table_size-1));
i += ((unsigned int) le) >> 2;
i += ((unsigned int) le) >> log2_table_size;
i += ((unsigned int) re) >> 2;
i += ((unsigned int) re) >> log2_table_size;
i += i >> log2_table_size;
#else
i = null_count;
i = lw + (i << 6) + (i << 16) - i;
i = rw + (i << 6) + (i << 16) - i;
i = l_id + (i << 6) + (i << 16) - i;
i = r_id + (i << 6) + (i << 16) - i;
#endif
if (i == 0) i = 1;
return i;
}
static inline int get_tracon_word_number(Connector *c, int dir)
{
c = connector_deepest(c);
return c->nearest_word + ((dir == 0) ? 1 : -1);
}
#endif