#ifndef _TOK_STRUCTURES_H_
#define _TOK_STRUCTURES_H_
#include <stddef.h>
#include "api-types.h"
#include "link-includes.h"
struct gword_set
{
Gword *o_gword;
struct gword_set *next;
struct gword_set *chain_next;
};
gword_set *gword_set_union(gword_set *, gword_set *);
typedef enum
{
MT_NOT_SET,
MT_WORD,
MT_FEATURE,
MT_INFRASTRUCTURE,
MT_WALL,
MT_EMPTY,
MT_UNKNOWN,
MT_TEMPLATE,
MT_ROOT,
MT_CONTR,
MT_PUNC,
MT_STEM    = 1<<6,
MT_PREFIX  = 1<<7,
MT_MIDDLE  = 1<<8,
MT_SUFFIX  = 1<<9
} Morpheme_type;
#define IS_REG_MORPHEME (MT_STEM|MT_PREFIX|MT_MIDDLE|MT_SUFFIX)
#define WS_UNKNOWN (1<<0)
#define WS_REGEX   (1<<1)
#define WS_SPELL   (1<<2)
#define WS_RUNON   (1<<3)
#define WS_HASALT  (1<<4)
#define WS_UNSPLIT (1<<5)
#define WS_INDICT  (1<<6)
#define WS_FIRSTUPPER (1<<7)
#define WS_PL      (1<<14)
#define WS_GUESS (WS_SPELL|WS_RUNON|WS_REGEX)
typedef enum
{
TS_INITIAL,
TS_LR_STRIP,
TS_AFFIX_SPLIT,
TS_REGEX,
TS_RUNON,
TS_SPELL,
TS_DONE
} Tokenizing_step;
typedef enum
{
GM_REGEX = '!',
GM_SPELL = '~',
GM_RUNON = '&',
GM_UNKNOWN = '?'
} Guess_mark;
#define MAX_SPLITS 10
struct Gword_struct
{
const char *subword;
const char *start;
const char *end;
Gword *unsplit_word;
Gword **next;
Gword **prev;
Gword *chain_next;
gword_set gword_set_head;
WordIdx sent_wordidx;
const char *label;
size_t node_num;
Tokenizing_step tokenizing_step;
bool issued_unsplit;
size_t split_counter;
unsigned int status;
Morpheme_type morpheme_type;
Gword *alternative_id;
const char *regex_name;
const Gword **hier_position;
size_t hier_depth;
Gword **null_subwords;
};
struct Wordgraph_pathpos_s
{
Gword *word;
bool same_word;
bool next_ok;
bool used;
};
#endif