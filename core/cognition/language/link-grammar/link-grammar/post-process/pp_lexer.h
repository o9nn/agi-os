#include <stdio.h>
#include "pp-structures.h"
typedef struct pp_label_node_s
{
const char *str;
struct pp_label_node_s *next;
} pp_label_node;
#define PP_LEXER_MAX_LABELS 512
struct PPLexTable_s
{
void *scanner;
String_set *string_set;
const char *labels[PP_LEXER_MAX_LABELS];
pp_label_node *nodes_of_label[PP_LEXER_MAX_LABELS];
pp_label_node *last_node_of_label[PP_LEXER_MAX_LABELS];
pp_label_node *current_node_of_active_label;
int idx_of_active_label;
const char **tokens;
int extents;
};
PPLexTable *pp_lexer_open (FILE *f);
void pp_lexer_close (PPLexTable *lt);
int pp_lexer_set_label (PPLexTable *lt, const char *label);
int pp_lexer_count_tokens_of_label (PPLexTable *lt);
const char *pp_lexer_get_next_token_of_label(PPLexTable *lt);
int pp_lexer_count_commas_of_label (PPLexTable *lt);
const char **pp_lexer_get_next_group_of_tokens_of_label(PPLexTable *lt, size_t *n_toks);