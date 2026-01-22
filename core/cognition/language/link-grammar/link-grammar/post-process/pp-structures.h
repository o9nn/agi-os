#ifndef _PP_STRUCTURES_H_
#define _PP_STRUCTURES_H_
#include <stdbool.h>
#include "api-types.h"
#include "post-process.h"
typedef struct Domain_s Domain;
typedef struct DTreeLeaf_s DTreeLeaf;
typedef struct List_o_links_struct List_o_links;
struct Domain_s
{
const char * string;
List_o_links * lol;
DTreeLeaf * child;
Domain * parent;
size_t size;
size_t start_link;
char type;
};
struct DTreeLeaf_s
{
Domain * parent;
DTreeLeaf * next;
int link;
};
struct PP_data_s
{
List_o_links ** word_links;
size_t wowlen;
size_t N_domains;
Domain * domain_array;
size_t domlen;
size_t num_words;
List_o_links * links_to_ignore;
bool *visited;
size_t vlength;
};
struct Postprocessor_s
{
pp_knowledge * knowledge;
int n_global_rules_firing;
int n_local_rules_firing;
pp_linkset *set_of_links_of_sentence;
pp_linkset *set_of_links_in_an_active_rule;
int *relevant_contains_one_rules;
int *relevant_contains_none_rules;
bool q_pruned_rules;
String_set *string_set;
const char *violation;
PP_data pp_data;
};
struct PP_domains_s
{
size_t num_domains;
const char ** domain_name;
};
struct List_o_links_struct
{
size_t link;
size_t word;
List_o_links * next;
};
typedef struct pp_linkset_node_s
{
const char *str;
struct pp_linkset_node_s *next;
} pp_linkset_node;
struct pp_linkset_s
{
unsigned int hash_table_size;
unsigned int population;
pp_linkset_node **hash_table;
};
typedef struct StartingLinkAndDomain_s StartingLinkAndDomain;
struct StartingLinkAndDomain_s
{
const char *starting_link;
int domain;
};
typedef struct pp_rule_s
{
const char *selector;
bool selector_has_wildcard;
pp_linkset *link_set;
int link_set_size;
int domain;
const char **link_array;
const char *msg;
int use_count;
} pp_rule;
typedef struct PPLexTable_s PPLexTable;
struct pp_knowledge_s
{
PPLexTable *lt;
const char *path;
pp_linkset *domain_starter_links;
pp_linkset *urfl_domain_starter_links;
pp_linkset *urfl_only_domain_starter_links;
pp_linkset *domain_contains_links;
pp_linkset *must_form_a_cycle_links;
pp_linkset *restricted_links;
pp_linkset *ignore_these_links;
pp_linkset *left_domain_starter_links;
pp_rule *form_a_cycle_rules;
pp_rule *contains_one_rules;
pp_rule *contains_none_rules;
pp_rule *bounded_rules;
size_t n_form_a_cycle_rules;
size_t n_contains_one_rules;
size_t n_contains_none_rules;
size_t n_bounded_rules;
size_t nStartingLinks;
pp_linkset *set_of_links_starting_bounded_domain;
StartingLinkAndDomain *starting_link_lookup_table;
String_set *string_set;
};
#endif