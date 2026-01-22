#include <memory.h>
#include <stdint.h>
#include "post-process.h"
#include "api-structures.h"
#include "connectors.h"
#include "error.h"
#include "linkage/linkage.h"
#include "linkage/score.h"
#include "pp_knowledge.h"
#include "pp_linkset.h"
#include "pp-structures.h"
#include "resources.h"
#include "string-set.h"
#define PP_MAX_DOMAINS 128
bool post_process_match(const char *s, const char *t)
{
if (NULL == t) return false;
if (islower((unsigned char)*t)) t++;
while (is_connector_name_char(*s))
{
if (*s != *t) return false;
s++;
t++;
}
if (is_connector_name_char(*t)) return false;
while (*t != '\0')
{
if (*s == '\0') return true;
if (*s != *t && *s != '#') return false;
s++;
t++;
}
while (*s != '\0')
{
if (*s != '*' && *s != '#') return false;
s++;
}
return true;
}
static bool string_in_list(const char * s, const char * a[])
{
int i;
for (i=0; a[i] != NULL; i++)
if (post_process_match(a[i], s)) return true;
return false;
}
static size_t find_domain_name(Postprocessor *pp, const char *link)
{
size_t i, domain;
StartingLinkAndDomain *sllt = pp->knowledge->starting_link_lookup_table;
for (i=0;;i++)
{
domain = sllt[i].domain;
if (domain == SIZE_MAX) return SIZE_MAX;
if (post_process_match(sllt[i].starting_link, link)) return domain;
}
}
static bool contained_in(const Domain * d1, const Domain * d2,
const Linkage sublinkage)
{
bool *mark = alloca(sublinkage->num_links*sizeof(bool));
List_o_links * lol;
memset(mark, 0, sublinkage->num_links*(sizeof(bool)));
for (lol=d2->lol; lol != NULL; lol = lol->next)
mark[lol->link] = true;
for (lol=d1->lol; lol != NULL; lol = lol->next)
if (!mark[lol->link]) return false;
return true;
}
static bool link_in_domain(size_t link, const Domain * d)
{
List_o_links * lol;
for (lol = d->lol; lol != NULL; lol = lol->next)
if (lol->link == link) return true;
return false;
}
#if defined(CHECK_DOMAIN_NESTING)
static bool check_domain_nesting(Postprocessor *pp, int num_links)
{
size_t id1, id2;
Domain * d1, * d2;
int counts[4];
char mark[MAX_NUM_LINKS];
List_o_links * lol;
int i;
PP_data *pp_data = &pp->pp_data;
for (id1 = 0; id1 < pp_data->N_domains; id1++)
{
d1 = &pp_data->domain_array[id1];
for (id2 = id1+1; id2 < pp_data->N_domains; id2++)
{
d2 = &pp_data->domain_array[id2];
memset(mark, 0, num_links);
for (lol=d2->lol; lol != NULL; lol = lol->next)
mark[lol->link] = 1;
for (lol=d1->lol; lol != NULL; lol = lol->next)
mark[lol->link] += 2;
counts[0] = counts[1] = counts[2] = counts[3] = 0;
for (i=0; i<num_links; i++)
{
assert(mark[i] < 4, "Miscount of link marks!");
counts[(size_t)mark[i]]++;
}
if ((counts[1] > 0) && (counts[2] > 0) && (counts[3] > 0))
return false;
}
}
return true;
}
#endif
static void free_List_o_links(List_o_links *lol)
{
List_o_links * xlol;
while (lol != NULL)
{
xlol = lol->next;
free(lol);
lol = xlol;
}
}
static void free_D_tree_leaves(DTreeLeaf *dtl)
{
DTreeLeaf * xdtl;
while (dtl != NULL)
{
xdtl = dtl->next;
free(dtl);
dtl = xdtl;
}
}
static void pp_free_domain_array(PP_data *ppd)
{
size_t d;
for (d = 0; d < ppd->domlen; d++)
{
free_List_o_links(ppd->domain_array[d].lol);
ppd->domain_array[d].lol = NULL;
free_D_tree_leaves(ppd->domain_array[d].child);
ppd->domain_array[d].child = NULL;
}
}
void post_process_free_data(PP_data * ppd)
{
size_t w;
for (w = 0; w < ppd->wowlen; w++)
{
free_List_o_links(ppd->word_links[w]);
ppd->word_links[w] = NULL;
}
pp_free_domain_array(ppd);
free_List_o_links(ppd->links_to_ignore);
ppd->links_to_ignore = NULL;
ppd->num_words = 0;
ppd->N_domains = 0;
}
#ifdef THIS_FUNCTION_IS_NOT_CURRENTLY_USED
static void connectivity_dfs(Postprocessor *pp, Linkage sublinkage,
int w, pp_linkset *ls)
{
List_o_links *lol;
assert(w < pp_data->num_words, "Bad word index");
pp_data->visited[w] = true;
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
if (!pp_data->visited[lol->word] &&
!pp_linkset_match(ls, sublinkage->link[lol->link]->name))
connectivity_dfs(pp, sublinkage, lol->word, ls);
}
}
#endif
const char * linkage_get_violation_name(const Linkage linkage)
{
return linkage->lifo.pp_violation_msg;
}
static void clear_visited(PP_data *pp_data)
{
memset(pp_data->visited, 0, pp_data->num_words * sizeof(bool));
}
static bool apply_rules(PP_data *pp_data,
bool (applyfn) (PP_data *, Linkage, pp_rule *),
Linkage sublinkage,
pp_rule *rule_array,
const char **msg)
{
int i;
for (i = 0; (*msg = rule_array[i].msg) != NULL; i++)
{
if (!applyfn(pp_data, sublinkage, &(rule_array[i])))
{
rule_array[i].use_count ++;
return false;
}
}
return true;
}
static bool
apply_relevant_rules(Postprocessor *pp,
bool (applyfn)(PP_data *, Linkage, pp_rule *),
Linkage sublinkage,
pp_rule *rule_array,
int *relevant_rules,
const char **msg)
{
int i, idx;
PP_data *pp_data = &pp->pp_data;
if (pp_linkset_population(pp->set_of_links_of_sentence) == 0) {
return apply_rules(pp_data, applyfn, sublinkage, rule_array, msg);
}
for (i = 0; (idx = relevant_rules[i]) != -1; i++)
{
*msg = rule_array[idx].msg;
if (!applyfn(pp_data, sublinkage, &(rule_array[idx]))) return false;
}
return true;
}
static bool
apply_contains_one(PP_data *pp_data, Linkage sublinkage, pp_rule *rule)
{
DTreeLeaf * dtl;
size_t d, count;
for (d=0; d<pp_data->N_domains; d++)
{
for (dtl = pp_data->domain_array[d].child;
dtl != NULL &&
!post_process_match(rule->selector,
sublinkage->link_array[dtl->link].link_name);
dtl = dtl->next) {}
if (dtl != NULL)
{
count=0;
for (dtl = pp_data->domain_array[d].child; dtl != NULL; dtl = dtl->next)
{
if (string_in_list(sublinkage->link_array[dtl->link].link_name,
rule->link_array))
{
count=1;
break;
}
}
if (count == 0) return false;
}
}
return true;
}
static bool
apply_contains_none(PP_data *pp_data, Linkage sublinkage, pp_rule *rule)
{
size_t d;
for (d=0; d<pp_data->N_domains; d++)
{
DTreeLeaf * dtl;
for (dtl = pp_data->domain_array[d].child;
dtl != NULL &&
!post_process_match(rule->selector,
sublinkage->link_array[dtl->link].link_name);
dtl = dtl->next) {}
if (dtl != NULL)
{
for (dtl = pp_data->domain_array[d].child; dtl != NULL; dtl = dtl->next)
{
if (string_in_list(sublinkage->link_array[dtl->link].link_name,
rule->link_array))
return false;
}
}
}
return true;
}
static bool
apply_contains_one_globally(PP_data *pp_data, Linkage sublinkage, pp_rule *rule)
{
size_t i;
for (i = 0; i < sublinkage->num_links; i++)
{
if (post_process_match(rule->selector, sublinkage->link_array[i].link_name)) break;
}
if (i == sublinkage->num_links) return true;
size_t count = 0;
for (size_t j = 0; j < sublinkage->num_links && count == 0; j++)
{
if (string_in_list(sublinkage->link_array[j].link_name, rule->link_array))
{
count = 1;
break;
}
}
if (count == 0) return false; else return true;
}
static void reachable_without_dfs(PP_data *pp_data,
Linkage sublinkage, size_t a, size_t b, size_t w)
{
List_o_links *lol;
assert(w < pp_data->num_words, "Bad word index");
pp_data->visited[w] = true;
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
assert(lol->word < pp_data->num_words, "Bad word index");
if (!pp_data->visited[lol->word] &&
!(w == a && lol->word == b) &&
!(w == b && lol->word == a))
{
reachable_without_dfs(pp_data, sublinkage, a, b, lol->word);
}
}
}
static bool
apply_must_form_a_cycle(PP_data *pp_data, Linkage sublinkage, pp_rule *rule)
{
List_o_links *lol;
size_t w;
for (w = 0; w < pp_data->num_words; w++)
{
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
if (w > lol->word) continue;
if (!pp_linkset_match(rule->link_set, sublinkage->link_array[lol->link].link_name)) continue;
clear_visited(pp_data);
reachable_without_dfs(pp_data, sublinkage, w, lol->word, w);
if (!pp_data->visited[lol->word]) return false;
}
}
for (lol = pp_data->links_to_ignore; lol != NULL; lol = lol->next)
{
w = sublinkage->link_array[lol->link].lw;
if (!pp_linkset_match(rule->link_set, sublinkage->link_array[lol->link].link_name)) continue;
clear_visited(pp_data);
reachable_without_dfs(pp_data, sublinkage, w, lol->word, w);
assert(lol->word < pp_data->num_words, "Bad word index");
if (!pp_data->visited[lol->word]) return false;
}
return true;
}
static bool
apply_bounded(PP_data *pp_data, Linkage sublinkage, pp_rule *rule)
{
size_t d, lw;
List_o_links * lol;
char d_type = rule->domain;
for (d = 0; d < pp_data->N_domains; d++)
{
if (pp_data->domain_array[d].type != d_type) continue;
lw = sublinkage->link_array[pp_data->domain_array[d].start_link].lw;
for (lol = pp_data->domain_array[d].lol; lol != NULL; lol = lol->next)
{
if (sublinkage->link_array[lol->link].lw < lw) return false;
}
}
return true;
}
static void build_graph(Postprocessor *pp, Linkage sublinkage)
{
PP_data *pp_data = &pp->pp_data;
if (pp_data->wowlen <= pp_data->num_words)
{
pp_data->wowlen += pp_data->num_words;
size_t newsz = pp_data->wowlen * sizeof(List_o_links *);
pp_data->word_links = (List_o_links **) realloc(
pp_data->word_links, newsz);
}
memset(pp_data->word_links, 0, pp_data->wowlen * sizeof(List_o_links *));
for (size_t link = 0; link < sublinkage->num_links; link++)
{
if (NULL == sublinkage->link_array[link].link_name) continue;
List_o_links * lol = (List_o_links *) malloc(sizeof(List_o_links));
lol->link = link;
lol->word = sublinkage->link_array[link].rw;
if (pp_linkset_match(pp->knowledge->ignore_these_links,
sublinkage->link_array[link].link_name))
{
lol->next = pp_data->links_to_ignore;
pp_data->links_to_ignore = lol;
continue;
}
lol->next = pp_data->word_links[sublinkage->link_array[link].lw];
pp_data->word_links[sublinkage->link_array[link].lw] = lol;
lol = (List_o_links *) malloc(sizeof(List_o_links));
lol->link = link;
lol->word = sublinkage->link_array[link].lw;
lol->next = pp_data->word_links[sublinkage->link_array[link].rw];
pp_data->word_links[sublinkage->link_array[link].rw] = lol;
}
}
static void setup_domain_array(Postprocessor *pp,
const char *string, int start_link)
{
PP_data *pp_data = &pp->pp_data;
size_t n = pp_data->N_domains;
if (pp_data->domlen <= n)
{
size_t oldsz, incsz;
#define DOMINC 16
oldsz = pp_data->domlen * sizeof(Domain);
incsz = DOMINC * sizeof(Domain);
pp_data->domain_array = (Domain *) realloc(pp_data->domain_array,
oldsz + incsz);
memset(&pp_data->domain_array[pp_data->domlen], 0, incsz);
pp_data->domlen += DOMINC;
}
pp_data->domain_array[n].string = string;
pp_data->domain_array[n].lol = NULL;
pp_data->domain_array[n].size = 0;
pp_data->domain_array[n].start_link = start_link;
pp_data->N_domains++;
assert(pp_data->N_domains<PP_MAX_DOMAINS, "raise value of PP_MAX_DOMAINS");
}
static void add_link_to_domain(PP_data *pp_data, int link)
{
size_t n = pp_data->N_domains - 1;
List_o_links *lol = (List_o_links *) malloc(sizeof(List_o_links));
lol->next = pp_data->domain_array[n].lol;
pp_data->domain_array[n].lol = lol;
pp_data->domain_array[n].size++;
lol->link = link;
}
static void depth_first_search(Postprocessor *pp, Linkage sublinkage,
size_t w, size_t root, size_t start_link)
{
List_o_links *lol;
PP_data *pp_data = &pp->pp_data;
assert(w < pp_data->num_words, "Bad word index");
pp_data->visited[w] = true;
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
if (lol->word < w && lol->link != start_link)
{
add_link_to_domain(pp_data, lol->link);
}
}
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
if (!pp_data->visited[lol->word] && (lol->word != root) &&
!(lol->word < root && lol->word < w &&
pp_linkset_match(pp->knowledge->restricted_links,
sublinkage->link_array[lol->link].link_name)))
{
depth_first_search(pp, sublinkage, lol->word, root, start_link);
}
}
}
static void bad_depth_first_search(Postprocessor *pp, Linkage sublinkage,
size_t w, size_t root, size_t start_link)
{
List_o_links * lol;
PP_data *pp_data = &pp->pp_data;
assert(w < pp_data->num_words, "Bad word index");
pp_data->visited[w] = true;
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
if ((lol->word < w) && (lol->link != start_link) && (w != root))
{
add_link_to_domain(pp_data, lol->link);
}
}
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
assert(lol->word < pp_data->num_words, "Bad word index");
if ((!pp_data->visited[lol->word]) && !(w == root && lol->word < w) &&
!(lol->word < root && lol->word < w &&
pp_linkset_match(pp->knowledge->restricted_links,
sublinkage->link_array[lol->link].link_name)))
{
bad_depth_first_search(pp, sublinkage, lol->word, root, start_link);
}
}
}
static void d_depth_first_search(Postprocessor *pp, Linkage sublinkage,
size_t w, size_t root, size_t right, size_t start_link)
{
List_o_links * lol;
PP_data *pp_data = &pp->pp_data;
assert(w < pp_data->num_words, "Bad word index");
pp_data->visited[w] = true;
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
if ((lol->word < w) && (lol->link != start_link) && (w != root))
{
add_link_to_domain(pp_data, lol->link);
}
}
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
assert(lol->word < pp_data->num_words, "Bad word index");
if (!pp_data->visited[lol->word] && !(w == root && lol->word >= right) &&
!(w == root && lol->word < root) &&
!(lol->word < root && lol->word < w &&
pp_linkset_match(pp->knowledge->restricted_links,
sublinkage->link_array[lol->link].link_name)))
{
d_depth_first_search(pp,sublinkage,lol->word,root,right,start_link);
}
}
}
static void left_depth_first_search(Postprocessor *pp, Linkage sublinkage,
size_t w, size_t right, size_t start_link)
{
List_o_links *lol;
PP_data *pp_data = &pp->pp_data;
assert(w < pp_data->num_words, "Bad word index");
pp_data->visited[w] = true;
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
if (lol->word < w && lol->link != start_link)
{
add_link_to_domain(pp_data, lol->link);
}
}
for (lol = pp_data->word_links[w]; lol != NULL; lol = lol->next)
{
assert(lol->word < pp_data->num_words, "Bad word index");
if (!pp_data->visited[lol->word] && (lol->word != right))
{
depth_first_search(pp, sublinkage, lol->word, right, start_link);
}
}
}
static int domain_compare(const Domain * d1, const Domain * d2)
{
if (d1->size == d2->size)
return (d1 > d2);
return (d1->size - d2->size);
}
static void build_domains(Postprocessor *pp, Linkage sublinkage)
{
PP_data *pp_data = &pp->pp_data;
pp_data->N_domains = 0;
for (size_t link = 0; link<sublinkage->num_links; link++)
{
if (NULL == sublinkage->link_array[link].link_name) continue;
const char *s = sublinkage->link_array[link].link_name;
if (pp_linkset_match(pp->knowledge->ignore_these_links, s)) continue;
if (pp_linkset_match(pp->knowledge->domain_starter_links, s))
{
setup_domain_array(pp, s, link);
if (pp_linkset_match(pp->knowledge->domain_contains_links, s))
add_link_to_domain(pp_data, link);
clear_visited(pp_data);
depth_first_search(pp, sublinkage, sublinkage->link_array[link].rw,
sublinkage->link_array[link].lw, link);
}
else
if (pp_linkset_match(pp->knowledge->urfl_domain_starter_links, s))
{
setup_domain_array(pp, s, link);
add_link_to_domain(pp_data, link);
clear_visited(pp_data);
bad_depth_first_search(pp, sublinkage,sublinkage->link_array[link].rw,
sublinkage->link_array[link].lw, link);
}
else
if (pp_linkset_match(pp->knowledge->urfl_only_domain_starter_links, s))
{
setup_domain_array(pp, s, link);
clear_visited(pp_data);
d_depth_first_search(pp, sublinkage, sublinkage->link_array[link].lw,
sublinkage->link_array[link].lw,
sublinkage->link_array[link].rw, link);
}
else
if (pp_linkset_match(pp->knowledge->left_domain_starter_links, s))
{
setup_domain_array(pp, s, link);
clear_visited(pp_data);
left_depth_first_search(pp, sublinkage, sublinkage->link_array[link].lw,
sublinkage->link_array[link].rw, link);
}
}
qsort((void *) pp_data->domain_array,
pp_data->N_domains,
sizeof(Domain),
(int (*)(const void *, const void *)) domain_compare);
for (size_t d = 0; d < pp_data->N_domains; d++)
{
size_t i = find_domain_name(pp, pp_data->domain_array[d].string);
if (i == SIZE_MAX)
prt_error("Error: post_process(): Need an entry for %s in LINK_TYPE_TABLE\n",
pp_data->domain_array[d].string);
pp_data->domain_array[d].type = i;
}
}
static void build_domain_forest(PP_data *pp_data, Linkage sublinkage)
{
if (0 == pp_data->N_domains) return;
pp_data->domain_array[pp_data->N_domains-1].parent = NULL;
for (size_t d=0; d < pp_data->N_domains-1; d++)
{
size_t d1;
for (d1 = d+1; d1 < pp_data->N_domains; d1++)
{
if (contained_in(&pp_data->domain_array[d], &pp_data->domain_array[d1], sublinkage))
{
pp_data->domain_array[d].parent = &pp_data->domain_array[d1];
break;
}
}
if (d1 == pp_data->N_domains)
{
pp_data->domain_array[d].parent = NULL;
}
}
for (size_t d = 0; d < pp_data->N_domains; d++)
{
pp_data->domain_array[d].child = NULL;
}
for (size_t link=0; link < sublinkage->num_links; link++)
{
for (size_t d=0; d<pp_data->N_domains; d++)
{
if (link_in_domain(link, &pp_data->domain_array[d]))
{
DTreeLeaf * dtl = (DTreeLeaf *) malloc(sizeof(DTreeLeaf));
dtl->link = link;
dtl->parent = &pp_data->domain_array[d];
dtl->next = pp_data->domain_array[d].child;
pp_data->domain_array[d].child = dtl;
break;
}
}
}
}
static int
internal_process(Postprocessor *pp, Linkage sublinkage, const char **msg)
{
PP_data *pp_data = &pp->pp_data;
if (!apply_relevant_rules(pp, apply_contains_one_globally,
sublinkage,
pp->knowledge->contains_one_rules,
pp->relevant_contains_one_rules, msg))
{
for (size_t i = 0; i < pp_data->wowlen; i++)
pp_data->word_links[i] = NULL;
pp_data->N_domains = 0;
return -1;
}
build_graph(pp, sublinkage);
build_domains(pp, sublinkage);
build_domain_forest(&pp->pp_data, sublinkage);
#if defined(CHECK_DOMAIN_NESTING)
if (!check_domain_nesting(pp, sublinkage->num_links))
prt_error("Warning: The domains are not nested.\n");
#endif
if (!apply_relevant_rules(pp, apply_contains_one, sublinkage,
pp->knowledge->contains_one_rules,
pp->relevant_contains_one_rules, msg)) return 1;
if (!apply_relevant_rules(pp, apply_contains_none, sublinkage,
pp->knowledge->contains_none_rules,
pp->relevant_contains_none_rules, msg)) return 1;
if (!apply_rules(pp_data, apply_must_form_a_cycle, sublinkage,
pp->knowledge->form_a_cycle_rules,msg)) return 1;
if (!apply_rules(pp_data, apply_bounded, sublinkage,
pp->knowledge->bounded_rules, msg)) return 1;
return 0;
}
static void prune_irrelevant_rules(Postprocessor *pp)
{
pp_rule *rule;
int coIDX, cnIDX, rcoIDX = 0, rcnIDX = 0;
if (pp_linkset_population(pp->set_of_links_of_sentence) == 0) return;
for (coIDX = 0; ; coIDX++)
{
rule = &(pp->knowledge->contains_one_rules[coIDX]);
if (rule->msg == NULL) break;
if (pp_linkset_match_bw(pp->set_of_links_of_sentence, rule->selector))
{
pp->relevant_contains_one_rules[rcoIDX++] = coIDX;
pp_linkset_add(pp->set_of_links_in_an_active_rule, rule->selector);
}
}
pp->relevant_contains_one_rules[rcoIDX] = -1;
for (cnIDX = 0; ; cnIDX++)
{
rule = &(pp->knowledge->contains_none_rules[cnIDX]);
if (rule->msg == NULL) break;
if (pp_linkset_match_bw(pp->set_of_links_of_sentence, rule->selector))
{
pp->relevant_contains_none_rules[rcnIDX++] = cnIDX;
pp_linkset_add(pp->set_of_links_in_an_active_rule, rule->selector);
}
}
pp->relevant_contains_none_rules[rcnIDX] = -1;
if (verbosity_level(5))
{
err_msg(lg_Debug, "PP: Saw %zu unique link names in all linkages.\n\\",
pp_linkset_population(pp->set_of_links_of_sentence));
err_msg(lg_Debug, "PP: Using %i 'contains one' rules "
"and %i 'contains none' rules\n",
rcoIDX, rcnIDX);
}
}
#define PP_INITLEN 60
static void pp_new_domain_array(PP_data *pp_data)
{
pp_data->domlen = PP_INITLEN;
pp_data->domain_array = (Domain*) malloc(pp_data->domlen * sizeof(Domain));
memset(pp_data->domain_array, 0, pp_data->domlen * sizeof(Domain));
}
Postprocessor * post_process_new(pp_knowledge * kno)
{
Postprocessor *pp;
PP_data *pp_data;
if (NULL == kno) return NULL;
pp = (Postprocessor *) malloc (sizeof(Postprocessor));
pp->knowledge = kno;
pp->string_set = string_set_create();
pp->set_of_links_of_sentence = pp_linkset_open(1024);
pp->set_of_links_in_an_active_rule = pp_linkset_open(1024);
pp->relevant_contains_one_rules =
(int *) malloc ((pp->knowledge->n_contains_one_rules + 1)
*(sizeof pp->relevant_contains_one_rules[0]));
pp->relevant_contains_none_rules =
(int *) malloc ((pp->knowledge->n_contains_none_rules + 1)
*(sizeof pp->relevant_contains_none_rules[0]));
pp->relevant_contains_one_rules[0] = -1;
pp->relevant_contains_none_rules[0] = -1;
pp->violation = NULL;
pp->n_local_rules_firing = 0;
pp->n_global_rules_firing = 0;
pp->q_pruned_rules = false;
pp_data = &pp->pp_data;
pp_data->vlength = PP_INITLEN;
pp_data->visited = (bool*) malloc(pp_data->vlength * sizeof(bool));
memset(pp_data->visited, 0, pp_data->vlength * sizeof(bool));
pp_data->links_to_ignore = NULL;
pp_new_domain_array(pp_data);
pp_data->wowlen = PP_INITLEN;
pp_data->word_links = (List_o_links **) malloc(pp_data->wowlen * sizeof(List_o_links*));
memset(pp_data->word_links, 0, pp_data->wowlen * sizeof(List_o_links *));
return pp;
}
void post_process_free(Postprocessor *pp)
{
PP_data *pp_data;
if (pp == NULL) return;
string_set_delete(pp->string_set);
pp_linkset_close(pp->set_of_links_of_sentence);
pp_linkset_close(pp->set_of_links_in_an_active_rule);
free(pp->relevant_contains_one_rules);
free(pp->relevant_contains_none_rules);
pp->knowledge = NULL;
pp->violation = NULL;
pp_data = &pp->pp_data;
post_process_free_data(pp_data);
free(pp_data->visited);
free(pp_data->domain_array);
free(pp_data->word_links);
free(pp);
}
static void post_process_scan_linkage(Postprocessor *pp, Linkage linkage)
{
size_t i;
if (pp == NULL) return;
for (i = 0; i < linkage->num_links; i++)
{
pp_linkset_add(pp->set_of_links_of_sentence,
linkage->link_array[i].link_name);
}
}
static size_t report_rule_use(pp_rule *set)
{
size_t cnt = 0;
for (size_t i=0; set[i].msg != NULL; i++)
{
err_msg(lg_Debug, "Used: %d rule: %s\n", set[i].use_count, set[i].msg);
cnt++;
}
return cnt;
}
static size_t report_unused_rule(pp_rule *set)
{
size_t cnt = 0;
for (size_t i=0; set[i].msg != NULL; i++)
{
if (0 == set[i].use_count)
{
err_msg(lg_Debug, "Unused rule: %s\n", set[i].msg);
cnt++;
}
}
return cnt;
}
static void report_pp_stats(Postprocessor *pp)
{
size_t rule_cnt = 0;
size_t unused_cnt = 0;
pp_knowledge * kno;
if (!verbosity_level(9)) return;
err_msg(lg_Debug, "PP stats: local_rules_firing=%d\n", pp->n_local_rules_firing);
kno = pp->knowledge;
err_msg(lg_Debug, "\nPP stats: form_a_cycle_rules\n");
rule_cnt += report_rule_use(kno->form_a_cycle_rules);
err_msg(lg_Debug, "\nPP stats: contains_one_rules\n");
rule_cnt += report_rule_use(kno->contains_one_rules);
err_msg(lg_Debug, "\nPP stats: contains_none_rules\n");
rule_cnt += report_rule_use(kno->contains_none_rules);
err_msg(lg_Debug, "\nPP stats: bounded_rules\n");
rule_cnt += report_rule_use(kno->bounded_rules);
err_msg(lg_Debug, "\nPP stats: Rules that were not used:\n");
unused_cnt += report_unused_rule(kno->form_a_cycle_rules);
unused_cnt += report_unused_rule(kno->contains_one_rules);
unused_cnt += report_unused_rule(kno->contains_none_rules);
unused_cnt += report_unused_rule(kno->bounded_rules);
err_msg(lg_Debug, "\nPP stats: %zu of %zu rules unused\n", unused_cnt, rule_cnt);
}
void do_post_process(Postprocessor *pp, Linkage sublinkage, bool is_long)
{
const char *msg;
PP_data *pp_data;
if (pp == NULL) return;
pp_data = &pp->pp_data;
pp_data->links_to_ignore = NULL;
pp_data->num_words = sublinkage->num_words;
if (pp_data->vlength <= pp_data->num_words)
{
size_t newsz;
pp_data->vlength += pp_data->num_words;
newsz = pp_data->vlength * sizeof(bool);
pp_data->visited = (bool *) realloc(pp_data->visited, newsz);
}
clear_visited(pp_data);
if (is_long && pp->q_pruned_rules == false)
{
prune_irrelevant_rules(pp);
}
pp->q_pruned_rules = true;
switch (internal_process(pp, sublinkage, &msg))
{
case -1:
pp->n_global_rules_firing++;
pp->violation = msg;
report_pp_stats(pp);
return;
case 1:
pp->n_local_rules_firing++;
pp->violation = msg;
break;
case 0:
pp->violation = NULL;
break;
}
report_pp_stats(pp);
}
void post_process_lkgs(Sentence sent, Parse_Options opts)
{
size_t in;
size_t N_linkages_post_processed = 0;
size_t N_valid_linkages = sent->num_valid_linkages;
size_t N_linkages_alloced = sent->num_linkages_alloced;
bool twopass = sent->length >= opts->twopass_length;
Postprocessor *pp = sent->postprocessor;
if (NULL == pp)
{
sent->num_linkages_post_processed = sent->num_valid_linkages;
for (in=0; in < N_linkages_alloced; in++)
{
Linkage lkg = &sent->lnkages[in];
linkage_score(lkg, opts);
}
return;
}
#define TCD 512
if (twopass)
{
for (in=0; in < N_linkages_alloced; in++)
{
Linkage lkg = &sent->lnkages[in];
Linkage_info *lifo = &lkg->lifo;
if (lifo->N_violations) continue;
post_process_scan_linkage(pp, lkg);
if (((TCD-1) == in%TCD) && resources_exhausted(opts->resources)) break;
}
}
for (in=0; in < N_linkages_alloced; in++)
{
Linkage lkg = &sent->lnkages[in];
Linkage_info *lifo = &lkg->lifo;
if (lifo->N_violations) continue;
do_post_process(pp, lkg, twopass);
post_process_free_data(&pp->pp_data);
if (NULL != pp->violation)
{
N_valid_linkages--;
lifo->N_violations++;
if (NULL == lifo->pp_violation_msg)
lifo->pp_violation_msg = pp->violation;
}
N_linkages_post_processed++;
linkage_score(lkg, opts);
if (((TCD-1) == in%TCD) && resources_exhausted(opts->resources)) break;
}
for (; in < N_linkages_alloced; in++)
{
Linkage lkg = &sent->lnkages[in];
Linkage_info *lifo = &lkg->lifo;
if (lifo->N_violations) continue;
N_valid_linkages--;
lifo->N_violations++;
if (NULL == lifo->pp_violation_msg)
lifo->pp_violation_msg = "Timeout during postprocessing";
}
print_time(opts, "Postprocessed all linkages");
if (verbosity_level(6))
{
err_msg(lg_Info, "%zu of %zu linkages with no P.P. violations\n",
N_valid_linkages, N_linkages_post_processed);
}
sent->num_linkages_post_processed = N_linkages_post_processed;
sent->num_valid_linkages = N_valid_linkages;
}
static void free_domain_names(PP_domains *ppi)
{
if (ppi->num_domains > 0) free(ppi->domain_name);
ppi->domain_name = NULL;
ppi->num_domains = 0;
}
void linkage_free_pp_domains(Linkage lkg)
{
size_t j;
if (!lkg || !lkg->pp_domains) return;
for (j = 0; j < lkg->num_links; ++j)
free_domain_names(&lkg->pp_domains[j]);
free(lkg->pp_domains);
lkg->pp_domains = NULL;
}
typedef struct D_type_list_s D_type_list;
struct D_type_list_s
{
D_type_list * next;
int type;
};
static void free_d_type(D_type_list * dtl)
{
D_type_list * dtlx;
for (; dtl != NULL; dtl = dtlx)
{
dtlx = dtl->next;
free((void*) dtl);
}
}
static D_type_list ** build_type_array(PP_data *pp_data,
size_t numlinks)
{
size_t nbytes = numlinks * sizeof(D_type_list*);
D_type_list** dta = malloc(nbytes);
memset(dta, 0, nbytes);
for (size_t d = 0; d < pp_data->N_domains; d++)
{
List_o_links * lol;
for (lol = pp_data->domain_array[d].lol; lol != NULL; lol = lol->next)
{
assert(lol->link < numlinks, "Something wrong about link numbering!");
D_type_list * dtl;
dtl = (D_type_list *) malloc(sizeof(D_type_list));
dtl->type = pp_data->domain_array[d].type;
dtl->next = dta[lol->link];
dta[lol->link] = dtl;
}
}
return dta;
}
static void linkage_set_domain_names(Postprocessor *postprocessor,
Linkage linkage)
{
if (NULL == linkage) return;
if (NULL == postprocessor) return;
if (0 == postprocessor->pp_data.N_domains) return;
if (postprocessor->violation != NULL) return;
D_type_list **dta = build_type_array(&postprocessor->pp_data,
linkage->num_links);
assert(NULL == linkage->pp_domains, "Not expecting pp_domains here!");
linkage->pp_domains = malloc(sizeof(PP_domains) * linkage->num_links);
memset(linkage->pp_domains, 0, sizeof(PP_domains) * linkage->num_links);
for (size_t j = 0; j < linkage->num_links; ++j)
{
D_type_list * d;
int k = 0;
for (d = dta[j]; d != NULL; d = d->next) k++;
linkage->pp_domains[j].num_domains = k;
if (k > 0)
{
linkage->pp_domains[j].domain_name =
(const char **) malloc(k * sizeof(const char *));
}
k = 0;
for (d = dta[j]; d != NULL; d = d->next)
{
char buff[] = {d->type, '\0'};
linkage->pp_domains[j].domain_name[k] =
string_set_add (buff, postprocessor->string_set);
k++;
}
}
for (size_t i=0; i<linkage->num_links; i++)
free_d_type(dta[i]);
free(dta);
}
void compute_domain_names(Linkage lkg)
{
Postprocessor *pp = lkg->sent->postprocessor;
if (NULL == pp) return;
Linkage_info *lifo = &lkg->lifo;
if (lifo->N_violations) return;
if (NULL != lkg->pp_domains) return;
do_post_process(pp, lkg, true);
linkage_set_domain_names(pp, lkg);
post_process_free_data(&pp->pp_data);
}
static inline bool verify_link_index(const Linkage linkage, LinkIdx index)
{
if (!linkage) return false;
if (index >= linkage->num_links) return false;
return true;
}
int linkage_get_link_num_domains(const Linkage linkage, LinkIdx index)
{
if (NULL == linkage->pp_domains) return -1;
if (!verify_link_index(linkage, index)) return -1;
return linkage->pp_domains[index].num_domains;
}
const char ** linkage_get_link_domain_names(const Linkage linkage, LinkIdx index)
{
if (NULL == linkage->pp_domains) return NULL;
if (!verify_link_index(linkage, index)) return NULL;
return linkage->pp_domains[index].domain_name;
}