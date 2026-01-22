#include <stdarg.h>
#include "api-structures.h"
#include "disjunct-utils.h"
#include "linkage.h"
#include "score.h"
static inline int cost_for_length(int length)
{
return length-1;
}
static size_t compute_link_cost(Linkage lkg)
{
size_t lcost, i;
lcost = 0;
for (i = 0; i < lkg->num_links; i++)
{
lcost += cost_for_length(lkg->link_array[i].rw - lkg->link_array[i].lw);
}
return lcost;
}
static int unused_word_cost(Linkage lkg)
{
int lcost;
size_t i;
lcost = 0;
for (i = 0; i < lkg->num_words; i++)
lcost += (lkg->chosen_disjuncts[i] == NULL);
return lcost;
}
static float compute_disjunct_cost(Linkage lkg)
{
size_t i;
float lcost;
lcost = 0.0;
for (i = 0; i < lkg->num_words; i++)
{
Disjunct * dj = lkg->chosen_disjuncts[i];
if (dj != NULL)
lcost += dj->is_category ? dj->category[0].cost : dj->cost;
}
return lcost;
}
void linkage_score(Linkage lkg, Parse_Options opts)
{
lkg->lifo.unused_word_cost = unused_word_cost(lkg);
lkg->lifo.disjunct_cost = compute_disjunct_cost(lkg);
lkg->lifo.link_cost = compute_link_cost(lkg);
}