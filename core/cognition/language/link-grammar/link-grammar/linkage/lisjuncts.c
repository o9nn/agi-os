#include <stdlib.h>
#include <string.h>
#include "api-structures.h"
#include "connectors.h"
#include "disjunct-utils.h"
#include "linkage.h"
#include "lisjuncts.h"
#include "string-set.h"
#ifdef DEBUG_lisjuncts
#include "print/print-util.h"
static void assert_same_disjunct(Linkage, WordIdx, const char *);
#endif
void lg_compute_disjunct_strings(Linkage lkg)
{
char djstr[MAX_LINK_NAME_LENGTH*20];
size_t nwords = lkg->num_words;
if (lkg->disjunct_list_str) return;
lkg->disjunct_list_str = malloc(nwords * sizeof(char *));
for (WordIdx w = 0; w < nwords; w++)
{
size_t len = 0;
for (int dir = 0; dir < 2; dir++)
{
int last_multi_tracon_id = 0;
for (LinkIdx i = lkg->num_links-1; i != (WordIdx)-1; i--)
{
Link *lnk = &lkg->link_array[i];
Connector *c;
if (0 == dir)
{
if (lnk->rw != w) continue;
c = lnk->rc;
}
else
{
if (lnk->lw != w) continue;
c = lnk->lc;
}
if (c->multi)
{
if (last_multi_tracon_id == c->tracon_id) continue;
last_multi_tracon_id = c->tracon_id;
djstr[len++] = '@';
}
len += lg_strlcpy(djstr+len, connector_string(c), sizeof(djstr)-len);
if (len >= sizeof(djstr) - 3)
{
len = sizeof(djstr) - 1;
break;
}
djstr[len++] = (dir == 0) ? '-' : '+';
djstr[len++] = ' ';
}
}
if ((len > 0) && (djstr[len-1] == ' ')) len--;
djstr[len++] = '\0';
#ifdef DEBUG_lisjuncts
assert_same_disjunct(lkg, w, djstr);
#endif
lkg->disjunct_list_str[w] = string_set_add(djstr, lkg->sent->string_set);
}
}
void lg_free_disjunct_strings(Linkage lkg)
{
free(lkg->disjunct_list_str);
}
#ifdef DEBUG_lisjuncts
static void assert_same_disjunct(Linkage lkg, WordIdx w, const char *djstr)
{
char *cs;
if (lkg->chosen_disjuncts[w])
{
cs = print_one_disjunct(lkg->chosen_disjuncts[w]);
char *cs_lastchar = &cs[strlen(cs)-1];
if (*cs_lastchar == ' ') *cs_lastchar = '\0';
}
else
cs = (char *)"";
assert(strcmp(cs, djstr) == 0,
"Word %zu: Inconsistent disjunct string %s (link_array %s)",
w, cs, djstr);
if (lkg->chosen_disjuncts[w])
free(cs);
}
#endif