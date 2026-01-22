#include <ctype.h>
#include <string.h>
#include "analyze-linkage.h"
#include "connectors.h"
#include "dict-common/dict-common.h"
#include "linkage.h"
#include "string-set.h"
const char *intersect_strings(String_set *sset, const Connector *c1,
const Connector *c2)
{
const condesc_t *d1 = c1->desc;
const condesc_t *d2 = c2->desc;
char l[MAX_TOKEN_LENGTH + 1];
lc_enc_t lc1_letters = d1->lc_letters >> 1;
lc_enc_t lc2_letters = d2->lc_letters >> 1;
lc_enc_t lc_label = lc1_letters | lc2_letters;
if (lc_label == lc1_letters) return &connector_string(c1)[d1->more->uc_start];
if (lc_label == lc2_letters) return &connector_string(c2)[d2->more->uc_start];
memcpy(l, &connector_string(c1)[d1->more->uc_start], d1->more->uc_length);
for (size_t i = d1->more->uc_length; ; i++)
{
l[i] = lc_label & LC_MASK;
if (l[i] == '\0') l[i] = '*';
lc_label >>= LC_BITS;
if (lc_label == 0)
{
l[i+1] = '\0';
break;
}
}
#ifdef DEBUG
const char *s1 = &connector_string(c1)[d1->more->uc_start];
const char *s2 = &connector_string(c1)[d1->more->uc_start];
do
{
assert(is_connector_name_char(*s1) == is_connector_name_char(*s2),
"Invalid uppercase part!");
assert(*s1 == *s2 || *s1 == '*' || *s2 == '*', "Invalid intersection!");
}
while ((*s1++ != '0') && (*s2++ != 0));
#endif
return string_set_add(l, sset);
}
void compute_link_names(Linkage lkg, String_set *sset)
{
size_t i;
for (i = 0; i < lkg->num_links; i++)
{
lkg->link_array[i].link_name = intersect_strings(sset,
lkg->link_array[i].lc, lkg->link_array[i].rc);
}
}