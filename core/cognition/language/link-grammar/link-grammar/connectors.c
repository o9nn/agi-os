#include <limits.h>
#include "dict-common/dict-utils.h"
#include "api-structures.h"
#include "connectors.h"
#include "link-includes.h"
#include "utilities.h"
#define WILD_TYPE '*'
#define LENGTH_LINIT_WILD_TYPE WILD_TYPE
void free_connectors(Connector *e)
{
Connector * n;
for (; e != NULL; e = n)
{
n = e->next;
free(e);
}
}
static unsigned int get_connector_length_limit(condesc_t *cd,
Parse_Options opts)
{
if (NULL == opts) return UNLIMITED_LEN;
int short_len = opts->short_length;
bool all_short = opts->all_short;
int length_limit = cd->more->length_limit;
if ((all_short && (length_limit > short_len)) || (0 == length_limit))
return short_len;
return length_limit;
}
void set_connector_farthest_word(Exp *e, int w, int sent_length,
Parse_Options opts)
{
if (e->type == CONNECTOR_type)
{
assert(NULL != e->condesc, "NULL connector");
int length_limit = get_connector_length_limit(e->condesc, opts);
dassert(0 != length_limit, "Zero length_limit");
if (e->dir == '-')
e->farthest_word = MAX(0, w - length_limit);
else
e->farthest_word = MIN(sent_length-1, w + length_limit);
}
else
{
for (Exp *opd = e->operand_first; opd != NULL; opd = opd->operand_next)
{
set_connector_farthest_word(opd, w, sent_length, opts);
}
}
}
Connector * connector_new(Pool_desc *connector_pool, const condesc_t *desc)
{
Connector *c;
#if USE_SAT_SOLVER
if (NULL == connector_pool)
{
c = malloc(sizeof(Connector));
memset(c, 0, sizeof(Connector));
}
else
#endif
c = pool_alloc(connector_pool);
c->desc = desc;
return c;
}
static size_t get_connectors_from_expression(condesc_t **conlist, const Exp *e)
{
if (e->type == CONNECTOR_type)
{
if (NULL != conlist) *conlist = e->condesc;
return 1;
}
size_t cl_size = 0;
for (Exp *opd = e->operand_first; opd != NULL; opd = opd->operand_next)
{
cl_size += get_connectors_from_expression(conlist, opd);
if (NULL != conlist) conlist++;
}
return cl_size;
}
static int condesc_by_uc_num(const void *a, const void *b)
{
const condesc_t * const * cda = a;
const condesc_t * const * cdb = b;
if ((*cda)->uc_num < (*cdb)->uc_num) return -1;
if ((*cda)->uc_num > (*cdb)->uc_num) return 1;
return 0;
}
static void set_condesc_length_limit(Dictionary dict, const Exp *e, int length_limit)
{
size_t exp_num_con;
ConTable *ct = &dict->contable;
condesc_t **sdesc = ct->sdesc;
condesc_t **econlist;
exp_num_con = get_connectors_from_expression(NULL, e);
if (0 == exp_num_con) return;
econlist = alloca(exp_num_con * sizeof(*econlist));
get_connectors_from_expression(econlist, e);
qsort(econlist, exp_num_con, sizeof(*econlist), condesc_by_uc_num);
size_t restart_cn = 0, cn, en;
for (en = 0; en < exp_num_con; en++)
{
for (cn = restart_cn; cn < ct->num_con; cn++)
if (sdesc[cn]->uc_num >= econlist[en]->uc_num) break;
for (; en < exp_num_con; en++)
if (econlist[en]->uc_num >= sdesc[cn]->uc_num) break;
if (en == exp_num_con) break;
if (econlist[en]->uc_num != sdesc[cn]->uc_num) continue;
restart_cn = cn;
const char *wc_str = econlist[en]->more->string;
char *uc_wildcard = strchr(wc_str, LENGTH_LINIT_WILD_TYPE);
for (; cn < ct->num_con; cn++)
{
if (NULL == uc_wildcard)
{
if (econlist[en]->uc_num != sdesc[cn]->uc_num)
break;
if (!lc_easy_match(econlist[en], sdesc[cn]))
continue;
}
else
{
if (0 != strncmp(wc_str, sdesc[cn]->more->string, uc_wildcard - wc_str))
break;
}
sdesc[cn]->more->length_limit = length_limit;
}
}
}
static void condesc_length_limit_def_delete(ConTable *ct)
{
length_limit_def_t *l_next;
for (length_limit_def_t *l = ct->length_limit_def; NULL != l; l = l_next)
{
l_next = l->next;
free(l);
}
ct->length_limit_def = NULL;
}
static void set_all_condesc_length_limit(Dictionary dict)
{
ConTable *ct = &dict->contable;
bool unlimited_len_found = false;
for (length_limit_def_t *l = ct->length_limit_def; NULL != l; l = l->next)
{
set_condesc_length_limit(dict, l->defexp, l->length_limit);
if (UNLIMITED_LEN == l->length_limit) unlimited_len_found = true;
}
if (!unlimited_len_found)
{
condesc_t **sdesc = ct->sdesc;
for (size_t en = 0; en < ct->num_con; en++)
{
if (0 == sdesc[en]->more->length_limit)
sdesc[en]->more->length_limit = UNLIMITED_LEN;
}
}
condesc_length_limit_def_delete(&dict->contable);
if (verbosity_level(D_SPEC+1))
{
prt_error("Debug:\n%5s %-6s %3s\n\\", "num", "uc_num", "ll");
for (size_t n = 0; n < ct->num_con; n++)
{
prt_error("%5zu %6u %3d %s\n\\", n, ct->sdesc[n]->uc_num,
ct->sdesc[n]->more->length_limit, ct->sdesc[n]->more->string);
}
prt_error("\n");
}
}
static void connector_encode_lc(const char *lc_string, condesc_t *desc)
{
lc_enc_t lc_mask = 0;
lc_enc_t lc_value = 0;
lc_enc_t wildcard = LC_MASK;
const char *s;
for (s = lc_string; '\0' != *s; s++)
{
if (*s != WILD_TYPE)
{
lc_value |= (lc_enc_t)(*s & LC_MASK) << ((s-lc_string)*LC_BITS);
lc_mask |= wildcard;
}
wildcard <<= LC_BITS;
};
if ((size_t)(s-lc_string) > (sizeof(lc_value)/LC_BITS)*CHAR_BIT)
{
prt_error("Warning: Lower-case part '%s' is too long (%d>%d)\n",
lc_string, (int)(s-lc_string), MAX_CONNECTOR_LC_LENGTH);
}
desc->lc_mask = (lc_mask << 1) + !!(desc->more->flags & CD_HEAD_DEPENDENT);
desc->lc_letters = (lc_value << 1) + !!(desc->more->flags & CD_HEAD);
}
void calculate_connector_info(condesc_t *condesc)
{
const char *s;
condesc_more_t *m = condesc->more;
s = m->string;
if (islower((unsigned char)*s))
{
dassert((s[0] == 'h') || (s[0] == 'd'), "'%s': Bad head/dependent", s);
if ((s[0] == 'h') || (s[0] == 'd')) m->flags |= CD_HEAD_DEPENDENT;
if (s[0] == 'h') m->flags |= CD_HEAD;
s++;
}
m->uc_start = (uint8_t)(s - m->string);
do { s++; } while (is_connector_name_char(*s));
m->uc_length = (uint8_t)(s - m->string - m->uc_start);
connector_encode_lc(s, condesc);
}
static uint32_t connector_str_hash(const char *s)
{
#ifdef USE_DJB2
uint32_t i = 5381;
while (is_connector_name_char(*s))
{
i = ((i << 5) + i) + *s;
s++;
}
i += i>>14;
#endif
#define USE_JENKINS
#ifdef USE_JENKINS
uint32_t i = 0;
while (is_connector_name_char(*s))
{
i += *s;
i += (i<<10);
i ^= (i>>6);
s++;
}
i += (i << 3);
i ^= (i >> 11);
i += (i << 15);
#endif
#ifdef USE_SDBM
uint32_t i = 0;
c->uc_start = s - c->string;
while (is_connector_name_char(*s))
{
i = *s + (i << 6) + (i << 16) - i;
s++;
}
#endif
return i;
}
int condesc_by_uc_constring(const void * a, const void * b)
{
const condesc_t * const * cda = a;
const condesc_t * const * cdb = b;
if (NULL == *cda) return (NULL != *cdb);
if (NULL == *cdb) return -1;
const char *sa = &(*cda)->more->string[(*cda)->more->uc_start];
const char *sb = &(*cdb)->more->string[(*cdb)->more->uc_start];
int la = (*cda)->more->uc_length;
int lb = (*cdb)->more->uc_length;
if (la == lb)
{
return strncmp(sa, sb, la);
}
if (la < lb)
{
char *uca = strdupa(sa);
uca[la] = '\0';
return strncmp(uca, sb, lb);
}
else
{
char *ucb = strdupa(sb);
ucb[lb] = '\0';
return strncmp(sa, ucb, la);
}
}
static bool sort_condesc_by_uc_constring(Dictionary dict)
{
ConTable *ct = &dict->contable;
if ((0 == ct->num_con) && !IS_DYNAMIC_DICT(dict))
{
prt_error("Error: Dictionary %s: No connectors found.\n", dict->name);
return false;
}
if (0 == ct->num_con)
return true;
condesc_t **sdesc = malloc(ct->num_con * sizeof(condesc_t *));
size_t i = 0;
for (size_t n = 0; n < ct->size; n++)
{
condesc_t *condesc = ct->hdesc[n].desc;
if (NULL == condesc) continue;
calculate_connector_info(condesc);
sdesc[i++] = condesc;
}
qsort(sdesc, ct->num_con, sizeof(*ct->sdesc), condesc_by_uc_constring);
int uc_num = 0;
sdesc[0]->uc_num = uc_num;
for (size_t n = 1; n < ct->num_con; n++)
{
condesc_t **condesc = &sdesc[n];
if (condesc[0]->more->uc_length != condesc[-1]->more->uc_length)
{
uc_num++;
}
else
{
const char *uc1 = &condesc[0]->more->string[condesc[0]->more->uc_start];
const char *uc2 = &condesc[-1]->more->string[condesc[-1]->more->uc_start];
if (0 != strncmp(uc1, uc2, condesc[0]->more->uc_length))
{
uc_num++;
}
}
condesc[0]->uc_num = uc_num;
}
lgdebug(+11, "Dictionary %s: %zu different connectors "
"(%d with a different UC part)\n",
dict->name, ct->num_con, uc_num+1);
ct->sdesc = sdesc;
ct->num_uc = uc_num + 1;
return true;
}
void condesc_delete(Dictionary dict)
{
ConTable *ct = &dict->contable;
free(ct->hdesc);
pool_delete(ct->desc_pool);
pool_delete(ct->more_pool);
condesc_length_limit_def_delete(ct);
}
void condesc_reuse(Dictionary dict)
{
ConTable *ct = &dict->contable;
ct->num_con = 0;
ct->num_uc = 0;
memset(ct->hdesc, 0, ct->size * sizeof(hdesc_t));
pool_reuse(ct->desc_pool);
pool_reuse(ct->more_pool);
}
static hdesc_t *condesc_find(ConTable *ct, const char *constring, uint32_t hash)
{
uint32_t i = hash & (ct->size-1);
while ((NULL != ct->hdesc[i].desc) &&
!string_set_cmp(constring, ct->hdesc[i].desc->more->string))
{
i = (i + 1) & (ct->size-1);
}
return &ct->hdesc[i];
}
static void condesc_table_alloc(ConTable *ct, size_t size)
{
ct->hdesc = malloc(size * sizeof(hdesc_t));
memset(ct->hdesc, 0, size * sizeof(hdesc_t));
ct->size = size;
}
#define CONDESC_TABLE_GROWTH_FACTOR 2
static bool condesc_grow(ConTable *ct)
{
size_t old_size = ct->size;
hdesc_t *old_hdesc = ct->hdesc;
lgdebug(+11, "Growing ConTable from %zu\n", old_size);
condesc_table_alloc(ct, ct->size * CONDESC_TABLE_GROWTH_FACTOR);
for (size_t i = 0; i < old_size; i++)
{
condesc_t *old_desc = old_hdesc[i].desc;
if (NULL == old_desc) continue;
hdesc_t *new_hdesc =
condesc_find(ct, old_desc->more->string, old_desc->more->str_hash);
if (NULL != new_hdesc->desc)
{
prt_error("Fatal Error: condesc_grow(): Internal error\n");
free(old_hdesc);
return false;
}
new_hdesc->desc = old_desc;
}
free(old_hdesc);
return true;
}
condesc_t *condesc_add(ConTable *ct, const char *constring)
{
uint32_t hash = (connector_uc_hash_t)connector_str_hash(constring);
hdesc_t *h = condesc_find(ct, constring, hash);
if (NULL == h->desc)
{
lgdebug(+11, "Creating connector '%s' (%zu)\n", constring, ct->num_con);
h->desc = pool_alloc(ct->desc_pool);
h->desc->uc_num = UINT32_MAX;
h->desc->con_num = ct->num_con;
condesc_more_t *m = h->desc->more = pool_alloc(ct->desc_pool);
m->string = constring;
m->str_hash = hash;
ct->num_con++;
if ((8 * ct->num_con) > (3 * ct->size))
{
if (!condesc_grow(ct)) return NULL;
h = condesc_find(ct, constring, hash);
}
}
return h->desc;
}
void condesc_init(Dictionary dict, size_t num_con)
{
ConTable *ct = &dict->contable;
ct->desc_pool = pool_new(__func__, "condesc_t",
num_con, sizeof(condesc_t),
true, true, false);
ct->more_pool = pool_new(__func__, "condesc_more_t",
num_con, sizeof(condesc_more_t),
true, true, false);
int nbits = 0;
while (num_con) { nbits++; num_con >>= 1; }
nbits += 2;
condesc_table_alloc(ct, 1<<nbits);
ct->length_limit_def = NULL;
ct->length_limit_def_next = &ct->length_limit_def;
}
void condesc_setup(Dictionary dict)
{
sort_condesc_by_uc_constring(dict);
set_all_condesc_length_limit(dict);
free(dict->contable.sdesc);
}