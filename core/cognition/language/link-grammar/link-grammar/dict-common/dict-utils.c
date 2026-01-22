#include "api-structures.h"
#include "connectors.h"
#include "dict-api.h"
#include "string-set.h"
#include "dict-defines.h"
#include "dict-utils.h"
void patch_subscript(char * s)
{
char *ds, *de;
int dp;
ds = strrchr(s, SUBSCRIPT_DOT);
if (!ds) return;
de = ds + 1;
if (*de == '\0') return;
dp = (int) *de;
if (127 < dp || dp < 0) return;
if (isdigit(dp)) return;
*ds = SUBSCRIPT_MARK;
}
static size_t exp_memory_size(const Exp * e)
{
size_t size;
if (e == NULL) return 0;
if (e->type == CONNECTOR_type) return 1;
size = 1;
for (Exp *opd = e->operand_first; opd != NULL; opd = opd->operand_next)
size += exp_memory_size(opd);
return size;
}
static Exp *create_external_exp(const Exp *e, Exp **exp_mem, Parse_Options opts)
{
if (e == NULL) return NULL;
Exp *new_e = (*exp_mem)++;
*new_e = *e;
if (opts != NULL)
{
if ((e->type != CONNECTOR_type) && (Exptag_dialect == e->tag_type))
new_e->cost += opts->dialect.cost_table[new_e->tag_id];
}
if (CONNECTOR_type == e->type) return new_e;
Exp **tmp_e_a = &new_e->operand_first;
for(Exp *opd = e->operand_first; opd != NULL; opd = opd->operand_next)
{
*tmp_e_a = create_external_exp(opd, exp_mem, opts);
tmp_e_a = &(*tmp_e_a)->operand_next;
}
*tmp_e_a = NULL;
return new_e;
}
const char * lg_exp_get_string(const Exp* exp)
{
return exp->condesc->more->string;
}
Exp *lg_exp_resolve(Dictionary dict, const Exp *e, Parse_Options opts)
{
if (opts != NULL)
{
if (!setup_dialect(dict, opts)) return NULL;
}
size_t elen = exp_memory_size(e);
Exp *exp_mem = malloc(elen * sizeof(Exp));
return create_external_exp(e, &exp_mem, opts);
}
#if 0
void free_Exp(Exp *e)
{
if (NULL == e) return;
Exp *operand_next;
if (e->type != CONNECTOR_type)
{
for (Exp *opd = e->operand_first; opd != NULL; opd = operand_next)
{
operand_next = opd->operand_next;
free_Exp(opd);
}
}
free(e);
}
#endif
int size_of_expression(Exp * e)
{
if (NULL == e) return 0;
if (e->type == CONNECTOR_type) return 1;
int size = 0;
for (Exp *opd = e->operand_first; opd != NULL; opd = opd->operand_next)
size += size_of_expression(opd);
return size;
}
Exp *copy_Exp(Exp *e, Pool_desc *Exp_pool, Parse_Options opts)
{
if (e == NULL) return NULL;
Exp *new_e = pool_alloc(Exp_pool);
*new_e = *e;
if (opts) {
if ((e->type != CONNECTOR_type) && (Exptag_dialect == e->tag_type))
new_e->cost += opts->dialect.cost_table[new_e->tag_id];
}
#if 0
new_e->operand_next = copy_Exp(e->operand_next, Exp_pool);
if (CONNECTOR_type == e->type) return new_e;
new_e->operand_first = copy_Exp(e->operand_first, Exp_pool);
#else
if (CONNECTOR_type == e->type) return new_e;
Exp **tmp_e_a = &new_e->operand_first;
for(Exp *opd = e->operand_first; opd != NULL; opd = opd->operand_next)
{
*tmp_e_a = copy_Exp(opd, Exp_pool, opts);
tmp_e_a = &(*tmp_e_a)->operand_next;
}
*tmp_e_a = NULL;
#endif
return new_e;
}
static bool exp_compare(Exp *e1, Exp *e2)
{
if ((e1 == NULL) && (e2 == NULL))
return true;
if ((e1 == NULL) || (e2 == NULL))
return false;
if (e1->type != e2->type)
return false;
if (!cost_eq(e1->cost, e2->cost))
return false;
if (e1->type == CONNECTOR_type)
{
if (e1->condesc != e2->condesc)
return false;
if (e1->dir != e2->dir)
return false;
}
else
{
for (e1 = e1->operand_first, e2 = e2->operand_first;
(e1 != NULL) && (e2 != NULL);
e1 = e1->operand_next, e2 = e2->operand_next)
{
if (!exp_compare(e1, e2))
return false;
}
return ((e1 == NULL) && (e2 == NULL));
}
return true;
}
static int exp_contains(Exp * super, Exp * sub)
{
#if 0
if (super) printf("SUP: %s\n", exp_stringify(super));
#endif
if (sub==NULL || super==NULL)
return 0;
if (exp_compare(sub,super))
return 1;
if (super->type==CONNECTOR_type)
return 0;
for(Exp *opd = super->operand_first; opd != NULL; opd = opd->operand_next)
{
if (exp_contains(opd, sub)==1)
return 1;
}
return 0;
}
static bool exp_has_connector(const Exp * e, int depth,
const char * cs, char direction)
{
if (e->type == CONNECTOR_type)
{
if (direction != e->dir) return false;
return string_set_cmp(e->condesc->more->string, cs);
}
if (depth == 0) return false;
if (depth > 0) depth--;
for (Exp *opd = e->operand_first; opd != NULL; opd = opd->operand_next)
{
if (exp_has_connector(opd, depth, cs, direction))
return true;
}
return false;
}
bool is_exp_like_empty_word(Dictionary dict, Exp *exp)
{
if (NULL == dict->zzz_connector) return false;
return exp_has_connector(exp, 2, dict->zzz_connector, '-');
}
static bool dn_word_contains(Dictionary dict,
Dict_node * w_dn, const char * macro)
{
Exp * m_exp;
Dict_node *m_dn;
if (w_dn == NULL) return false;
m_dn = dictionary_lookup_list(dict, macro);
if (m_dn == NULL) return false;
m_exp = m_dn->exp;
#if 0
printf("\nWORD: %s\n", exp_stringify(w_dn->exp));
printf("\nMACR: %s\n", exp_stringify(m_exp));
#endif
for (;w_dn != NULL; w_dn = w_dn->right)
{
if (1 == exp_contains(w_dn->exp, m_exp))
{
free_lookup_list(dict, m_dn);
return true;
}
}
free_lookup_list(dict, m_dn);
return false;
}
bool word_contains(Dictionary dict, const char * word, const char * macro)
{
Dict_node *w_dn = dictionary_lookup_list(dict, word);
bool ret = dn_word_contains(dict, w_dn, macro);
free_lookup_list(dict, w_dn);
return ret;
}