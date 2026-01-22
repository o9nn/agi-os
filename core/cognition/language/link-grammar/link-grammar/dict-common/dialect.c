#include <string.h>
#include "api-structures.h"
#include "dialect.h"
#include "dict-api.h"
#include "dict-common.h"
#include "dict-structures.h"
#include "dict-file/read-dialect.h"
#include "file-utils.h"
#include "string-id.h"
#include "string-set.h"
#define D_DIALECT 7
static void free_dialect_table(Dialect *di)
{
free(di->table);
free(di->kept_input);
}
void free_dialect(Dialect *di)
{
if (di == NULL) return;
free_dialect_table(di);
free(di->section);
string_id_delete(di->section_set);
free(di);
}
Dialect *dialect_alloc(void)
{
Dialect *di = malloc(sizeof(*di));
memset(di, 0, sizeof(*di));
di->section_set = string_id_create();
return di;
}
unsigned int exptag_dialect_add(Dictionary dict, const char *tag)
{
expression_tag *dt = &dict->dialect_tag;
unsigned int tag_index = string_id_lookup(tag, dt->set);
if (tag_index != SID_NOTFOUND) return tag_index;
tag_index = string_id_add(tag, dt->set);
tag = string_set_add(tag, dict->string_set);
if (dt->num == dt->size)
{
if (dt->num == 0)
dt->size = EXPTAG_SZ;
else
dt->size *= 2;
dt->name = realloc(dt->name, dt->size * sizeof(*dt->name));
}
dt->name[tag_index] = tag;
dt->num++;
assert(dt->num == tag_index, "Tag index mismatch");
return tag_index;
}
static bool apply_component(Dictionary dict, Dialect *di,
unsigned int table_index, float *cost_table)
{
expression_tag *dt = &dict->dialect_tag;
unsigned int cost_index =
string_id_lookup(di->table[table_index].name, dt->set);
if (cost_index == SID_NOTFOUND)
{
prt_error("Error: Dialect component \"%s\" is not in the dictionary.\n",
di->table[table_index].name);
return false;
}
cost_table[cost_index] = di->table[table_index].cost;
return true;
}
static bool apply_table_entry(Dictionary dict, Dialect *from,
unsigned int table_index, Dialect *to,
dialect_info *dinfo, bool *encountered)
{
int skip = (int)(to == from);
for (unsigned int i = table_index + skip; i < from->num_table_tags; i++)
{
if (cost_eq(from->table[i].cost, DIALECT_SECTION)) break;
lgdebug(+D_DIALECT, "Apply %s %s%s\n",
from->table[i].name, cost_stringify(from->table[i].cost),
(to == from) ? "" : " (user setup)");
if (!cost_eq(from->table[i].cost, DIALECT_SUB))
{
if (!apply_component(dict, from, i, dinfo->cost_table)) return false;
}
else
{
unsigned int sub_index = SID_NOTFOUND;
if (to != NULL)
sub_index = string_id_lookup(from->table[i].name, to->section_set);
if (sub_index == SID_NOTFOUND)
{
prt_error("Error: Undefined dialect \"%s\"\n", from->table[i].name);
return false;
}
if (encountered[sub_index])
{
prt_error("Error: Loop detected at sub-dialect \"%s\" "
"(of dialect \"%s\").\n",
to->table[i].name, to->table[table_index].name);
return false;
}
encountered[sub_index] = true;
if (!apply_table_entry(dict, to, to->section[sub_index].index, to,
dinfo, encountered))
return false;
}
}
return true;
}
bool apply_dialect(Dictionary dict, Dialect *from, unsigned int table_index,
Dialect *to, dialect_info *dinfo)
{
bool *loopdet;
if (to == NULL)
{
loopdet = NULL;
}
else
{
loopdet = alloca(to->num_sections + 1);
memset(loopdet, 0, to->num_sections + 1);
}
if (!apply_table_entry(dict, from, table_index, to, dinfo, loopdet))
return false;
return true;
}
static void print_cost_table(Dictionary dict, Dialect *di, dialect_info *dinfo)
{
expression_tag *dt = &dict->dialect_tag;
if (dt->num == 0)
{
assert(dinfo->cost_table == NULL, "Unexpected cost table.");
prt_error("Debug: No dialect cost table (no tags in the dict).\n");
return;
}
if (dinfo->cost_table == NULL)
{
prt_error("Debug: No dialect cost table.\n");
return;
}
prt_error("Dialect cost table (%u component%s):\n\\",
dt->num, dt->num == 1 ? "" : "s");
prt_error("%-15s %s\n", "component", "cost");
for (unsigned int i = 1; i <= dt->num; i++)
{
prt_error("%-15s %s\n\\",
dt->name[i], cost_stringify(dinfo->cost_table[i]));
}
lg_error_flush();
}
void free_cost_table(Parse_Options opts)
{
free(opts->dialect.cost_table);
opts->dialect.cost_table = NULL;
}
static bool dialect_conf_exists(dialect_info *dinfo)
{
for (const char *p = dinfo->conf; *p != '\0'; p++)
if (!lg_isspace((unsigned char)*p)) return true;
return false;
}
const char no_dialect[] = "(unset the dialect option)\n";
bool setup_dialect(Dictionary dict, Parse_Options opts)
{
Dialect *di = dict->dialect;
dialect_info *dinfo = &opts->dialect;
expression_tag *dt = &dict->dialect_tag;
if (dt->num == 0)
{
if (!dialect_conf_exists(dinfo)) return true;
prt_error("Error: Dialect setup failed: No dialects in the \"%s\" "
"dictionary %s.\n", dict->lang, no_dialect);
return false;
}
if (dinfo->cost_table != NULL)
{
if ((dinfo->dict != dict) || (dict->cached_dialect != dinfo))
{
lgdebug(+D_DIALECT,
"Debug: Resetting dialect cache of a different dictionary.\n");
free_cost_table(opts);
}
else
{
lgdebug(+D_DIALECT, "Debug: Cached cost table found\n");
if (verbosity_level(+D_DIALECT+1))
print_cost_table(dict, di, dinfo);
return true;
}
}
dinfo->dict = dict;
dict->cached_dialect = dinfo;
if (dt->num != 0)
{
dinfo->cost_table = malloc((dt->num + 1) * sizeof(*dinfo->cost_table));
for (unsigned int i = 1; i <= dt->num; i++)
dinfo->cost_table[i] = DIALECT_COST_DISABLE;
}
if ((di != NULL) && (di->section != NULL) &&
(di->section[0].index != NO_INDEX))
{
if (!apply_dialect(dict, di, di->section[0].index, di, dinfo))
return false;
}
if (dialect_conf_exists(dinfo))
{
Dialect user_setup = (Dialect){ 0 };
if (!dialect_read_from_one_line_str(dict, &user_setup, dinfo->conf))
{
free_dialect_table(&user_setup);
return false;
}
if (!apply_dialect(dict, &user_setup, 0, di, dinfo))
{
free_dialect_table(&user_setup);
return false;
}
free_dialect_table(&user_setup);
}
if (verbosity_level(+D_DIALECT+1))
print_cost_table(dict, di, dinfo);
return true;
}