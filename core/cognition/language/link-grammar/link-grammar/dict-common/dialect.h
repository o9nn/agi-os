#ifndef _DIALECT_H_
#define _DIALECT_H_
#include <ctype.h>
#include <stdint.h>
#include "api-types.h"
#include "dict-structures.h"
#include "string-id.h"
#define DIALECT_COST_MAX         9999.0F
#define DIALECT_COST_DISABLE    10000.0F
#define DIALECT_SUB             10001.0F
#define DIALECT_SECTION         10002.0F
typedef struct
{
const char *name;
float cost;
} dialect_tag;
typedef struct
{
const char *name;
unsigned int index;
} dialect_section_tag;
#define NO_INDEX ((unsigned int)-1)
struct Dialect_s
{
dialect_tag *table;
String_id *section_set;
dialect_section_tag *section;
char *kept_input;
unsigned int num_table_tags;
unsigned int num_sections;
};
struct dialect_option_s
{
Dictionary dict;
char *conf;
float *cost_table;
};
typedef struct dialect_option_s dialect_info;
Dialect *dialect_alloc(void);
void free_dialect(Dialect *);
unsigned int exptag_dialect_add(Dictionary, const char *);
bool setup_dialect(Dictionary, Parse_Options);
void free_cost_table(Parse_Options opts);
bool apply_dialect(Dictionary, Dialect *, unsigned int, Dialect *, dialect_info *);
static inline const char *valid_dialect_name(const char *name)
{
if (!isalpha((unsigned char)name[0])) return name;
while (*++name != '\0')
{
if (!isalnum((unsigned char)name[0]) && name[0] != '_' && name[0] != '-')
return name;
}
return NULL;
}
#endif