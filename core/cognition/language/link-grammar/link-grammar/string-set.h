#ifndef _STRING_SET_H_
#define _STRING_SET_H_
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include "api-types.h"
#include "const-prime.h"
#include "error.h"
#ifdef _WIN32
#include "utilities.h"
#endif
typedef struct
{
const char *str;
unsigned int hash;
} ss_slot;
typedef struct str_mem_pool_s str_mem_pool;
struct String_set_s
{
size_t size;
size_t count;
size_t available_count;
ss_slot *table;
unsigned int prime_idx;
prime_mod_func_t mod_func;
ssize_t pool_free_count;
char *alloc_next;
str_mem_pool *string_pool;
};
#define MAX_STRING_SET_TABLE_SIZE(s) ((s) * 3 / 4)
String_set * string_set_create(void);
const char * string_set_add(const char * source_string, String_set * ss);
const char * string_set_lookup(const char * source_string, String_set * ss);
void string_set_delete(String_set *ss);
static inline bool string_set_cmp(const char *s1, const char *s2)
{
#ifdef DEBUG
size_t p1 = ((strlen(s1)+1)&~(sizeof(String_set *)-1))+sizeof(String_set *);
size_t p2 = ((strlen(s2)+1)&~(sizeof(String_set *)-1))+sizeof(String_set *);
assert(*(String_set **)&s1[p1] == *(String_set **)&s2[p2],
"Strings '%s' and '%s' are not from the same string_set", s1, s2);
assert((s1 != s2) == !!strcmp(s1, s2),
"Bogus string-set string comparison ('%s' and '%s')", s1, s2);
#endif
return s1 == s2;
}
#endif