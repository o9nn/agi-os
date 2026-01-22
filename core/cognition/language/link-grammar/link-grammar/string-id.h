#ifndef _STRING_ID_H_
#define _STRING_ID_H_
#include <string.h>
#include <stddef.h>
#include <stdlib.h>
#include "api-types.h"
#include "const-prime.h"
#include "error.h"
#ifdef _WIN32
#include "utilities.h"
#endif
#define SID_NOTFOUND 0
typedef struct
{
const char *str;
unsigned int id;
unsigned int hash;
} ss_id;
typedef struct str_mem_pool_s str_mem_pool;
struct String_id_s
{
size_t size;
size_t count;
size_t available_count;
ss_id *table;
unsigned int prime_idx;
prime_mod_func_t mod_func;
ssize_t pool_free_count;
char *alloc_next;
str_mem_pool *string_pool;
};
#define MAX_STRING_SET_TABLE_SIZE(s) ((s) * 3 / 4)
String_id *string_id_create(void);
unsigned int string_id_add(const char *source_string, String_id *ss);
unsigned int string_id_lookup(const char *source_string, String_id *ss);
void string_id_delete(String_id *ss);
#endif