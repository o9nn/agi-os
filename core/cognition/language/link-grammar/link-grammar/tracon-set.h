#ifndef _TRACON_SET_H_
#define _TRACON_SET_H_
#include <string.h>
#include <stddef.h>
#include <stdint.h>
#include "api-types.h"
#include "connectors.h"
#include "const-prime.h"
#include "error.h"
#ifdef DEBUG
#ifndef TRACON_SET_DEBUG
#define TRACON_SET_DEBUG
#endif
#endif
typedef connector_hash_t tid_hash_t;
typedef struct
{
Connector *clist;
tid_hash_t hash;
#ifdef TRACON_SET_DEBUG
unsigned int pri_collN;
unsigned int sec_collN;
#endif
} clist_slot;
typedef struct
{
size_t size;
size_t available_count;
clist_slot *table;
prime_mod_func_t mod_func;
unsigned int prime_idx;
bool shallow;
#ifdef TRACON_SET_DEBUG
size_t addN;
size_t pri_collN;
size_t sec_collN;
unsigned int resetN;
#endif
} Tracon_set;
#define MAX_TRACON_SET_TABLE_SIZE(s) ((s) * 3 / 8)
Tracon_set *tracon_set_create(void);
Connector **tracon_set_add(Connector *, Tracon_set *);
Connector *tracon_set_lookup(const Connector *, Tracon_set *);
void tracon_set_delete(Tracon_set *);
void tracon_set_shallow(bool, Tracon_set *);
void tracon_set_reset(Tracon_set *);
#endif