#ifndef HEADER_LHASH_H
# define HEADER_LHASH_H
# include <openssl/e_os2.h>
# ifndef OPENSSL_NO_FP_API
#  include <stdio.h>
# endif
# ifndef OPENSSL_NO_BIO
#  include <openssl/bio.h>
# endif
#ifdef  __cplusplus
extern "C" {
#endif
typedef struct lhash_node_st {
void *data;
struct lhash_node_st *next;
# ifndef OPENSSL_NO_HASH_COMP
unsigned long hash;
# endif
} LHASH_NODE;
typedef int (*LHASH_COMP_FN_TYPE) (const void *, const void *);
typedef unsigned long (*LHASH_HASH_FN_TYPE) (const void *);
typedef void (*LHASH_DOALL_FN_TYPE) (void *);
typedef void (*LHASH_DOALL_ARG_FN_TYPE) (void *, void *);
# define DECLARE_LHASH_HASH_FN(name, o_type) \
unsigned long name##_LHASH_HASH(const void *);
# define IMPLEMENT_LHASH_HASH_FN(name, o_type) \
unsigned long name##_LHASH_HASH(const void *arg) { \
const o_type *a = arg; \
return name##_hash(a); }
# define LHASH_HASH_FN(name) name##_LHASH_HASH
# define DECLARE_LHASH_COMP_FN(name, o_type) \
int name##_LHASH_COMP(const void *, const void *);
# define IMPLEMENT_LHASH_COMP_FN(name, o_type) \
int name##_LHASH_COMP(const void *arg1, const void *arg2) { \
const o_type *a = arg1;             \
const o_type *b = arg2; \
return name##_cmp(a,b); }
# define LHASH_COMP_FN(name) name##_LHASH_COMP
# define DECLARE_LHASH_DOALL_FN(name, o_type) \
void name##_LHASH_DOALL(void *);
# define IMPLEMENT_LHASH_DOALL_FN(name, o_type) \
void name##_LHASH_DOALL(void *arg) { \
o_type *a = arg; \
name##_doall(a); }
# define LHASH_DOALL_FN(name) name##_LHASH_DOALL
# define DECLARE_LHASH_DOALL_ARG_FN(name, o_type, a_type) \
void name##_LHASH_DOALL_ARG(void *, void *);
# define IMPLEMENT_LHASH_DOALL_ARG_FN(name, o_type, a_type) \
void name##_LHASH_DOALL_ARG(void *arg1, void *arg2) { \
o_type *a = arg1; \
a_type *b = arg2; \
name##_doall_arg(a, b); }
# define LHASH_DOALL_ARG_FN(name) name##_LHASH_DOALL_ARG
typedef struct lhash_st {
LHASH_NODE **b;
LHASH_COMP_FN_TYPE comp;
LHASH_HASH_FN_TYPE hash;
unsigned int num_nodes;
unsigned int num_alloc_nodes;
unsigned int p;
unsigned int pmax;
unsigned long up_load;
unsigned long down_load;
unsigned long num_items;
unsigned long num_expands;
unsigned long num_expand_reallocs;
unsigned long num_contracts;
unsigned long num_contract_reallocs;
unsigned long num_hash_calls;
unsigned long num_comp_calls;
unsigned long num_insert;
unsigned long num_replace;
unsigned long num_delete;
unsigned long num_no_delete;
unsigned long num_retrieve;
unsigned long num_retrieve_miss;
unsigned long num_hash_comps;
int error;
} _LHASH;
# define LH_LOAD_MULT    256
# define lh_error(lh)    ((lh)->error)
_LHASH *lh_new(LHASH_HASH_FN_TYPE h, LHASH_COMP_FN_TYPE c);
void lh_free(_LHASH *lh);
void *lh_insert(_LHASH *lh, void *data);
void *lh_delete(_LHASH *lh, const void *data);
void *lh_retrieve(_LHASH *lh, const void *data);
void lh_doall(_LHASH *lh, LHASH_DOALL_FN_TYPE func);
void lh_doall_arg(_LHASH *lh, LHASH_DOALL_ARG_FN_TYPE func, void *arg);
unsigned long lh_strhash(const char *c);
unsigned long lh_num_items(const _LHASH *lh);
# ifndef OPENSSL_NO_FP_API
void lh_stats(const _LHASH *lh, FILE *out);
void lh_node_stats(const _LHASH *lh, FILE *out);
void lh_node_usage_stats(const _LHASH *lh, FILE *out);
# endif
# ifndef OPENSSL_NO_BIO
void lh_stats_bio(const _LHASH *lh, BIO *out);
void lh_node_stats_bio(const _LHASH *lh, BIO *out);
void lh_node_usage_stats_bio(const _LHASH *lh, BIO *out);
# endif
# define LHASH_OF(type) struct lhash_st_##type
# define DECLARE_LHASH_OF(type) LHASH_OF(type) { int dummy; }
# define CHECKED_LHASH_OF(type,lh) \
((_LHASH *)CHECKED_PTR_OF(LHASH_OF(type),lh))
# define LHM_lh_new(type, name) \
((LHASH_OF(type) *)lh_new(LHASH_HASH_FN(name), LHASH_COMP_FN(name)))
# define LHM_lh_error(type, lh) \
lh_error(CHECKED_LHASH_OF(type,lh))
# define LHM_lh_insert(type, lh, inst) \
((type *)lh_insert(CHECKED_LHASH_OF(type, lh), \
CHECKED_PTR_OF(type, inst)))
# define LHM_lh_retrieve(type, lh, inst) \
((type *)lh_retrieve(CHECKED_LHASH_OF(type, lh), \
CHECKED_PTR_OF(type, inst)))
# define LHM_lh_delete(type, lh, inst) \
((type *)lh_delete(CHECKED_LHASH_OF(type, lh),                        \
CHECKED_PTR_OF(type, inst)))
# define LHM_lh_doall(type, lh,fn) lh_doall(CHECKED_LHASH_OF(type, lh), fn)
# define LHM_lh_doall_arg(type, lh, fn, arg_type, arg) \
lh_doall_arg(CHECKED_LHASH_OF(type, lh), fn, CHECKED_PTR_OF(arg_type, arg))
# define LHM_lh_num_items(type, lh) lh_num_items(CHECKED_LHASH_OF(type, lh))
# define LHM_lh_down_load(type, lh) (CHECKED_LHASH_OF(type, lh)->down_load)
# define LHM_lh_node_stats_bio(type, lh, out) \
lh_node_stats_bio(CHECKED_LHASH_OF(type, lh), out)
# define LHM_lh_node_usage_stats_bio(type, lh, out) \
lh_node_usage_stats_bio(CHECKED_LHASH_OF(type, lh), out)
# define LHM_lh_stats_bio(type, lh, out) \
lh_stats_bio(CHECKED_LHASH_OF(type, lh), out)
# define LHM_lh_free(type, lh) lh_free(CHECKED_LHASH_OF(type, lh))
DECLARE_LHASH_OF(OPENSSL_STRING);
DECLARE_LHASH_OF(OPENSSL_CSTRING);
#ifdef  __cplusplus
}
#endif
#endif