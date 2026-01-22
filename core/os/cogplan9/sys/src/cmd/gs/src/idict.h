#ifndef idict_INCLUDED
# define idict_INCLUDED
#include "iddstack.h"
struct dict_s {
ref values;
ref keys;
ref count;
ref maxlength;
ref memory;
#define dict_memory(pdict) r_ptr(&(pdict)->memory, gs_ref_memory_t)
#define dict_mem(pdict) r_ptr(&(pdict)->memory, gs_memory_t)
};
extern const uint dict_max_size;
extern bool dict_auto_expand;
#ifndef gs_ref_memory_DEFINED
# define gs_ref_memory_DEFINED
typedef struct gs_ref_memory_s gs_ref_memory_t;
#endif
int dict_alloc(gs_ref_memory_t *, uint maxlength, ref * pdref);
#define dict_create(maxlen, pdref)\
dict_alloc(iimemory, maxlen, pdref)
#define dict_access_ref(pdref) (&(pdref)->value.pdict->values)
#define check_dict_read(dref) check_read(*dict_access_ref(&dref))
#define check_dict_write(dref) check_write(*dict_access_ref(&dref))
int dict_find(const ref * pdref, const ref * key, ref ** ppvalue);
int dict_find_string(const ref * pdref, const char *kstr, ref ** ppvalue);
int dict_put(ref * pdref, const ref * key, const ref * pvalue,
dict_stack_t *pds);
int dict_put_string(ref * pdref, const char *kstr, const ref * pvalue,
dict_stack_t *pds);
int dict_undef(ref * pdref, const ref * key, dict_stack_t *pds);
uint dict_length(const ref * pdref);
uint dict_maxlength(const ref * pdref);
uint dict_max_index(const ref * pdref);
int dict_copy_entries(const ref * dfrom, ref * dto, bool new_only,
dict_stack_t *pds);
#define dict_copy(dfrom, dto, pds) dict_copy_entries(dfrom, dto, false, pds)
#define dict_copy_new(dfrom, dto, pds) dict_copy_entries(dfrom, dto, true, pds)
int dict_resize(ref * pdref, uint newmaxlength, dict_stack_t *pds);
int dict_grow(ref * pdref, dict_stack_t *pds);
int dict_unpack(ref * pdref, dict_stack_t *pds);
int dict_first(const ref * pdref);
int dict_next(const ref * pdref, int index, ref * eltp);
int dict_value_index(const ref * pdref, const ref * pvalue);
int dict_index_entry(const ref * pdref, int index, ref * eltp);
#define dict_max_non_huge ((uint)(max_array_size / 2 + 1))
#define dict_name_index_hash(nidx) (nidx)
#define dict_hash_mod_rem(hash, size) ((hash) % (size))
#define dict_hash_mod_mask(hash, size) ((hash) & ((size) - 1))
#define dict_hash_mod_small(hash, size) dict_hash_mod_rem(hash, size)
#define dict_hash_mod_inline_small(hash, size) dict_hash_mod_rem(hash, size)
#define dict_hash_mod_large(hash, size)\
(size > dict_max_non_huge ? dict_hash_mod_rem(hash, size) :\
dict_hash_mod_mask(hash, size))
#define dict_hash_mod_inline_large(hash, size) dict_hash_mod_mask(hash, size)
uint dict_round_size_small(uint rsize);
uint dict_round_size_large(uint rsize);
#if arch_small_memory
# define dict_hash_mod(h, s) dict_hash_mod_small(h, s)
# define dict_hash_mod_inline(h, s) dict_hash_mod_inline_small(h, s)
# define dict_round_size(s) dict_round_size_small(s)
#else
# ifdef DEBUG
# define dict_hash_mod(h, s)\
(gs_debug_c('.') ? dict_hash_mod_small(h, s) :\
dict_hash_mod_large(h, s))
# define dict_hash_mod_inline(h, s)\
(gs_debug_c('.') ? dict_hash_mod_inline_small(h, s) :\
dict_hash_mod_inline_large(h, s))
# define dict_round_size(s)\
(gs_debug_c('.') ? dict_round_size_small(s) :\
dict_round_size_large(s))
# else
# define dict_hash_mod(h, s) dict_hash_mod_large(h, s)
# define dict_hash_mod_inline(h, s) dict_hash_mod_inline_large(h, s)
# define dict_round_size(s) dict_round_size_large(s)
# endif
#endif
#endif