#ifndef idstack_INCLUDED
# define idstack_INCLUDED
#include "iddstack.h"
#include "idsdata.h"
#include "istack.h"
typedef s_ptr ds_ptr;
typedef const_s_ptr const_ds_ptr;
void dstack_gc_cleanup(dict_stack_t *);
ref *dstack_find_name_by_index(dict_stack_t *, uint);
#define dstack_find_name_by_index_inline(pds,nidx,htemp)\
((pds)->top_keys[htemp = dict_hash_mod_inline(dict_name_index_hash(nidx),\
(pds)->top_npairs) + 1] == pt_tag(pt_literal_name) + (nidx) ?\
(pds)->top_values + htemp : dstack_find_name_by_index(pds, nidx))
#define if_dstack_find_name_by_index_top(pds,nidx,htemp,pvslot)\
if ( (((pds)->top_keys[htemp = dict_hash_mod_inline(dict_name_index_hash(nidx),\
(pds)->top_npairs) + 1] == pt_tag(pt_literal_name) + (nidx)) ?\
((pvslot) = (pds)->top_values + (htemp), 1) :\
0)\
)
#endif