#include "ghost.h"
#include "idict.h"
#include "idictdef.h"
#include "idstack.h"
#include "inamedef.h"
#include "iname.h"
#include "ipacked.h"
#include "iutil.h"
#include "ivmspace.h"
#ifdef DEBUG
#include "idebug.h"
#define MAX_STATS_DEPTH 6
struct stats_dstack_s {
long lookups;
long probes[2];
long depth[MAX_STATS_DEPTH + 1];
} stats_dstack;
# define INCR(v) (++stats_dstack.v)
#else
# define INCR(v) DO_NOTHING
#endif
#ifdef DEBUG
ref *real_dstack_find_name_by_index(dict_stack_t * pds, uint nidx);
ref *
dstack_find_name_by_index(dict_stack_t * pds, uint nidx)
{
ref *pvalue = real_dstack_find_name_by_index(pds, nidx);
dict *pdict = pds->stack.p->value.pdict;
INCR(lookups);
if (dict_is_packed(pdict)) {
uint hash =
dict_hash_mod(dict_name_index_hash(nidx), npairs(pdict)) + 1;
if (pdict->keys.value.packed[hash] ==
pt_tag(pt_literal_name) + nidx
)
INCR(probes[0]);
else if (pdict->keys.value.packed[hash - 1] ==
pt_tag(pt_literal_name) + nidx
)
INCR(probes[1]);
}
if (gs_debug_c('d') && !(stats_dstack.lookups % 1000))
dlprintf3("[d]lookups=%ld probe1=%ld probe2=%ld\n",
stats_dstack.lookups, stats_dstack.probes[0],
stats_dstack.probes[1]);
return pvalue;
}
#define dstack_find_name_by_index real_dstack_find_name_by_index
#endif
bool
dstack_dict_is_permanent(const dict_stack_t * pds, const ref * pdref)
{
dict *pdict = pdref->value.pdict;
int i;
if (pds->stack.extension_size == 0) {
for (i = 0; i < pds->min_size; ++i)
if (pds->stack.bot[i].value.pdict == pdict)
return true;
} else {
uint count = ref_stack_count(&pds->stack);
for (i = count - pds->min_size; i < count; ++i)
if (ref_stack_index(&pds->stack, i)->value.pdict == pdict)
return true;
}
return false;
}
ref *
dstack_find_name_by_index(dict_stack_t * pds, uint nidx)
{
ds_ptr pdref = pds->stack.p;
#define hash dict_name_index_hash(nidx)
ref_packed kpack = packed_name_key(nidx);
do {
dict *pdict = pdref->value.pdict;
uint size = npairs(pdict);
const gs_memory_t *mem = dict_mem(pdict);
#ifdef DEBUG
if (gs_debug_c('D')) {
ref dnref;
name_index_ref(mem, nidx, &dnref);
dlputs("[D]lookup ");
debug_print_name(mem, &dnref);
dprintf3(" in 0x%lx(%u/%u)\n",
(ulong) pdict, dict_length(pdref),
dict_maxlength(pdref));
}
#endif
#define INCR_DEPTH(pdref)\
INCR(depth[min(MAX_STATS_DEPTH, pds->stack.p - pdref)])
if (dict_is_packed(pdict)) {
packed_search_1(INCR_DEPTH(pdref),
return packed_search_value_pointer,
DO_NOTHING, goto miss);
packed_search_2(INCR_DEPTH(pdref),
return packed_search_value_pointer,
DO_NOTHING, break);
miss:;
} else {
ref *kbot = pdict->keys.value.refs;
register ref *kp;
int wrap = 0;
for (kp = kbot + dict_hash_mod(hash, size) + 2;;) {
--kp;
if (r_has_type(kp, t_name)) {
if (name_index(mem, kp) == nidx) {
INCR_DEPTH(pdref);
return pdict->values.value.refs + (kp - kbot);
}
} else if (r_has_type(kp, t_null)) {
if (!r_has_attr(kp, a_executable))
break;
if (kp == kbot) {
if (wrap++)
break;
kp += size + 1;
}
}
}
}
#undef INCR_DEPTH
}
while (pdref-- > pds->stack.bot);
if (!pds->stack.extension_size)
return (ref *) 0;
{
ref key;
uint i = pds->stack.p + 1 - pds->stack.bot;
uint size = ref_stack_count(&pds->stack);
ref *pvalue;
dict *pdict = pds->stack.p->value.pdict;
const gs_memory_t *mem = dict_mem(pdict);
name_index_ref(mem, nidx, &key);
for (; i < size; i++) {
if (dict_find(ref_stack_index(&pds->stack, i),
&key, &pvalue) > 0
) {
INCR(depth[min(MAX_STATS_DEPTH, i)]);
return pvalue;
}
}
}
return (ref *) 0;
#undef hash
}
private const ref_packed no_packed_keys[2] =
{packed_key_deleted, packed_key_empty};
void
dstack_set_top(dict_stack_t * pds)
{
ds_ptr dsp = pds->stack.p;
dict *pdict = dsp->value.pdict;
if_debug3('d', "[d]dsp = 0x%lx -> 0x%lx, key array type = %d\n",
(ulong) dsp, (ulong) pdict, r_type(&pdict->keys));
if (dict_is_packed(pdict) &&
r_has_attr(dict_access_ref(dsp), a_read)
) {
pds->top_keys = pdict->keys.value.packed;
pds->top_npairs = npairs(pdict);
pds->top_values = pdict->values.value.refs;
} else {
pds->top_keys = no_packed_keys;
pds->top_npairs = 1;
}
if (!r_has_attr(dict_access_ref(dsp), a_write))
pds->def_space = -1;
else
pds->def_space = r_space(dsp);
}
void
dstack_gc_cleanup(dict_stack_t * pds)
{
uint count = ref_stack_count(&pds->stack);
uint dsi;
for (dsi = pds->min_size; dsi > 0; --dsi) {
const dict *pdict =
ref_stack_index(&pds->stack, count - dsi)->value.pdict;
uint size = nslots(pdict);
ref *pvalue = pdict->values.value.refs;
uint i;
for (i = 0; i < size; ++i, ++pvalue) {
ref key;
ref *old_pvalue;
array_get(dict_mem(pdict), &pdict->keys, (long)i, &key);
if (r_has_type(&key, t_name) &&
pv_valid(old_pvalue = key.value.pname->pvalue)
) {
if (old_pvalue == pvalue) {
if_debug1('d', "[d]skipping dstack entry %d\n",
dsi - 1);
break;
}
key.value.pname->pvalue = pvalue;
}
}
}
}