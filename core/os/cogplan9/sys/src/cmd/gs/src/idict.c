#include "math_.h"
#include "string_.h"
#include "ghost.h"
#include "gxalloc.h"
#include "ierrors.h"
#include "imemory.h"
#include "idebug.h"
#include "inamedef.h"
#include "iname.h"
#include "ipacked.h"
#include "isave.h"
#include "store.h"
#include "idict.h"
#include "idictdef.h"
#include "iutil.h"
#include "ivmspace.h"
#include "iddstack.h"
const uint dict_max_size = max_array_size - 1;
bool dict_default_pack = true;
#define CAN_SET_PVALUE_CACHE(pds, pdref, mem)\
(pds && dstack_dict_is_permanent(pds, pdref) && !ref_saving_in(mem))
private int dict_create_contents(uint size, const ref * pdref, bool pack);
#ifdef DEBUG
struct stats_dict_s {
long lookups;
long probe1;
long probe2;
} stats_dict;
int real_dict_find(const ref * pdref, const ref * key, ref ** ppvalue);
int
dict_find(const ref * pdref, const ref * pkey, ref ** ppvalue)
{
dict *pdict = pdref->value.pdict;
int code = real_dict_find(pdref, pkey, ppvalue);
stats_dict.lookups++;
if (r_has_type(pkey, t_name) && dict_is_packed(pdict)) {
uint nidx = name_index(dict_mem(pdict), pkey);
uint hash =
dict_hash_mod(dict_name_index_hash(nidx), npairs(pdict)) + 1;
if (pdict->keys.value.packed[hash] ==
pt_tag(pt_literal_name) + nidx
)
stats_dict.probe1++;
else if (pdict->keys.value.packed[hash - 1] ==
pt_tag(pt_literal_name) + nidx
)
stats_dict.probe2++;
}
if (gs_debug_c('d') && !(stats_dict.lookups % 1000))
dlprintf3("[d]lookups=%ld probe1=%ld probe2=%ld\n",
stats_dict.lookups, stats_dict.probe1, stats_dict.probe2);
return code;
}
#define dict_find real_dict_find
#endif
uint
dict_round_size_small(uint rsize)
{
return (rsize > dict_max_size ? 0 : rsize);
}
uint
dict_round_size_large(uint rsize)
{
if (rsize > dict_max_non_huge)
return (rsize > dict_max_size ? 0 : rsize);
while (rsize & (rsize - 1))
rsize = (rsize | (rsize - 1)) + 1;
return (rsize <= dict_max_size ? rsize : dict_max_non_huge);
}
int
dict_alloc(gs_ref_memory_t * mem, uint size, ref * pdref)
{
ref arr;
int code =
gs_alloc_ref_array(mem, &arr, a_all, sizeof(dict) / sizeof(ref),
"dict_alloc");
dict *pdict;
ref dref;
if (code < 0)
return code;
pdict = (dict *) arr.value.refs;
make_tav(&dref, t_dictionary,
r_space(&arr) | imemory_new_mask(mem) | a_all,
pdict, pdict);
make_struct(&pdict->memory, avm_foreign, mem);
code = dict_create_contents(size, &dref, dict_default_pack);
if (code < 0) {
gs_free_ref_array(mem, &arr, "dict_alloc");
return code;
}
*pdref = dref;
return 0;
}
private int
dict_create_unpacked_keys(uint asize, const ref * pdref)
{
dict *pdict = pdref->value.pdict;
gs_ref_memory_t *mem = dict_memory(pdict);
int code;
code = gs_alloc_ref_array(mem, &pdict->keys, a_all, asize,
"dict_create_unpacked_keys");
if (code >= 0) {
uint new_mask = imemory_new_mask(mem);
ref *kp = pdict->keys.value.refs;
r_set_attrs(&pdict->keys, new_mask);
refset_null_new(kp, asize, new_mask);
r_set_attrs(kp, a_executable);
}
return code;
}
private int
dict_create_contents(uint size, const ref * pdref, bool pack)
{
dict *pdict = pdref->value.pdict;
gs_ref_memory_t *mem = dict_memory(pdict);
uint new_mask = imemory_new_mask(mem);
uint asize = dict_round_size((size == 0 ? 1 : size));
int code;
register uint i;
if (asize == 0 || asize > max_array_size - 1)
return_error(e_limitcheck);
asize++;
code = gs_alloc_ref_array(mem, &pdict->values, a_all, asize,
"dict_create_contents(values)");
if (code < 0)
return code;
r_set_attrs(&pdict->values, new_mask);
refset_null_new(pdict->values.value.refs, asize, new_mask);
if (pack) {
uint ksize = (asize + packed_per_ref - 1) / packed_per_ref;
ref arr;
ref_packed *pkp;
ref_packed *pzp;
code = gs_alloc_ref_array(mem, &arr, a_all, ksize,
"dict_create_contents(packed keys)");
if (code < 0)
return code;
pkp = (ref_packed *) arr.value.refs;
make_tasv(&pdict->keys, t_shortarray,
r_space(&arr) | a_all | new_mask,
asize, packed, pkp);
for (pzp = pkp, i = 0; i < asize || i % packed_per_ref; pzp++, i++)
*pzp = packed_key_empty;
*pkp = packed_key_deleted;
} else {
int code = dict_create_unpacked_keys(asize, pdref);
if (code < 0)
return code;
}
make_tav(&pdict->count, t_integer, new_mask, intval, 0);
make_tav(&pdict->maxlength, t_integer, new_mask, intval, size);
return 0;
}
int
dict_unpack(ref * pdref, dict_stack_t *pds)
{
dict *pdict = pdref->value.pdict;
if (!dict_is_packed(pdict))
return 0;
{
gs_ref_memory_t *mem = dict_memory(pdict);
uint count = nslots(pdict);
const ref_packed *okp = pdict->keys.value.packed;
ref old_keys;
int code;
ref *nkp;
old_keys = pdict->keys;
if (ref_must_save_in(mem, &old_keys))
ref_do_save_in(mem, pdref, &pdict->keys, "dict_unpack(keys)");
code = dict_create_unpacked_keys(count, pdref);
if (code < 0)
return code;
for (nkp = pdict->keys.value.refs; count--; okp++, nkp++)
if (r_packed_is_name(okp)) {
packed_get((const gs_memory_t *)mem, okp, nkp);
ref_mark_new_in(mem, nkp);
} else if (*okp == packed_key_deleted)
r_set_attrs(nkp, a_executable);
if (!ref_must_save_in(mem, &old_keys))
gs_free_ref_array(mem, &old_keys, "dict_unpack(old keys)");
if (pds)
dstack_set_top(pds);
}
return 0;
}
int
dict_find(const ref * pdref, const ref * pkey,
ref ** ppvalue )
{
dict *pdict = pdref->value.pdict;
uint size = npairs(pdict);
register int etype;
uint nidx;
ref_packed kpack;
uint hash;
int ktype;
const gs_memory_t *mem = dict_mem(pdict);
switch (r_type(pkey)) {
case t_name:
nidx = name_index(mem, pkey);
nh:
hash = dict_name_index_hash(nidx);
kpack = packed_name_key(nidx);
ktype = t_name;
break;
case t_string:
{
ref nref;
int code;
if (!r_has_attr(pkey, a_read))
return_error(e_invalidaccess);
code = name_ref(mem, pkey->value.bytes, r_size(pkey), &nref, 1);
if (code < 0)
return code;
nidx = name_index(mem, &nref);
}
goto nh;
case t_real:
{
int expt, i;
double mant = frexp(pkey->value.realval, &expt);
if (expt < sizeof(long) * 8 || pkey->value.realval == min_long)
i = (int)pkey->value.realval;
else
i = (int)(mant * min_long);
hash = (uint)i * 30503;
}
goto ih;
case t_integer:
hash = (uint)pkey->value.intval * 30503;
ih:
kpack = packed_key_impossible;
ktype = -1;
nidx = 0;
break;
case t_null:
return_error(e_typecheck);
default:
hash = r_btype(pkey) * 99;
kpack = packed_key_impossible;
ktype = -1;
nidx = 0;
}
if (dict_is_packed(pdict)) {
const ref_packed *pslot = 0;
packed_search_1(*ppvalue = packed_search_value_pointer,
return 1,
if (pslot == 0) pslot = kp, goto miss);
packed_search_2(*ppvalue = packed_search_value_pointer,
return 1,
if (pslot == 0) pslot = kp, goto miss);
if (pslot == 0 || d_length(pdict) == d_maxlength(pdict))
return_error(e_dictfull);
*ppvalue = pdict->values.value.refs + (pslot - kbot);
return 0;
miss:
if (d_length(pdict) == d_maxlength(pdict))
return_error(e_dictfull);
if (pslot == 0)
pslot = kp;
*ppvalue = pdict->values.value.refs + (pslot - kbot);
return 0;
} else {
ref *kbot = pdict->keys.value.refs;
register ref *kp;
ref *pslot = 0;
int wrap = 0;
for (kp = kbot + dict_hash_mod(hash, size) + 2;;) {
--kp;
if ((etype = r_type(kp)) == ktype) {
if (name_index(mem, kp) == nidx) {
*ppvalue = pdict->values.value.refs + (kp - kbot);
return 1;
}
} else if (etype == t_null) {
if (kp == kbot) {
if (wrap++) {
if (pslot == 0)
return_error(e_dictfull);
break;
}
kp += size + 1;
} else if (r_has_attr(kp, a_executable)) {
if (pslot == 0)
pslot = kp;
} else
break;
} else {
if (obj_eq(mem, kp, pkey)) {
*ppvalue = pdict->values.value.refs + (kp - kbot);
return 1;
}
}
}
if (d_length(pdict) == d_maxlength(pdict))
return_error(e_dictfull);
*ppvalue = pdict->values.value.refs +
((pslot != 0 ? pslot : kp) - kbot);
return 0;
}
}
int
dict_find_string(const ref * pdref, const char *kstr, ref ** ppvalue)
{
int code;
ref kname;
if ( pdref != 0 ) {
dict *pdict = pdref->value.pdict;
if ((code = name_ref(dict_mem(pdict),
(const byte *)kstr, strlen(kstr), &kname, -1)) < 0)
return code;
return dict_find(pdref, &kname, ppvalue);
}
return 0;
}
int
dict_put(ref * pdref , const ref * pkey, const ref * pvalue,
dict_stack_t *pds)
{
dict *pdict = pdref->value.pdict;
gs_ref_memory_t *mem = dict_memory(pdict);
gs_memory_t *pmem = dict_mem(pdict);
int rcode = 0;
int code;
ref *pvslot;
store_check_dest(pdref, pvalue);
top:if ((code = dict_find(pdref, pkey, &pvslot)) <= 0) {
ref kname;
uint index;
switch (code) {
case 0:
break;
case e_dictfull:
if (!pmem->gs_lib_ctx->dict_auto_expand)
return_error(e_dictfull);
code = dict_grow(pdref, pds);
if (code < 0)
return code;
goto top;
default:
return code;
}
index = pvslot - pdict->values.value.refs;
if (r_has_type(pkey, t_string)) {
int code;
if (!r_has_attr(pkey, a_read))
return_error(e_invalidaccess);
code = name_from_string(pmem, pkey, &kname);
if (code < 0)
return code;
pkey = &kname;
}
if (dict_is_packed(pdict)) {
ref_packed *kp;
if (!r_has_type(pkey, t_name) ||
name_index(pmem, pkey) > packed_name_max_index
) {
int code = dict_unpack(pdref, pds);
if (code < 0)
return code;
goto top;
}
kp = pdict->keys.value.writable_packed + index;
if (ref_must_save_in(mem, &pdict->keys)) {
ref_do_save_in(mem, &pdict->keys, kp, "dict_put(key)");
}
*kp = pt_tag(pt_literal_name) + name_index(pmem, pkey);
} else {
ref *kp = pdict->keys.value.refs + index;
if_debug2('d', "[d]0x%lx: fill key at 0x%lx\n",
(ulong) pdict, (ulong) kp);
store_check_dest(pdref, pkey);
ref_assign_old_in(mem, &pdict->keys, kp, pkey,
"dict_put(key)");
}
ref_save_in(mem, pdref, &pdict->count, "dict_put(count)");
pdict->count.value.intval++;
if (r_has_type(pkey, t_name)) {
name *pname = pkey->value.pname;
if (pname->pvalue == pv_no_defn &&
CAN_SET_PVALUE_CACHE(pds, pdref, mem)
) {
if_debug0('d', "[d]set cache\n");
pname->pvalue = pvslot;
} else {
if_debug0('d', "[d]no cache\n");
pname->pvalue = pv_other;
}
}
rcode = 1;
}
if_debug8('d', "[d]0x%lx: put key 0x%lx 0x%lx\n  value at 0x%lx: old 0x%lx 0x%lx, new 0x%lx 0x%lx\n",
(ulong) pdref->value.pdict,
((const ulong *)pkey)[0], ((const ulong *)pkey)[1],
(ulong) pvslot,
((const ulong *)pvslot)[0], ((const ulong *)pvslot)[1],
((const ulong *)pvalue)[0], ((const ulong *)pvalue)[1]);
ref_assign_old_in(mem, &pdref->value.pdict->values, pvslot, pvalue,
"dict_put(value)");
return rcode;
}
int
dict_put_string(ref * pdref, const char *kstr, const ref * pvalue,
dict_stack_t *pds)
{
int code;
ref kname;
dict *pdict = pdref->value.pdict;
if ((code = name_ref(dict_mem(pdict),
(const byte *)kstr, strlen(kstr), &kname, 0)) < 0)
return code;
return dict_put(pdref, &kname, pvalue, pds);
}
int
dict_undef(ref * pdref, const ref * pkey, dict_stack_t *pds)
{
gs_ref_memory_t *mem;
ref *pvslot;
dict *pdict;
uint index;
if (dict_find(pdref, pkey, &pvslot) <= 0)
return (e_undefined);
pdict = pdref->value.pdict;
index = pvslot - pdict->values.value.refs;
mem = dict_memory(pdict);
if (dict_is_packed(pdict)) {
ref_packed *pkp = pdict->keys.value.writable_packed + index;
if_debug3('d', "[d]0x%lx: removing key at 0%lx: 0x%x\n",
(ulong)pdict, (ulong)pkp, (uint)*pkp);
if (ref_must_save_in(mem, &pdict->keys))
ref_do_save_in(mem, &pdict->keys, pkp, "dict_undef(key)");
if (pkp[-1] == packed_key_empty) {
uint end = nslots(pdict);
*pkp = packed_key_empty;
while (++index < end && *++pkp == packed_key_deleted)
*pkp = packed_key_empty;
} else
*pkp = packed_key_deleted;
} else {
ref *kp = pdict->keys.value.refs + index;
if_debug4('d', "[d]0x%lx: removing key at 0%lx: 0x%lx 0x%lx\n",
(ulong)pdict, (ulong)kp, ((ulong *)kp)[0], ((ulong *)kp)[1]);
make_null_old_in(mem, &pdict->keys, kp, "dict_undef(key)");
if (!r_has_type(kp - 1, t_null) ||
r_has_attr(kp - 1, a_executable)
)
r_set_attrs(kp, a_executable);
}
ref_save_in(mem, pdref, &pdict->count, "dict_undef(count)");
pdict->count.value.intval--;
if (r_has_type(pkey, t_name)) {
name *pname = pkey->value.pname;
if (pv_valid(pname->pvalue)) {
#ifdef DEBUG
if (!(pds && dstack_dict_is_permanent(pds, pdref)))
lprintf1("dict_undef: cached name value pointer 0x%lx is incorrect!\n",
(ulong) pname->pvalue);
#endif
pname->pvalue = pv_no_defn;
}
}
make_null_old_in(mem, &pdict->values, pvslot, "dict_undef(value)");
return 0;
}
uint
dict_length(const ref * pdref )
{
return d_length(pdref->value.pdict);
}
uint
dict_maxlength(const ref * pdref )
{
return d_maxlength(pdref->value.pdict);
}
uint
dict_max_index(const ref * pdref )
{
return npairs(pdref->value.pdict) - 1;
}
#define COPY_NEW_ONLY 1
#define COPY_FOR_RESIZE 2
private int
dict_copy_elements(const ref * pdrfrom ,
ref * pdrto , int options,
dict_stack_t *pds)
{
int space = r_space(pdrto);
int index;
ref elt[2];
ref *pvslot;
int code;
if (space != avm_max) {
index = dict_first(pdrfrom);
while ((index = dict_next(pdrfrom, index, elt)) >= 0)
if (!(options & COPY_NEW_ONLY) ||
dict_find(pdrto, &elt[0], &pvslot) <= 0
) {
store_check_space(space, &elt[0]);
store_check_space(space, &elt[1]);
}
}
index = dict_first(pdrfrom);
while ((index = dict_next(pdrfrom, index, elt)) >= 0) {
ref *pvalue = pv_no_defn;
if ((options & COPY_NEW_ONLY) &&
dict_find(pdrto, &elt[0], &pvslot) > 0
)
continue;
if ((options & COPY_FOR_RESIZE) &&
r_has_type(&elt[0], t_name) &&
(pvalue = elt[0].value.pname->pvalue, pv_valid(pvalue))
)
elt[0].value.pname->pvalue = pv_no_defn;
if ((code = dict_put(pdrto, &elt[0], &elt[1], pds)) < 0) {
if (pvalue != pv_no_defn)
elt[0].value.pname->pvalue = pvalue;
return code;
}
}
return 0;
}
int
dict_copy_entries(const ref *pdrfrom, ref *pdrto, bool new_only,
dict_stack_t *pds)
{
return dict_copy_elements(pdrfrom, pdrto, (new_only ? COPY_NEW_ONLY : 0),
pds);
}
int
dict_resize(ref * pdref, uint new_size, dict_stack_t *pds)
{
dict *pdict = pdref->value.pdict;
gs_ref_memory_t *mem = dict_memory(pdict);
uint new_mask = imemory_new_mask(mem);
dict dnew;
ref drto;
int code;
if (new_size < d_length(pdict)) {
if (!mem->gs_lib_ctx->dict_auto_expand)
return_error(e_dictfull);
new_size = d_length(pdict);
}
make_tav(&drto, t_dictionary, r_space(pdref) | a_all | new_mask,
pdict, &dnew);
dnew.memory = pdict->memory;
if ((code = dict_create_contents(new_size, &drto, dict_is_packed(pdict))) < 0)
return code;
r_set_space(&drto, avm_local);
if (CAN_SET_PVALUE_CACHE(pds, pdref, mem)) {
ref drfrom;
drfrom = *pdref;
*pdref = drto;
dict_copy_elements(&drfrom, pdref, COPY_FOR_RESIZE, pds);
*pdref = drfrom;
} else {
dict_copy_elements(pdref, &drto, 0, pds);
}
if (ref_must_save_in(mem, &pdict->values))
ref_do_save_in(mem, pdref, &pdict->values, "dict_resize(values)");
else
gs_free_ref_array(mem, &pdict->values, "dict_resize(old values)");
if (ref_must_save_in(mem, &pdict->keys))
ref_do_save_in(mem, pdref, &pdict->keys, "dict_resize(keys)");
else
gs_free_ref_array(mem, &pdict->keys, "dict_resize(old keys)");
ref_assign(&pdict->keys, &dnew.keys);
ref_assign(&pdict->values, &dnew.values);
ref_save_in(dict_memory(pdict), pdref, &pdict->maxlength,
"dict_resize(maxlength)");
d_set_maxlength(pdict, new_size);
if (pds)
dstack_set_top(pds);
return 0;
}
int
dict_grow(ref * pdref, dict_stack_t *pds)
{
dict *pdict = pdref->value.pdict;
ulong new_size = (ulong) d_maxlength(pdict) * 3 / 2 + 2;
#if arch_sizeof_int < arch_sizeof_long
if (new_size > max_uint)
new_size = max_uint;
#endif
if (new_size > npairs(pdict)) {
int code = dict_resize(pdref, (uint) new_size, pds);
if (code >= 0)
return code;
if (npairs(pdict) < dict_max_size) {
code = dict_resize(pdref, dict_max_size, pds);
if (code >= 0)
return code;
}
if (npairs(pdict) == d_maxlength(pdict)) {
return code;
}
new_size = npairs(pdict);
}
ref_save_in(dict_memory(pdict), pdref, &pdict->maxlength,
"dict_put(maxlength)");
d_set_maxlength(pdict, new_size);
return 0;
}
int
dict_first(const ref * pdref)
{
return (int)nslots(pdref->value.pdict);
}
int
dict_next(const ref * pdref, int index, ref * eltp )
{
dict *pdict = pdref->value.pdict;
ref *vp = pdict->values.value.refs + index;
while (vp--, --index >= 0) {
array_get(dict_mem(pdict), &pdict->keys, (long)index, eltp);
if (r_has_type(eltp, t_name) ||
(!dict_is_packed(pdict) && !r_has_type(eltp, t_null))
) {
eltp[1] = *vp;
if_debug6('d', "[d]0x%lx: index %d: %lx %lx, %lx %lx\n",
(ulong) pdict, index,
((ulong *) eltp)[0], ((ulong *) eltp)[1],
((ulong *) vp)[0], ((ulong *) vp)[1]);
return index;
}
}
return -1;
}
int
dict_value_index(const ref * pdref, const ref * pvalue)
{
return (int)(pvalue - pdref->value.pdict->values.value.refs - 1);
}
int
dict_index_entry(const ref * pdref, int index, ref * eltp )
{
const dict *pdict = pdref->value.pdict;
array_get(dict_mem(pdict), &pdict->keys, (long)(index + 1), eltp);
if (r_has_type(eltp, t_name) ||
(!dict_is_packed(pdict) && !r_has_type(eltp, t_null))
) {
eltp[1] = pdict->values.value.refs[index + 1];
return 0;
}
return e_undefined;
}