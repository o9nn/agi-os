#include "memory_.h"
#include "ghost.h"
#include "gsexit.h"
#include "gsstruct.h"
#include "iname.h"
#include "iastate.h"
#include "idebug.h"
#include "igc.h"
#include "ipacked.h"
#include "store.h"
#if 0
#  define rputc(c) dputc(c)
#else
#  define rputc(c) DO_NOTHING
#endif
ptr_proc_reloc(igc_reloc_ref_ptr, ref_packed);
refs_proc_reloc(igc_reloc_refs);
private gc_proc_clear_reloc(refs_clear_reloc);
private gc_proc_set_reloc(refs_set_reloc);
private gc_proc_compact(refs_compact);
private const struct_shared_procs_t refs_shared_procs =
{refs_clear_reloc, refs_set_reloc, refs_compact};
private struct_proc_clear_marks(refs_clear_marks);
private struct_proc_reloc_ptrs(refs_do_reloc);
const gs_memory_struct_type_t st_refs =
{sizeof(ref), "refs", &refs_shared_procs, refs_clear_marks, 0, refs_do_reloc};
CLEAR_MARKS_PROC(ref_struct_clear_marks)
{
ref *pref = (ref *) vptr;
ref *end = (ref *) ((char *)vptr + size);
for (; pref < end; pref++)
r_clear_attrs(pref, l_mark);
}
ENUM_PTRS_BEGIN_PROC(ref_struct_enum_ptrs)
{
if (index >= size / sizeof(ref))
return 0;
pep->ptr = (const ref *)vptr + index;
return ptr_ref_type;
ENUM_PTRS_END_PROC
}
RELOC_PTRS_BEGIN(ref_struct_reloc_ptrs)
{
vm_spaces spaces = gcst->spaces;
const gs_memory_t *cmem = space_system->stable_memory;
ref *beg = vptr;
ref *end = (ref *) ((char *)vptr + size);
igc_reloc_refs((ref_packed *) beg, (ref_packed *) end, gcst);
ref_struct_clear_marks(cmem, vptr, size, pstype);
} RELOC_PTRS_END
void
ptr_ref_unmark(enum_ptr_t *pep, gc_state_t * ignored)
{
ref_packed *rpp = (ref_packed *)pep->ptr;
if (r_is_packed(rpp))
r_clear_pmark(rpp);
else
r_clear_attrs((ref *)rpp, l_mark);
}
private void
refs_clear_marks(const gs_memory_t *cmem,
void  *vptr, uint size,
const gs_memory_struct_type_t * pstype)
{
ref_packed *rp = (ref_packed *) vptr;
ref_packed *end = (ref_packed *) ((byte *) vptr + size);
for (;;) {
if (r_is_packed(rp)) {
#ifdef DEBUG
if (gs_debug_c('8')) {
dlprintf1("  [8]unmark packed 0x%lx ", (ulong) rp);
debug_print_ref(cmem, (const ref *)rp);
dputs("\n");
}
#endif
r_clear_pmark(rp);
rp++;
} else {
ref *const pref = (ref *)rp;
#ifdef DEBUG
if (gs_debug_c('8')) {
dlprintf1("  [8]unmark ref 0x%lx ", (ulong) rp);
debug_print_ref(cmem, pref);
dputs("\n");
}
#endif
r_clear_attrs(pref, l_mark);
rp += packed_per_ref;
if (rp >= (ref_packed *) end)
break;
}
}
}
bool
ptr_ref_mark(enum_ptr_t *pep, gc_state_t * ignored)
{
ref_packed *rpp = (void *)pep->ptr;
if (r_is_packed(rpp)) {
if (r_has_pmark(rpp))
return false;
r_set_pmark(rpp);
} else {
ref *const pref = (ref *)rpp;
if (r_has_attr(pref, l_mark))
return false;
r_set_attrs(pref, l_mark);
}
return true;
}
private void
refs_clear_reloc(obj_header_t *hdr, uint size)
{
ref_packed *rp = (ref_packed *) (hdr + 1);
ref_packed *end = (ref_packed *) ((byte *) rp + size);
while (rp < end) {
if (r_is_packed(rp))
rp++;
else {
ref *const pref = (ref *)rp;
if (!ref_type_uses_size_or_null(r_type(pref))) {
if_debug1('8', "  [8]clearing reloc at 0x%lx\n", (ulong) rp);
r_set_size(pref, 0);
}
rp += packed_per_ref;
}
}
}
private bool
refs_set_reloc(obj_header_t * hdr, uint reloc, uint size)
{
ref_packed *rp = (ref_packed *) (hdr + 1);
ref_packed *end = (ref_packed *) ((byte *) rp + size);
uint freed = 0;
while (rp < end) {
if (r_is_packed(rp)) {
#if align_packed_per_ref == 1
if (r_has_pmark(rp)) {
if_debug1('8',
"  [8]packed ref 0x%lx is marked\n",
(ulong) rp);
rp++;
} else {
#else
int i;
#define all_marked (align_packed_per_ref * lp_mark)
# if align_packed_per_ref == 2
#  if arch_sizeof_int == arch_sizeof_short * 2
#    undef all_marked
#    define all_marked ( (lp_mark << (sizeof(short) * 8)) + lp_mark )
#    define marked (*(int *)rp & all_marked)
#  else
#    define marked ((*rp & lp_mark) + (rp[1] & lp_mark))
#  endif
# else
#  if align_packed_per_ref == 4
#    define marked ((*rp & lp_mark) + (rp[1] & lp_mark) +\
(rp[2] & lp_mark) + (rp[3] & lp_mark))
#  else
int marked = *rp & lp_mark;
for (i = 1; i < align_packed_per_ref; i++)
marked += rp[i] & lp_mark;
#  endif
# endif
switch (marked) {
case all_marked:
if_debug2('8',
"  [8]packed refs 0x%lx..0x%lx are marked\n",
(ulong) rp,
(ulong) (rp + (align_packed_per_ref - 1)));
rp += align_packed_per_ref;
break;
default:
for (i = align_packed_per_ref; i--; rp++) {
r_set_pmark(rp);
if_debug1('8',
"  [8]packed ref 0x%lx is marked\n",
(ulong) rp);
}
break;
case 0:
#endif
if_debug2('8', "  [8]%d packed ref(s) at 0x%lx are unmarked\n",
align_packed_per_ref, (ulong) rp);
{
uint rel = reloc + freed;
*rp = pt_tag(pt_integer) +
min(rel, packed_max_value);
}
rp += align_packed_per_ref;
freed += sizeof(ref_packed) * align_packed_per_ref;
}
} else {
uint rel = reloc + freed;
ref *pref = (ref *) rp;
if (!r_has_attr(pref, l_mark)) {
if_debug1('8', "  [8]ref 0x%lx is unmarked\n",
(ulong) pref);
r_set_type(pref, t_mark);
r_set_size(pref, rel);
freed += sizeof(ref);
} else {
if_debug1('8', "  [8]ref 0x%lx is marked\n",
(ulong) pref);
if (!ref_type_uses_size_or_null(r_type(pref))) {
if_debug2('8', "  [8]storing reloc %u at 0x%lx\n",
rel, (ulong) pref);
r_set_size(pref, rel);
}
}
rp += packed_per_ref;
}
}
if_debug3('7', " [7]at end of refs 0x%lx, size = %u, freed = %u\n",
(ulong) (hdr + 1), size, freed);
if (freed == size)
return false;
#if arch_sizeof_int > arch_sizeof_short
if (freed <= max_ushort)
return true;
rp = (ref_packed *) (hdr + 1);
while (rp < end) {
if (r_is_packed(rp)) {
if (!r_has_pmark(rp))
*rp = pt_tag(pt_integer) | lp_mark;
++rp;
} else {
ref *pref = (ref *) rp;
if (!r_has_attr(pref, l_mark)) {
r_set_type_attrs(pref, t_mark, l_mark);
r_set_size(pref, reloc);
} else {
if (!ref_type_uses_size_or_null(r_type(pref)))
r_set_size(pref, reloc);
}
rp += packed_per_ref;
}
}
r_clear_attrs((ref *) rp - 1, l_mark);
#endif
return true;
}
private void
refs_do_reloc(void  *vptr, uint size,
const gs_memory_struct_type_t * pstype, gc_state_t * gcst)
{
igc_reloc_refs((ref_packed *) vptr,
(ref_packed *) ((char *)vptr + size),
gcst);
}
void
igc_reloc_refs(ref_packed * from, ref_packed * to, gc_state_t * gcst)
{
int min_trace = gcst->min_collect;
ref_packed *rp = from;
bool do_all = gcst->relocating_untraced;
vm_spaces spaces = gcst->spaces;
const gs_memory_t *cmem = space_system->stable_memory;
while (rp < to) {
ref *pref;
#ifdef DEBUG
const void *before = 0;
const void *after = 0;
# define DO_RELOC(var, stat)\
BEGIN before = (var); stat; after = (var); END
# define SET_RELOC(var, expr)\
BEGIN before = (var); after = (var) = (expr); END
#else
# define DO_RELOC(var, stat) stat
# define SET_RELOC(var, expr) var = expr
#endif
if (r_is_packed(rp)) {
rp++;
continue;
}
pref = (ref *) rp;
if_debug3('8', "  [8]relocating %s %d ref at 0x%lx",
(r_has_attr(pref, l_mark) ? "marked" : "unmarked"),
r_btype(pref), (ulong) pref);
if ((r_has_attr(pref, l_mark) || do_all) &&
r_space(pref) >= min_trace
) {
switch (r_type(pref)) {
case t_file:
DO_RELOC(pref->value.pfile, RELOC_VAR(pref->value.pfile));
break;
case t_device:
DO_RELOC(pref->value.pdevice,
RELOC_VAR(pref->value.pdevice));
break;
case t_fontID:
case t_struct:
case t_astruct:
DO_RELOC(pref->value.pstruct,
RELOC_VAR(pref->value.pstruct));
break;
case t_dictionary:
rputc('d');
SET_RELOC(pref->value.pdict,
(dict *)igc_reloc_ref_ptr((ref_packed *)pref->value.pdict, gcst));
break;
case t_array:
{
uint size = r_size(pref);
if (size != 0) {
if (size < max_size_st_refs / sizeof(ref)) {
rputc('a');
SET_RELOC(pref->value.refs,
(ref *) igc_reloc_ref_ptr(
(ref_packed *) pref->value.refs, gcst));
} else {
rputc('A');
--size;
SET_RELOC(pref->value.refs,
(ref *) igc_reloc_ref_ptr(
(ref_packed *) (pref->value.refs + size),
gcst) - size);
}
}
}
break;
case t_mixedarray:
if (r_size(pref) != 0) {
rputc('m');
SET_RELOC(pref->value.packed,
igc_reloc_ref_ptr(pref->value.packed, gcst));
}
break;
case t_shortarray:
{
uint size = r_size(pref);
if (size != 0) {
rputc('s');
--size;
SET_RELOC(pref->value.packed,
igc_reloc_ref_ptr(pref->value.packed + size,
gcst) - size);
}
}
break;
case t_name:
{
void *psub = name_ref_sub_table(cmem, pref);
void *rsub = RELOC_OBJ(psub);
SET_RELOC(pref->value.pname,
(name *)
((char *)rsub + ((char *)pref->value.pname -
(char *)psub)));
} break;
case t_string:
{
gs_string str;
str.data = pref->value.bytes;
str.size = r_size(pref);
DO_RELOC(str.data, RELOC_STRING_VAR(str));
pref->value.bytes = str.data;
}
break;
case t_oparray:
rputc('o');
SET_RELOC(pref->value.const_refs,
(const ref *)igc_reloc_ref_ptr((const ref_packed *)pref->value.const_refs, gcst));
break;
default:
goto no_reloc;
}
if_debug2('8', ", 0x%lx => 0x%lx", (ulong)before, (ulong)after);
}
no_reloc:
if_debug0('8', "\n");
rp += packed_per_ref;
}
}
ref_packed *
igc_reloc_ref_ptr(const ref_packed * prp, gc_state_t *gcst)
{
const ref_packed *rp = prp;
uint dec = 0;
#ifdef ALIGNMENT_ALIASING_BUG
const ref *rpref;
# define RP_REF(rp) (rpref = (const ref *)rp, rpref)
#else
# define RP_REF(rp) ((const ref *)rp)
#endif
if (r_is_packed(rp)) {
if (!r_has_pmark(rp))
goto ret_rp;
} else {
if (!r_has_attr(RP_REF(rp), l_mark))
goto ret_rp;
}
for (;;) {
if (r_is_packed(rp)) {
rputc((*rp & lp_mark ? '1' : '0'));
if (!(*rp & lp_mark)) {
if (*rp != pt_tag(pt_integer) + packed_max_value) {
rputc('\n');
rp = print_reloc(prp, "ref",
(const ref_packed *)
((const char *)prp -
(*rp & packed_value_mask) + dec));
break;
}
dec += sizeof(ref_packed) * align_packed_per_ref;
rp += align_packed_per_ref;
} else
rp++;
continue;
}
if (!ref_type_uses_size_or_null(r_type(RP_REF(rp)))) {
rputc('\n');
rp = print_reloc(prp, "ref",
(const ref_packed *)
(r_size(RP_REF(rp)) == 0 ? prp :
(const ref_packed *)((const char *)prp -
r_size(RP_REF(rp)) + dec)));
break;
}
rputc('u');
rp += packed_per_ref;
}
ret_rp:
{
union { const ref_packed *r; ref_packed *w; } u;
u.r = rp;
return u.w;
}
}
private void
refs_compact(const gs_memory_t *mem, obj_header_t * pre, obj_header_t * dpre, uint size)
{
ref_packed *dest;
ref_packed *src;
ref_packed *end;
uint new_size;
src = (ref_packed *) (pre + 1);
end = (ref_packed *) ((byte *) src + size);
if (dpre == pre)
for (;;) {
if (r_is_packed(src)) {
if (!r_has_pmark(src))
break;
if_debug1('8', "  [8]packed ref 0x%lx \"copied\"\n",
(ulong) src);
*src &= ~lp_mark;
src++;
} else {
ref *const pref = (ref *)src;
if (!r_has_attr(pref, l_mark))
break;
if_debug1('8', "  [8]ref 0x%lx \"copied\"\n", (ulong) src);
r_clear_attrs(pref, l_mark);
src += packed_per_ref;
}
} else
*dpre = *pre;
dest = (ref_packed *) ((char *)dpre + ((char *)src - (char *)pre));
for (;;) {
if (r_is_packed(src)) {
if (r_has_pmark(src)) {
if_debug2('8', "  [8]packed ref 0x%lx copied to 0x%lx\n",
(ulong) src, (ulong) dest);
*dest++ = *src & ~lp_mark;
}
src++;
} else {
if (r_has_attr((ref *) src, l_mark)) {
ref rtemp;
if_debug2('8', "  [8]ref 0x%lx copied to 0x%lx\n",
(ulong) src, (ulong) dest);
ref_assign_inline(&rtemp, (ref *) src);
r_clear_attrs(&rtemp, l_mark);
ref_assign_inline((ref *) dest, &rtemp);
dest += packed_per_ref;
src += packed_per_ref;
} else {
src += packed_per_ref;
if (src >= end)
break;
}
}
}
new_size = (byte *) dest - (byte *) (dpre + 1) + sizeof(ref);
#ifdef DEBUG
if ((byte *) src - (byte *) dest != r_size((ref *) src - 1) + sizeof(ref)) {
lprintf3("Reloc error for refs 0x%lx: reloc = %lu, stored = %u\n",
(ulong) dpre, (ulong) ((byte *) src - (byte *) dest),
(uint) r_size((ref *) src - 1));
gs_abort(mem);
}
#endif
while (new_size & (sizeof(ref) - 1))
*dest++ = pt_tag(pt_integer),
new_size += sizeof(ref_packed);
if (size - new_size < sizeof(obj_header_t)) {
while (new_size < size)
*dest++ = pt_tag(pt_integer),
new_size += sizeof(ref_packed);
} else {
obj_header_t *pfree = (obj_header_t *) ((ref *) dest + 1);
pfree->o_alone = 0;
pfree->o_size = size - new_size - sizeof(obj_header_t);
pfree->o_type = &st_bytes;
}
r_set_type((ref *) dest, t_integer);
dpre->o_size = new_size;
}