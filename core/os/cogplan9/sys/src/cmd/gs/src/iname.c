#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "gsstruct.h"
#include "gxobj.h"
#include "ierrors.h"
#include "inamedef.h"
#include "imemory.h"
#include "isave.h"
#include "store.h"
const uint name_max_string = max_name_string;
private const byte hash_permutation[256] = {
NAME_HASH_PERMUTATION_DATA
};
private const byte nt_1char_names[NT_1CHAR_SIZE] = {
NT_1CHAR_NAMES_DATA
};
gs_private_st_simple(st_name_sub_table, name_sub_table, "name_sub_table");
gs_private_st_composite(st_name_string_sub_table, name_string_sub_table_t,
"name_string_sub_table_t",
name_string_sub_enum_ptrs, name_string_sub_reloc_ptrs);
gs_private_st_composite(st_name_table, name_table, "name_table",
name_table_enum_ptrs, name_table_reloc_ptrs);
private int name_alloc_sub(name_table *);
private void name_free_sub(name_table *, uint);
private void name_scan_sub(name_table *, uint, bool);
#ifdef DEBUG
private void
name_print(const char *msg, const name_table *nt, uint nidx, const int *pflag)
{
const name_string_t *pnstr = names_index_string_inline(nt, nidx);
const name *pname = names_index_ptr_inline(nt, nidx);
const byte *str = pnstr->string_bytes;
dlprintf1("[n]%s", msg);
if (pflag)
dprintf1("(%d)", *pflag);
dprintf2(" (0x%lx#%u)", (ulong)pname, nidx);
debug_print_string(str, pnstr->string_size);
dprintf2("(0x%lx,%u)\n", (ulong)str, pnstr->string_size);
}
# define if_debug_name(msg, nt, nidx, pflag)\
if ( gs_debug_c('n') ) name_print(msg, nt, nidx, pflag)
#else
# define if_debug_name(msg, nt, nidx, pflag) DO_NOTHING
#endif
name_table *
names_init(ulong count, gs_ref_memory_t *imem)
{
gs_memory_t *mem = (gs_memory_t *)imem;
name_table *nt;
int i;
if (count == 0)
count = max_name_count + 1L;
else if (count - 1 > max_name_count)
return 0;
nt = gs_alloc_struct(mem, name_table, &st_name_table, "name_init(nt)");
if (nt == 0)
return 0;
memset(nt, 0, sizeof(name_table));
nt->max_sub_count =
((count - 1) | nt_sub_index_mask) >> nt_log2_sub_size;
nt->name_string_attrs = imemory_space(imem) | a_readonly;
nt->memory = mem;
for (i = 0; i < NT_1CHAR_FIRST + NT_1CHAR_SIZE; i += nt_sub_size) {
int code = name_alloc_sub(nt);
if (code < 0) {
while (nt->sub_next > 0)
name_free_sub(nt, --(nt->sub_next));
gs_free_object(mem, nt, "name_init(nt)");
return 0;
}
}
for (i = -1; i < NT_1CHAR_SIZE; i++) {
uint ncnt = NT_1CHAR_FIRST + i;
uint nidx = name_count_to_index(ncnt);
name *pname = names_index_ptr_inline(nt, nidx);
name_string_t *pnstr = names_index_string_inline(nt, nidx);
if (i < 0)
pnstr->string_bytes = nt_1char_names,
pnstr->string_size = 0;
else
pnstr->string_bytes = nt_1char_names + i,
pnstr->string_size = 1;
pnstr->foreign_string = 1;
pnstr->mark = 1;
pname->pvalue = pv_no_defn;
}
nt->perm_count = NT_1CHAR_FIRST + NT_1CHAR_SIZE;
nt->free = 0;
names_trace_finish(nt, NULL);
return nt;
}
gs_memory_t *
names_memory(const name_table * nt)
{
return nt->memory;
}
int
names_ref(name_table *nt, const byte *ptr, uint size, ref *pref, int enterflag)
{
name *pname;
name_string_t *pnstr;
uint nidx;
uint *phash;
switch (size) {
case 0:
nidx = name_count_to_index(1);
pname = names_index_ptr_inline(nt, nidx);
goto mkn;
case 1:
if (*ptr < NT_1CHAR_SIZE) {
uint hash = *ptr + NT_1CHAR_FIRST;
nidx = name_count_to_index(hash);
pname = names_index_ptr_inline(nt, nidx);
goto mkn;
}
default: {
uint hash;
NAME_HASH(hash, hash_permutation, ptr, size);
phash = nt->hash + (hash & (NT_HASH_SIZE - 1));
}
}
for (nidx = *phash; nidx != 0;
nidx = name_next_index(nidx, pnstr)
) {
pnstr = names_index_string_inline(nt, nidx);
if (pnstr->string_size == size &&
!memcmp_inline(ptr, pnstr->string_bytes, size)
) {
pname = name_index_ptr_inline(nt, nidx);
goto mkn;
}
}
if (enterflag < 0)
return_error(e_undefined);
if (size > max_name_string)
return_error(e_limitcheck);
nidx = nt->free;
if (nidx == 0) {
int code = name_alloc_sub(nt);
if (code < 0)
return code;
nidx = nt->free;
}
pnstr = names_index_string_inline(nt, nidx);
if (enterflag == 1) {
byte *cptr = (byte *)gs_alloc_string(nt->memory, size,
"names_ref(string)");
if (cptr == 0)
return_error(e_VMerror);
memcpy(cptr, ptr, size);
pnstr->string_bytes = cptr;
pnstr->foreign_string = 0;
} else {
pnstr->string_bytes = ptr;
pnstr->foreign_string = (enterflag == 0 ? 1 : 0);
}
pnstr->string_size = size;
pname = name_index_ptr_inline(nt, nidx);
pname->pvalue = pv_no_defn;
nt->free = name_next_index(nidx, pnstr);
set_name_next_index(nidx, pnstr, *phash);
*phash = nidx;
if_debug_name("new name", nt, nidx, &enterflag);
mkn:
make_name(pref, nidx, pname);
return 0;
}
void
names_string_ref(const name_table * nt, const ref * pnref ,
ref * psref )
{
const name_string_t *pnstr = names_string_inline(nt, pnref);
make_const_string(psref,
(pnstr->foreign_string ? avm_foreign | a_readonly :
nt->name_string_attrs),
pnstr->string_size,
(const byte *)pnstr->string_bytes);
}
int
names_from_string(name_table * nt, const ref * psref, ref * pnref)
{
int exec = r_has_attr(psref, a_executable);
int code = names_ref(nt, psref->value.bytes, r_size(psref), pnref, 1);
if (code < 0)
return code;
if (exec)
r_set_attrs(pnref, a_executable);
return code;
}
int
names_enter_string(name_table * nt, const char *str, ref * pref)
{
return names_ref(nt, (const byte *)str, strlen(str), pref, 0);
}
void
names_invalidate_value_cache(name_table * nt, const ref * pnref)
{
pnref->value.pname->pvalue = pv_other;
}
#undef names_index
name_index_t
names_index(const name_table * nt, const ref * pnref)
{
return names_index_inline(nt, pnref);
}
void
names_index_ref(const name_table * nt, name_index_t index, ref * pnref)
{
names_index_ref_inline(nt, index, pnref);
}
name *
names_index_ptr(const name_table * nt, name_index_t index)
{
return names_index_ptr_inline(nt, index);
}
name_index_t
names_next_valid_index(name_table * nt, name_index_t nidx)
{
const name_string_sub_table_t *ssub =
nt->sub[nidx >> nt_log2_sub_size].strings;
const name_string_t *pnstr;
do {
++nidx;
if ((nidx & nt_sub_index_mask) == 0)
for (;; nidx += nt_sub_size) {
if ((nidx >> nt_log2_sub_size) >= nt->sub_count)
return 0;
ssub = nt->sub[nidx >> nt_log2_sub_size].strings;
if (ssub != 0)
break;
}
pnstr = &ssub->strings[nidx & nt_sub_index_mask];
}
while (pnstr->string_bytes == 0);
return nidx;
}
void
names_unmark_all(name_table * nt)
{
uint si;
name_string_sub_table_t *ssub;
for (si = 0; si < nt->sub_count; ++si)
if ((ssub = nt->sub[si].strings) != 0) {
uint i;
for (i = 0; i < nt_sub_size; ++i)
if (name_index_to_count((si << nt_log2_sub_size) + i) >=
nt->perm_count)
ssub->strings[i].mark = 0;
}
}
bool
names_mark_index(name_table * nt, name_index_t nidx)
{
name_string_t *pnstr = names_index_string_inline(nt, nidx);
if (pnstr->mark)
return false;
pnstr->mark = 1;
return true;
}
void *
names_ref_sub_table(name_table * nt, const ref * pnref)
{
return pnref->value.pname - (r_size(pnref) & nt_sub_index_mask);
}
void *
names_index_sub_table(name_table * nt, name_index_t index)
{
return nt->sub[index >> nt_log2_sub_size].names;
}
void *
names_index_string_sub_table(name_table * nt, name_index_t index)
{
return nt->sub[index >> nt_log2_sub_size].strings;
}
void
names_trace_finish(name_table * nt, gc_state_t * gcst)
{
uint *phash = &nt->hash[0];
uint i;
for (i = 0; i < NT_HASH_SIZE; phash++, i++) {
name_index_t prev = 0;
name_string_t *pnprev = 0;
name_index_t nidx = *phash;
while (nidx != 0) {
name_string_t *pnstr = names_index_string_inline(nt, nidx);
name_index_t next = name_next_index(nidx, pnstr);
if (pnstr->mark) {
prev = nidx;
pnprev = pnstr;
} else {
if_debug_name("GC remove name", nt, nidx, NULL);
pnstr->string_bytes = 0;
pnstr->string_size = 0;
if (prev == 0)
*phash = next;
else
set_name_next_index(prev, pnprev, next);
}
nidx = next;
}
}
nt->free = 0;
for (i = nt->sub_count; i--;) {
name_sub_table *sub = nt->sub[i].names;
name_string_sub_table_t *ssub = nt->sub[i].strings;
if (sub != 0) {
name_scan_sub(nt, i, true);
if (nt->sub[i].names == 0 && gcst != 0) {
o_set_unmarked((obj_header_t *)sub - 1);
o_set_unmarked((obj_header_t *)ssub - 1);
}
}
if (i == 0)
break;
}
nt->sub_next = 0;
}
void
names_restore(name_table * nt, alloc_save_t * save)
{
uint si;
for (si = 0; si < nt->sub_count; ++si)
if (nt->sub[si].strings != 0) {
uint i;
for (i = 0; i < nt_sub_size; ++i) {
name_string_t *pnstr =
names_index_string_inline(nt, (si << nt_log2_sub_size) + i);
if (pnstr->string_bytes == 0)
pnstr->mark = 0;
else if (pnstr->foreign_string) {
if (!pnstr->mark)
pnstr->mark = 1;
} else
pnstr->mark =
!alloc_is_since_save(pnstr->string_bytes, save);
}
}
names_trace_finish(nt, NULL);
}
private int
name_alloc_sub(name_table * nt)
{
gs_memory_t *mem = nt->memory;
uint sub_index = nt->sub_next;
name_sub_table *sub;
name_string_sub_table_t *ssub;
for (;; ++sub_index) {
if (sub_index > nt->max_sub_count)
return_error(e_limitcheck);
if (nt->sub[sub_index].names == 0)
break;
}
nt->sub_next = sub_index + 1;
if (nt->sub_next > nt->sub_count)
nt->sub_count = nt->sub_next;
sub = gs_alloc_struct(mem, name_sub_table, &st_name_sub_table,
"name_alloc_sub(sub-table)");
ssub = gs_alloc_struct(mem, name_string_sub_table_t,
&st_name_string_sub_table,
"name_alloc_sub(string sub-table)");
if (sub == 0 || ssub == 0) {
gs_free_object(mem, ssub, "name_alloc_sub(string sub-table)");
gs_free_object(mem, sub, "name_alloc_sub(sub-table)");
return_error(e_VMerror);
}
memset(sub, 0, sizeof(name_sub_table));
memset(ssub, 0, sizeof(name_string_sub_table_t));
#if name_extension_bits > 0
sub->high_index = (sub_index >> (16 - nt_log2_sub_size)) << 16;
#endif
nt->sub[sub_index].names = sub;
nt->sub[sub_index].strings = ssub;
name_scan_sub(nt, sub_index, false);
#ifdef DEBUG
if (gs_debug_c('n')) {
int i0;
for (i0 = 0; i0 < NT_HASH_SIZE; i0 += 16) {
int i;
dlprintf1("[n]chain %d:", i0);
for (i = i0; i < i0 + 16; i++) {
int n = 0;
uint nidx;
for (nidx = nt->hash[i]; nidx != 0;
nidx = name_next_index(nidx,
names_index_string_inline(nt, nidx))
)
n++;
dprintf1(" %d", n);
}
dputc('\n');
}
}
#endif
return 0;
}
private void
name_free_sub(name_table * nt, uint sub_index)
{
gs_free_object(nt->memory, nt->sub[sub_index].strings,
"name_free_sub(string sub-table)");
gs_free_object(nt->memory, nt->sub[sub_index].names,
"name_free_sub(sub-table)");
nt->sub[sub_index].names = 0;
nt->sub[sub_index].strings = 0;
}
private void
name_scan_sub(name_table * nt, uint sub_index, bool free_empty)
{
name_string_sub_table_t *ssub = nt->sub[sub_index].strings;
uint free = nt->free;
uint nbase = sub_index << nt_log2_sub_size;
uint ncnt = nbase + (nt_sub_size - 1);
bool keep = !free_empty;
if (ssub == 0)
return;
if (nbase == 0)
nbase = 1, keep = true;
for (;; --ncnt) {
uint nidx = name_count_to_index(ncnt);
name_string_t *pnstr = &ssub->strings[nidx & nt_sub_index_mask];
if (pnstr->mark)
keep = true;
else {
set_name_next_index(nidx, pnstr, free);
free = nidx;
}
if (ncnt == nbase)
break;
}
if (keep)
nt->free = free;
else {
name_free_sub(nt, sub_index);
if (sub_index == nt->sub_count - 1) {
do {
--sub_index;
} while (nt->sub[sub_index].names == 0);
nt->sub_count = sub_index + 1;
if (nt->sub_next > sub_index)
nt->sub_next = sub_index;
} else if (nt->sub_next == sub_index)
nt->sub_next--;
}
}
private
ENUM_PTRS_BEGIN_PROC(name_table_enum_ptrs)
{
EV_CONST name_table *const nt = vptr;
uint i = index >> 1;
if (i >= nt->sub_count)
return 0;
if (index & 1)
ENUM_RETURN(nt->sub[i].strings);
else
ENUM_RETURN(nt->sub[i].names);
}
ENUM_PTRS_END_PROC
private RELOC_PTRS_WITH(name_table_reloc_ptrs, name_table *nt)
{
uint sub_count = nt->sub_count;
uint i;
for (i = 0; i < sub_count; i++) {
RELOC_VAR(nt->sub[i].names);
RELOC_VAR(nt->sub[i].strings);
}
}
RELOC_PTRS_END
private ENUM_PTRS_BEGIN_PROC(name_string_sub_enum_ptrs)
{
return 0;
}
ENUM_PTRS_END_PROC
private RELOC_PTRS_BEGIN(name_string_sub_reloc_ptrs)
{
name_string_t *pnstr = ((name_string_sub_table_t *)vptr)->strings;
uint i;
for (i = 0; i < nt_sub_size; ++pnstr, ++i) {
if (pnstr->string_bytes != 0 && !pnstr->foreign_string) {
gs_const_string nstr;
nstr.data = pnstr->string_bytes;
nstr.size = pnstr->string_size;
RELOC_CONST_STRING_VAR(nstr);
pnstr->string_bytes = nstr.data;
}
}
}
RELOC_PTRS_END