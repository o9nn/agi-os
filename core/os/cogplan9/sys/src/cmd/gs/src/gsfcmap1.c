#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gsutil.h"
#include "gxfcmap1.h"
inline private ulong
bytes2int(const byte *p, int n)
{
ulong v = 0;
int i;
for (i = 0; i < n; ++i)
v = (v << 8) + p[i];
return v;
}
public_st_cmap_adobe1();
private
ENUM_PTRS_WITH(cmap_lookup_range_enum_ptrs,
gx_cmap_lookup_range_t *pclr) return 0;
case 0:
if (pclr->value_type == CODE_VALUE_GLYPH) {
const byte *pv = pclr->values.data;
int size = pclr->value_size;
int k;
for (k = 0; k < pclr->num_entries; ++k, pv += size) {
gs_glyph glyph = bytes2int(pv, size);
pclr->cmap->mark_glyph(mem, glyph, pclr->cmap->mark_glyph_data);
}
}
return ENUM_OBJ(pclr->cmap);
case 1: return ENUM_STRING(&pclr->keys);
case 2: return ENUM_STRING(&pclr->values);
ENUM_PTRS_END
private
RELOC_PTRS_WITH(cmap_lookup_range_reloc_ptrs, gx_cmap_lookup_range_t *pclr)
RELOC_VAR(pclr->cmap);
RELOC_STRING_VAR(pclr->keys);
RELOC_STRING_VAR(pclr->values);
RELOC_PTRS_END
public_st_cmap_lookup_range();
public_st_cmap_lookup_range_element();
private void
print_msg_str_in_range(const byte *str,
const byte *key_lo, const byte *key_hi,
int key_size)
{
debug_print_string_hex(str, key_size);
dlprintf(" in ");
debug_print_string_hex(key_lo, key_size);
dlprintf(" - ");
debug_print_string_hex(key_hi, key_size);
dlprintf("\n");
}
private int
gs_cmap_get_shortest_chr(const gx_code_map_t * pcmap, uint *pfidx)
{
int i;
int len_shortest = MAX_CMAP_CODE_SIZE;
uint fidx_shortest = 0;
for (i = pcmap->num_lookup - 1; i >= 0; --i) {
const gx_cmap_lookup_range_t *pclr = &pcmap->lookup[i];
if ((pclr->key_prefix_size + pclr->key_size) <= len_shortest) {
len_shortest = (pclr->key_prefix_size + pclr->key_size);
fidx_shortest = pclr->font_index;
}
}
*pfidx = fidx_shortest;
return len_shortest;
}
private int
gs_multidim_CID_offset(const byte *key_str,
const byte *key_lo, const byte *key_hi,
int key_size)
{
int i;
int CID_offset = 0;
if (gs_debug_c('J')) {
dlprintf("[J]gmCo()         calc CID_offset for 0x");
print_msg_str_in_range(key_str, key_lo, key_hi, key_size);
}
for (i = 0; i < key_size; i++)
CID_offset = CID_offset * (key_hi[i] - key_lo[i] + 1) +
key_str[i] - key_lo[i];
if_debug1('J', "[J]gmCo()         CID_offset = %d\n", CID_offset);
return CID_offset;
}
private int
code_map_decode_next_multidim_regime(const gx_code_map_t * pcmap,
const gs_const_string * pstr,
uint * pindex, uint * pfidx,
gs_char * pchr, gs_glyph * pglyph)
{
const byte *str = pstr->data + *pindex;
uint ssize = pstr->size - *pindex;
int i;
int pm_maxlen = 0;
int pm_index = *pindex;
uint pm_fidx = *pfidx;
gs_char pm_chr = *pchr;
*pchr = '\0';
if (gs_debug_c('J')) {
dlprintf("[J]CMDNmr() is called: str=(");
debug_print_string_hex(str, ssize);
dlprintf3(") @ 0x%lx ssize=%d, %d ranges to check\n",
str, ssize, pcmap->num_lookup);
}
for (i = pcmap->num_lookup - 1; i >= 0; --i) {
const gx_cmap_lookup_range_t *pclr = &pcmap->lookup[i];
int pre_size = pclr->key_prefix_size, key_size = pclr->key_size,
chr_size = pre_size + key_size;
int j = 0;
if (ssize < chr_size)
continue;
if (0 < pre_size) {
const byte * prefix = pclr->key_prefix;
for (j = 0; j < pre_size; j++)
if (prefix[j] != str[j])
break;
if (0 == j)
continue;
else if (j < pre_size) {
if (gs_debug_c('J')) {
dlprintf("[J]CMDNmr() partial match with prefix:");
print_msg_str_in_range(str, prefix,
prefix, pre_size);
}
if (pm_maxlen < j) {
pm_maxlen = chr_size;
pm_chr = bytes2int(str, chr_size);
pm_index = (*pindex) + chr_size;
pm_fidx = pclr->font_index;
}
continue ;
}
if (gs_debug_c('J')) {
dlprintf("[J]CMDNmr()   full match with prefix:");
print_msg_str_in_range(str, prefix, prefix, pre_size);
}
}
{
const byte *key = pclr->keys.data;
int step = key_size;
int k, l;
const byte *pvalue = NULL;
if (pclr->key_is_range)
step <<=1;
for (k = 0; k < pclr->num_entries; ++k, key += step) {
if_debug0('j', "[j]CMDNmr()     check key:");
if (gs_debug_c('j'))
print_msg_str_in_range(str + pre_size,
key, key + step - key_size, key_size) ;
for (l = 0; l < key_size; l++) {
byte c = str[l + pre_size];
if (c < key[l] || c > key[step - key_size + l])
break;
}
if (pm_maxlen < pre_size + l) {
pm_maxlen = chr_size;
pm_chr = bytes2int(str, chr_size);
pm_index = (*pindex) + chr_size;
pm_fidx = pclr->font_index;
}
if (l == key_size)
break;
}
if (k == pclr->num_entries)
continue;
*pchr = bytes2int(str, chr_size);
*pindex += chr_size;
*pfidx = pclr->font_index;
pvalue = pclr->values.data + k * pclr->value_size;
if (gs_debug_c('J')) {
dlprintf("[J]CMDNmr()     full matched pvalue=(");
debug_print_string_hex(pvalue, pclr->value_size);
dlprintf(")\n");
}
switch (pclr->value_type) {
case CODE_VALUE_CID:
*pglyph = gs_min_cid_glyph +
bytes2int(pvalue, pclr->value_size) +
gs_multidim_CID_offset(str + pre_size,
key, key + step - key_size, key_size);
return 0;
case CODE_VALUE_NOTDEF:
*pglyph = gs_min_cid_glyph +
bytes2int(pvalue, pclr->value_size);
return 0;
case CODE_VALUE_GLYPH:
*pglyph = bytes2int(pvalue, pclr->value_size);
return 0;
case CODE_VALUE_CHARS:
*pglyph =
bytes2int(pvalue, pclr->value_size) +
bytes2int(str + pre_size, key_size) -
bytes2int(key, key_size);
return pclr->value_size;
default:
return_error(gs_error_rangecheck);
}
}
}
*pchr = pm_chr;
*pindex = pm_index;
*pfidx = pm_fidx;
*pglyph = gs_no_glyph;
if (gs_debug_c('J')) {
dlprintf("[J]CMDNmr()     no full match, use partial match for (");
debug_print_string_hex(str, pm_maxlen);
dlprintf(")\n");
}
return 0;
}
private int
gs_cmap_adobe1_decode_next(const gs_cmap_t * pcmap_in,
const gs_const_string * pstr,
uint * pindex, uint * pfidx,
gs_char * pchr, gs_glyph * pglyph)
{
const gs_cmap_adobe1_t *pcmap = (const gs_cmap_adobe1_t *)pcmap_in;
uint save_index = *pindex;
int code;
uint pm_index;
uint pm_fidx;
gs_char pm_chr;
if_debug0('J', "[J]GCDN() check def CMap\n");
code =
code_map_decode_next_multidim_regime(&pcmap->def, pstr, pindex, pfidx, pchr, pglyph);
if (code != 0 || *pglyph != gs_no_glyph)
return code;
pm_index = *pindex;
pm_fidx = *pfidx;
pm_chr = *pchr;
if_debug0('J', "[J]GCDN() check notdef CMap\n");
*pindex = save_index;
code =
code_map_decode_next_multidim_regime(&pcmap->notdef, pstr, pindex, pfidx, pchr, pglyph);
if (code != 0 || *pglyph != gs_no_glyph)
return code;
if (save_index < pm_index) {
*pglyph = gs_min_cid_glyph;
*pindex = pm_index;
*pfidx = pm_fidx;
*pchr = '\0';
return 0;
}
else {
const byte *str = pstr->data + save_index;
uint ssize = pstr->size - save_index;
int chr_size_shortest =
gs_cmap_get_shortest_chr(&pcmap->def, pfidx);
if (chr_size_shortest <= ssize) {
*pglyph = gs_min_cid_glyph;
*pindex = save_index + chr_size_shortest;
*pchr = '\0';
if (gs_debug_c('J')) {
dlprintf1("[J]GCDN() no partial match, skip %d byte (",
chr_size_shortest);
debug_print_string_hex(str, chr_size_shortest);
dlprintf(")\n");
}
return 0;
}
else {
if (gs_debug_c('J')) {
dlprintf2("[J]GCDN() left data in buffer (%d) is shorter than shortest defined character (%d)\n",
ssize, chr_size_shortest);
}
*pglyph = gs_no_glyph;
return_error(gs_error_rangecheck);
}
}
}
private int
adobe1_next_range(gs_cmap_ranges_enum_t *penum)
{
const gs_cmap_adobe1_t *const pcmap =
(const gs_cmap_adobe1_t *)penum->cmap;
if (penum->index >= pcmap->code_space.num_ranges)
return 1;
penum->range = pcmap->code_space.ranges[penum->index++];
return 0;
}
private const gs_cmap_ranges_enum_procs_t adobe1_range_procs = {
adobe1_next_range
};
private void
gs_cmap_adobe1_enum_ranges(const gs_cmap_t *pcmap, gs_cmap_ranges_enum_t *pre)
{
gs_cmap_ranges_enum_setup(pre, pcmap, &adobe1_range_procs);
}
private int
adobe1_next_lookup(gs_cmap_lookups_enum_t *penum, const gx_code_map_t *pcm)
{
const gx_cmap_lookup_range_t *lookup = &pcm->lookup[penum->index[0]];
if (penum->index[0] >= pcm->num_lookup)
return 1;
penum->entry.key_size = lookup->key_prefix_size + lookup->key_size;
penum->entry.key_is_range = lookup->key_is_range;
penum->entry.value_type = lookup->value_type;
penum->entry.value.size = lookup->value_size;
penum->entry.font_index = lookup->font_index;
penum->index[0]++;
penum->index[1] = 0;
return 0;
}
private int
adobe1_next_lookup_def(gs_cmap_lookups_enum_t *penum)
{
return adobe1_next_lookup(penum,
&((const gs_cmap_adobe1_t *)penum->cmap)->def);
}
private int
adobe1_next_lookup_notdef(gs_cmap_lookups_enum_t *penum)
{
return adobe1_next_lookup(penum,
&((const gs_cmap_adobe1_t *)penum->cmap)->notdef);
}
private int
adobe1_next_entry(gs_cmap_lookups_enum_t *penum, const gx_code_map_t *pcm)
{
const gx_cmap_lookup_range_t *lookup = &pcm->lookup[penum->index[0] - 1];
int psize = lookup->key_prefix_size;
int ksize = lookup->key_size;
const byte *key =
lookup->keys.data + penum->index[1] * ksize *
(lookup->key_is_range ? 2 : 1);
int i;
if (penum->index[1] >= lookup->num_entries)
return 1;
if (psize + ksize > MAX_CMAP_CODE_SIZE)
return_error(gs_error_rangecheck);
for (i = 0; i < 2; ++i, key += ksize) {
memcpy(penum->entry.key[i], lookup->key_prefix, psize);
memcpy(penum->entry.key[i] + psize, key, ksize);
}
penum->entry.value.data =
lookup->values.data + penum->index[1] * lookup->value_size;
penum->entry.value.size = lookup->value_size;
penum->index[1]++;
return 0;
}
private int
adobe1_next_entry_def(gs_cmap_lookups_enum_t *penum)
{
return adobe1_next_entry(penum,
&((const gs_cmap_adobe1_t *)penum->cmap)->def);
}
private int
adobe1_next_entry_notdef(gs_cmap_lookups_enum_t *penum)
{
return adobe1_next_entry(penum,
&((const gs_cmap_adobe1_t *)penum->cmap)->notdef);
}
private const gs_cmap_lookups_enum_procs_t adobe1_lookup_def_procs = {
adobe1_next_lookup_def, adobe1_next_entry_def
};
private const gs_cmap_lookups_enum_procs_t adobe1_lookup_notdef_procs = {
adobe1_next_lookup_notdef, adobe1_next_entry_notdef
};
private void
gs_cmap_adobe1_enum_lookups(const gs_cmap_t *pcmap, int which,
gs_cmap_lookups_enum_t *pre)
{
gs_cmap_lookups_enum_setup(pre, pcmap,
(which ? &adobe1_lookup_notdef_procs :
&adobe1_lookup_def_procs));
}
private const gs_cmap_procs_t cmap_adobe1_procs = {
gs_cmap_adobe1_decode_next,
gs_cmap_adobe1_enum_ranges,
gs_cmap_adobe1_enum_lookups,
gs_cmap_compute_identity
};
int
gs_cmap_adobe1_alloc(gs_cmap_adobe1_t **ppcmap, int wmode,
const byte *map_name, uint name_size,
uint num_fonts, uint num_ranges, uint num_lookups,
uint keys_size, uint values_size,
const gs_cid_system_info_t *pcidsi_in, gs_memory_t *mem)
{
gs_cmap_t *pcmap;
gs_cmap_adobe1_t *pcmap1;
gx_code_space_range_t *ranges = (gx_code_space_range_t *)
gs_alloc_byte_array(mem, num_ranges, sizeof(gx_code_space_range_t),
"gs_cmap_alloc(code space ranges)");
gx_cmap_lookup_range_t *lookups =
(num_lookups == 0 ? NULL :
gs_alloc_struct_array(mem, num_lookups, gx_cmap_lookup_range_t,
&st_cmap_lookup_range,
"gs_cmap_alloc(lookup ranges)"));
byte *keys =
(keys_size == 0 ? NULL :
gs_alloc_string(mem, keys_size, "gs_cmap_alloc(keys)"));
byte *values =
(values_size == 0 ? NULL :
gs_alloc_string(mem, values_size, "gs_cmap_alloc(values)"));
int code =
gs_cmap_alloc(&pcmap, &st_cmap_adobe1, wmode, map_name, name_size,
pcidsi_in, num_fonts, &cmap_adobe1_procs, mem);
uint i;
if (code < 0 || ranges == 0 || (num_lookups != 0 && lookups == 0) ||
(keys_size != 0 && keys == 0) || (values_size != 0 && values == 0)) {
gs_free_string(mem, values, values_size, "gs_cmap_alloc(values)");
gs_free_string(mem, keys, keys_size, "gs_cmap_alloc(keys)");
gs_free_object(mem, lookups, "gs_cmap_alloc(lookup ranges)");
gs_free_object(mem, ranges, "gs_cmap_alloc(code space ranges)");
return_error(gs_error_VMerror);
}
*ppcmap = pcmap1 = (gs_cmap_adobe1_t *)pcmap;
pcmap1->code_space.ranges = ranges;
pcmap1->code_space.num_ranges = num_ranges;
if (num_lookups > 0) {
for (i = 0; i < num_lookups; ++i) {
memset(&lookups[i], 0, sizeof(*lookups));
lookups[i].cmap = pcmap1;
}
lookups[0].keys.data = keys;
lookups[0].keys.size = keys_size;
lookups[0].values.data = values;
lookups[0].values.size = values_size;
}
pcmap1->def.lookup = lookups;
pcmap1->def.num_lookup = num_lookups;
pcmap1->notdef.lookup = 0;
pcmap1->notdef.num_lookup = 0;
return 0;
}