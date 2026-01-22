#include "memory_.h"
#include "string_.h"
#include "math_.h"
#include "ghost.h"
#include "gp.h"
#include "oper.h"
#include "gxdevice.h"
#include "gxfont.h"
#include "gxfont1.h"
#include "gxchar.h"
#include "gxpath.h"
#include "gxfcache.h"
#include "gxchrout.h"
#include "gscoord.h"
#include "gspaint.h"
#include "gsfont.h"
#include "gspath.h"
#include "bfont.h"
#include "dstack.h"
#include "ichar.h"
#include "idict.h"
#include "iddict.h"
#include "idparam.h"
#include "iname.h"
#include "ifont.h"
#include "icid.h"
#include "igstate.h"
#include "icharout.h"
#include "ifapi.h"
#include "iplugin.h"
#include "store.h"
#include "gzstate.h"
#include "gdevpsf.h"
#include "stream.h"
#include "files.h"
#include "gscrypt1.h"
#include "gxfcid.h"
typedef struct FAPI_outline_handler_s {
struct gx_path_s *path;
fixed x0, y0;
bool close_path, need_close;
} FAPI_outline_handler;
private inline int import_shift(int x, int n)
{ return n > 0 ? x << n : x >> -n;
}
private inline int export_shift(int x, int n)
{ return n > 0 ? x >> n : x << -n;
}
private inline int fapi_round(double x)
{ return (int)(x + 0.5);
}
private int add_closepath(FAPI_path *I)
{ FAPI_outline_handler *olh = (FAPI_outline_handler *)I->olh;
olh->need_close = false;
return gx_path_close_subpath_notes(olh->path, 0);
}
private int add_move(FAPI_path *I, FracInt x, FracInt y)
{ FAPI_outline_handler *olh = (FAPI_outline_handler *)I->olh;
int code;
if (olh->need_close && olh->close_path)
if ((code = add_closepath(I)) < 0)
return code;
olh->need_close = false;
return gx_path_add_point(olh->path, import_shift(x, I->shift) + olh->x0, -import_shift(y, I->shift) + olh->y0);
}
private int add_line(FAPI_path *I, FracInt x, FracInt y)
{ FAPI_outline_handler *olh = (FAPI_outline_handler *)I->olh;
olh->need_close = true;
return gx_path_add_line_notes(olh->path, import_shift(x, I->shift) + olh->x0, -import_shift(y, I->shift) + olh->y0, 0);
}
private int add_curve(FAPI_path *I, FracInt x0, FracInt y0, FracInt x1, FracInt y1, FracInt x2, FracInt y2)
{ FAPI_outline_handler *olh = (FAPI_outline_handler *)I->olh;
olh->need_close = true;
return gx_path_add_curve_notes(olh->path, import_shift(x0, I->shift) + olh->x0, -import_shift(y0, I->shift) + olh->y0,
import_shift(x1, I->shift) + olh->x0, -import_shift(y1, I->shift) + olh->y0,
import_shift(x2, I->shift) + olh->x0, -import_shift(y2, I->shift) + olh->y0, 0);
}
private FAPI_path path_interface_stub = { NULL, 0, add_move, add_line, add_curve, add_closepath };
private inline bool IsCIDFont(const gs_font_base *pbfont)
{ return (pbfont->FontType == ft_CID_encrypted ||
pbfont->FontType == ft_CID_user_defined ||
pbfont->FontType == ft_CID_TrueType);
}
private inline bool IsType1GlyphData(const gs_font_base *pbfont)
{ return pbfont->FontType == ft_encrypted ||
pbfont->FontType == ft_encrypted2 ||
pbfont->FontType == ft_CID_encrypted;
}
typedef struct sfnts_reader_s sfnts_reader;
struct sfnts_reader_s {
ref *sfnts;
const gs_memory_t *memory;
const byte *p;
long index;
uint offset;
uint length;
bool error;
byte (*rbyte)(sfnts_reader *r);
ushort (*rword)(sfnts_reader *r);
ulong (*rlong)(sfnts_reader *r);
void (*rstring)(sfnts_reader *r, byte *v, int length);
void (*seek)(sfnts_reader *r, ulong pos);
};
private void sfnts_next_elem(sfnts_reader *r)
{ ref s;
if (r->error)
return;
r->index++;
r->error |= (array_get(r->memory, r->sfnts, r->index, &s) < 0);
if (r->error)
return;
r->p = s.value.const_bytes;
r->length = r_size(&s) & ~(uint)1;
r->offset = 0;
}
private inline byte sfnts_reader_rbyte_inline(sfnts_reader *r)
{ if (r->offset >= r->length)
sfnts_next_elem(r);
return (r->error ? 0 : r->p[r->offset++]);
}
private byte sfnts_reader_rbyte(sfnts_reader *r)
{ return sfnts_reader_rbyte_inline(r);
}
private ushort sfnts_reader_rword(sfnts_reader *r)
{ return (sfnts_reader_rbyte_inline(r) << 8) + sfnts_reader_rbyte_inline(r);
}
private ulong sfnts_reader_rlong(sfnts_reader *r)
{ return (sfnts_reader_rbyte_inline(r) << 24) + (sfnts_reader_rbyte_inline(r) << 16) +
(sfnts_reader_rbyte_inline(r) << 8) + sfnts_reader_rbyte_inline(r);
}
private void sfnts_reader_rstring(sfnts_reader *r, byte *v, int length)
{ if (length < 0)
return;
while (!r->error) {
int l = min(length, r->length - r->offset);
memcpy(v, r->p + r->offset, l);
r->p += l;
length -= l;
if (length <= 0)
return;
sfnts_next_elem(r);
}
}
private void sfnts_reader_seek(sfnts_reader *r, ulong pos)
{
ulong skipped = 0;
r->index = -1;
sfnts_next_elem(r);
while (skipped + r->length < pos && !r->error) {
skipped += r->length;
sfnts_next_elem(r);
}
r->offset = pos - skipped;
}
private void sfnts_reader_init(sfnts_reader *r, ref *pdr)
{ r->rbyte = sfnts_reader_rbyte;
r->rword = sfnts_reader_rword;
r->rlong = sfnts_reader_rlong;
r->rstring = sfnts_reader_rstring;
r->seek = sfnts_reader_seek;
r->index = -1;
r->error = false;
if (r_type(pdr) != t_dictionary ||
dict_find_string(pdr, "sfnts", &r->sfnts) <= 0)
r->error = true;
sfnts_next_elem(r);
}
typedef struct sfnts_writer_s sfnts_writer;
struct sfnts_writer_s {
byte *buf, *p;
int buf_size;
void (*wbyte)(sfnts_writer *w, byte v);
void (*wword)(sfnts_writer *w, ushort v);
void (*wlong)(sfnts_writer *w, ulong v);
void (*wstring)(sfnts_writer *w, byte *v, int length);
};
private void sfnts_writer_wbyte(sfnts_writer *w, byte v)
{ if (w->buf + w->buf_size < w->p + 1)
return;
w->p[0] = v;
w->p++;
}
private void sfnts_writer_wword(sfnts_writer *w, ushort v)
{ if (w->buf + w->buf_size < w->p + 2)
return;
w->p[0] = v / 256;
w->p[1] = v % 256;
w->p += 2;
}
private void sfnts_writer_wlong(sfnts_writer *w, ulong v)
{ if (w->buf + w->buf_size < w->p + 4)
return;
w->p[0] = v >> 24;
w->p[1] = (v >> 16) & 0xFF;
w->p[2] = (v >> 8) & 0xFF;
w->p[3] = v & 0xFF;
w->p += 4;
}
private void sfnts_writer_wstring(sfnts_writer *w, byte *v, int length)
{ if (w->buf + w->buf_size < w->p + length)
return;
memcpy(w->p, v, length);
w->p += length;
}
private const sfnts_writer sfnts_writer_stub = {
0, 0, 0,
sfnts_writer_wbyte,
sfnts_writer_wword,
sfnts_writer_wlong,
sfnts_writer_wstring
};
private inline bool sfnts_need_copy_table(byte *tag)
{ return memcmp(tag, "glyf", 4) &&
memcmp(tag, "glyx", 4) &&
memcmp(tag, "loca", 4) &&
memcmp(tag, "locx", 4) &&
memcmp(tag, "cmap", 4);
}
private void sfnt_copy_table(sfnts_reader *r, sfnts_writer *w, int length)
{ byte buf[1024];
while (length > 0 && !r->error) {
int l = min(length, sizeof(buf));
r->rstring(r, buf, l);
w->wstring(w, buf, l);
length -= l;
}
}
private ulong sfnts_copy_except_glyf(sfnts_reader *r, sfnts_writer *w)
{
struct {
byte tag[4];
ulong checkSum, offset, offset_new, length;
} tables[30];
const ushort alignment = 4;
ulong version = r->rlong(r);
ushort num_tables = r->rword(r);
ushort i, num_tables_new = 0;
ushort searchRange, entrySelector = 0, rangeShift, v;
ulong size_new = 12;
r->rword(r);
r->rword(r);
r->rword(r);
for (i = 0; i < num_tables; i++) {
if (r->error)
return 0;
r->rstring(r, tables[i].tag, 4);
tables[i].checkSum = r->rlong(r);
tables[i].offset = r->rlong(r);
tables[i].length = r->rlong(r);
tables[i].offset_new = size_new;
if (sfnts_need_copy_table(tables[i].tag)) {
num_tables_new ++;
size_new += (tables[i].length + alignment - 1) / alignment * alignment;
}
}
size_new += num_tables_new * 16;
if (w == 0)
return size_new;
searchRange = v = num_tables_new * 16;
for (i = 0; v; i++) {
v >>= 1;
searchRange |= v;
entrySelector++;
}
searchRange -= searchRange >> 1;
rangeShift = num_tables_new * 16 - searchRange;
w->wlong(w, version);
w->wword(w, num_tables_new);
w->wword(w, searchRange);
w->wword(w, entrySelector);
w->wword(w, rangeShift);
for (i = 0; i < num_tables; i++)
if (sfnts_need_copy_table(tables[i].tag)) {
w->wstring(w, tables[i].tag, 4);
w->wlong(w, tables[i].checkSum);
w->wlong(w, tables[i].offset_new + num_tables_new * 16);
w->wlong(w, tables[i].length);
}
for (i = 0; i < num_tables; i++)
if (sfnts_need_copy_table(tables[i].tag)) {
int k = tables[i].length;
r->seek(r, tables[i].offset);
if (r->error)
return 0;
if (w->p - w->buf != tables[i].offset_new + num_tables_new * 16)
return 0;
sfnt_copy_table(r, w, tables[i].length);
for (; k & (alignment - 1); k++)
w->wbyte(w, 0);
}
return size_new;
}
private ulong true_type_size(ref *pdr)
{ sfnts_reader r;
sfnts_reader_init(&r, pdr);
return sfnts_copy_except_glyf(&r, 0);
}
private ushort FAPI_FF_serialize_tt_font(FAPI_font *ff, void *buf, int buf_size)
{ ref *pdr = (ref *)ff->client_font_data2;
sfnts_reader r;
sfnts_writer w = sfnts_writer_stub;
w.buf_size = buf_size;
w.buf = w.p = buf;
sfnts_reader_init(&r, pdr);
if(!sfnts_copy_except_glyf(&r, &w))
return 1;
return r.error;
}
private inline ushort float_to_ushort(float v)
{ return (ushort)(v * 16);
}
private ushort FAPI_FF_get_word(FAPI_font *ff, fapi_font_feature var_id, int index)
{ gs_font_type1 *pfont = (gs_font_type1 *)ff->client_font_data;
ref *pdr = (ref *)ff->client_font_data2;
switch((int)var_id) {
case FAPI_FONT_FEATURE_Weight: return 0;
case FAPI_FONT_FEATURE_ItalicAngle: return 0;
case FAPI_FONT_FEATURE_IsFixedPitch: return 0;
case FAPI_FONT_FEATURE_UnderLinePosition: return 0;
case FAPI_FONT_FEATURE_UnderlineThickness: return 0;
case FAPI_FONT_FEATURE_FontType: return (pfont->FontType == 2 ? 2 : 1);
case FAPI_FONT_FEATURE_FontBBox:
switch (index) {
case 0 : return (ushort)pfont->FontBBox.p.x;
case 1 : return (ushort)pfont->FontBBox.p.y;
case 2 : return (ushort)pfont->FontBBox.q.x;
case 3 : return (ushort)pfont->FontBBox.q.y;
}
return 0;
case FAPI_FONT_FEATURE_BlueValues_count: return pfont->data.BlueValues.count;
case FAPI_FONT_FEATURE_BlueValues: return float_to_ushort(pfont->data.BlueValues.values[index]);
case FAPI_FONT_FEATURE_OtherBlues_count: return pfont->data.OtherBlues.count;
case FAPI_FONT_FEATURE_OtherBlues: return float_to_ushort(pfont->data.OtherBlues.values[index]);
case FAPI_FONT_FEATURE_FamilyBlues_count: return pfont->data.FamilyBlues.count;
case FAPI_FONT_FEATURE_FamilyBlues: return float_to_ushort(pfont->data.FamilyBlues.values[index]);
case FAPI_FONT_FEATURE_FamilyOtherBlues_count: return pfont->data.FamilyOtherBlues.count;
case FAPI_FONT_FEATURE_FamilyOtherBlues: return float_to_ushort(pfont->data.FamilyOtherBlues.values[index]);
case FAPI_FONT_FEATURE_BlueShift: return float_to_ushort(pfont->data.BlueShift);
case FAPI_FONT_FEATURE_BlueFuzz: return float_to_ushort(pfont->data.BlueShift);
case FAPI_FONT_FEATURE_StdHW: return (pfont->data.StdHW.count == 0 ? 0 : float_to_ushort(pfont->data.StdHW.values[0]));
case FAPI_FONT_FEATURE_StdVW: return (pfont->data.StdVW.count == 0 ? 0 : float_to_ushort(pfont->data.StdVW.values[0]));
case FAPI_FONT_FEATURE_StemSnapH_count: return pfont->data.StemSnapH.count;
case FAPI_FONT_FEATURE_StemSnapH: return float_to_ushort(pfont->data.StemSnapH.values[index]);
case FAPI_FONT_FEATURE_StemSnapV_count: return pfont->data.StemSnapV.count;
case FAPI_FONT_FEATURE_StemSnapV: return float_to_ushort(pfont->data.StemSnapV.values[index]);
case FAPI_FONT_FEATURE_ForceBold: return pfont->data.ForceBold;
case FAPI_FONT_FEATURE_LanguageGroup: return pfont->data.LanguageGroup;
case FAPI_FONT_FEATURE_lenIV: return (ff->need_decrypt ? 0 : pfont->data.lenIV);
case FAPI_FONT_FEATURE_Subrs_count:
{ ref *Private, *Subrs, *GlobalSubrs;
int n1, n2;
if (dict_find_string(pdr, "Private", &Private) <= 0)
return 0;
if (dict_find_string(Private, "Subrs", &Subrs) <= 0)
Subrs = NULL;
if (dict_find_string(Private, "GlobalSubrs", &GlobalSubrs) <= 0)
GlobalSubrs = NULL;
n1 = (Subrs != NULL ? r_size(Subrs) : 0);
n2 = (GlobalSubrs != NULL ? r_size(GlobalSubrs) : 0);
return (n1 < n2 ? n2 : n1) * 2;
}
}
return 0;
}
private ulong FAPI_FF_get_long(FAPI_font *ff, fapi_font_feature var_id, int index)
{ gs_font_type1 *pfont = (gs_font_type1 *)ff->client_font_data;
ref *pdr = (ref *)ff->client_font_data2;
switch((int)var_id) {
case FAPI_FONT_FEATURE_UniqueID: return pfont->UID.id;
case FAPI_FONT_FEATURE_BlueScale: return (ulong)(pfont->data.BlueScale * 65536);
case FAPI_FONT_FEATURE_Subrs_total_size :
{ ref *Private, *Subrs, v;
int lenIV = max(pfont->data.lenIV, 0), k;
ulong size = 0;
long i;
const char *name[2] = {"Subrs", "GlobalSubrs"};
if (dict_find_string(pdr, "Private", &Private) <= 0)
return 0;
for (k = 0; k < 2; k++) {
if (dict_find_string(Private, name[k], &Subrs) > 0)
for (i = r_size(Subrs) - 1; i >= 0; i--) {
array_get(pfont->memory, Subrs, i, &v);
size += r_size(&v) - (ff->need_decrypt ? 0 : lenIV);
}
}
return size;
}
case FAPI_FONT_FEATURE_TT_size:
return true_type_size(pdr);
}
return 0;
}
private float FAPI_FF_get_float(FAPI_font *ff, fapi_font_feature var_id, int index)
{ gs_font_base *pbfont = (gs_font_base *)ff->client_font_data;
switch((int)var_id) {
case FAPI_FONT_FEATURE_FontMatrix:
{ double FontMatrix_div = (ff->is_cid && !IsCIDFont(pbfont) ? 1000 : 1);
switch(index) {
case 0 : return pbfont->base->FontMatrix.xx / FontMatrix_div;
case 1 : return pbfont->base->FontMatrix.xy / FontMatrix_div;
case 2 : return pbfont->base->FontMatrix.yx / FontMatrix_div;
case 3 : return pbfont->base->FontMatrix.yy / FontMatrix_div;
case 4 : return pbfont->base->FontMatrix.tx / FontMatrix_div;
case 5 : return pbfont->base->FontMatrix.ty / FontMatrix_div;
}
}
}
return 0;
}
private inline void decode_bytes(byte *p, const byte *s, int l, int lenIV)
{ ushort state = 4330;
for (; l; s++, l--) {
uchar c = (*s ^ (state >> 8));
state = (*s + state) * crypt_c1 + crypt_c2;
if (lenIV > 0)
lenIV--;
else {
*p = c;
p++;
}
}
}
private ushort get_type1_data(FAPI_font *ff, const ref *type1string,
byte *buf, ushort buf_length)
{ gs_font_type1 *pfont = (gs_font_type1 *)ff->client_font_data;
int lenIV = max(pfont->data.lenIV, 0);
int length = r_size(type1string) - (ff->need_decrypt ? lenIV : 0);
if (buf != 0) {
int l = min(length, buf_length);
if (ff->need_decrypt && pfont->data.lenIV >= 0)
decode_bytes(buf, type1string->value.const_bytes, l + lenIV, lenIV);
else
memcpy(buf, type1string->value.const_bytes, l);
}
return length;
}
private ushort FAPI_FF_get_subr(FAPI_font *ff, int index, byte *buf, ushort buf_length)
{ ref *pdr = (ref *)ff->client_font_data2;
ref *Private, *Subrs, *GlobalSubrs, subr;
int n1, n2, n;
if (dict_find_string(pdr, "Private", &Private) <= 0)
return 0;
if (dict_find_string(Private, "Subrs", &Subrs) <= 0)
Subrs = NULL;
if (dict_find_string(Private, "GlobalSubrs", &GlobalSubrs) <= 0)
GlobalSubrs = NULL;
n1 = (Subrs != NULL ? r_size(Subrs) : 0);
n2 = (GlobalSubrs != NULL ? r_size(GlobalSubrs) : 0);
n = (n1 < n2 ? n2 : n1);
if (index < n && Subrs != NULL) {
if (array_get(ff->memory, Subrs, index, &subr) < 0 || r_type(&subr) != t_string)
return 0;
} else if (index >= n && GlobalSubrs != NULL) {
if (array_get(ff->memory,
GlobalSubrs, index - n, &subr) < 0 || r_type(&subr) != t_string)
return 0;
} else
return 0;
return get_type1_data(ff, &subr, buf, buf_length);
}
private bool sfnt_get_glyph_offset(ref *pdr, gs_font_type42 *pfont42, int index, ulong *offset0, ulong *offset1)
{
sfnts_reader r;
int glyf_elem_size = (2 << pfont42->data.indexToLocFormat);
sfnts_reader_init(&r, pdr);
r.seek(&r, pfont42->data.loca + index * glyf_elem_size);
*offset0 = pfont42->data.glyf + (glyf_elem_size == 2 ? r.rword(&r) * 2 : r.rlong(&r));
*offset1 = pfont42->data.glyf + (glyf_elem_size == 2 ? r.rword(&r) * 2 : r.rlong(&r));
return r.error;
}
private int get_GlyphDirectory_data_ptr(const gs_memory_t *mem,
ref *pdr, int char_code, const byte **ptr)
{
ref *GlyphDirectory, glyph0, *glyph = &glyph0, glyph_index;
if ((dict_find_string(pdr, "GlyphDirectory", &GlyphDirectory) > 0 &&
(r_type(GlyphDirectory) == t_dictionary &&
( make_int(&glyph_index, char_code),
dict_find(GlyphDirectory, &glyph_index, &glyph) > 0))) ||
((r_type(GlyphDirectory) == t_array &&
array_get(mem, GlyphDirectory, char_code, &glyph0) >= 0) &&
r_type(glyph) == t_string)) {
*ptr = glyph->value.const_bytes;
return r_size(glyph);
}
return -1;
}
private bool get_MetricsCount(FAPI_font *ff)
{ if (!ff->is_type1 && ff->is_cid) {
gs_font_cid2 *pfcid = (gs_font_cid2 *)ff->client_font_data;
return pfcid->cidata.MetricsCount;
}
return 0;
}
private ushort FAPI_FF_get_glyph(FAPI_font *ff, int char_code, byte *buf, ushort buf_length)
{
ref *pdr = (ref *)ff->client_font_data2;
ushort glyph_length;
if (ff->is_type1) {
if (ff->is_cid) {
const ref *glyph = ff->char_data;
glyph_length = get_type1_data(ff, glyph, buf, buf_length);
} else {
ref *CharStrings, char_name, *glyph;
if (ff->char_data != NULL) {
if (name_ref(ff->memory, ff->char_data,
ff->char_data_len, &char_name, -1) < 0)
return -1;
if (buf != NULL) {
ff->char_data = NULL;
}
} else {
i_ctx_t *i_ctx_p = (i_ctx_t *)ff->client_ctx_p;
ref *StandardEncoding;
if (dict_find_string(systemdict, "StandardEncoding", &StandardEncoding) <= 0 ||
array_get(ff->memory, StandardEncoding, char_code, &char_name) < 0)
if (name_ref(ff->memory, (const byte *)".notdef", 7, &char_name, -1) < 0)
return -1;
}
if (dict_find_string(pdr, "CharStrings", &CharStrings) <= 0)
return -1;
if (dict_find(CharStrings, &char_name, &glyph) <= 0)
return -1;
glyph_length = get_type1_data(ff, glyph, buf, buf_length);
}
} else {
const byte *data_ptr;
int l = get_GlyphDirectory_data_ptr(ff->memory, pdr, char_code, &data_ptr);
if (l >= 0) {
int MetricsCount = get_MetricsCount(ff), mc = MetricsCount << 1;
glyph_length = max((ushort)(l - mc), 0);
if (buf != 0 && glyph_length > 0)
memcpy(buf, data_ptr + mc, min(glyph_length, buf_length));
} else {
gs_font_type42 *pfont42 = (gs_font_type42 *)ff->client_font_data;
ulong offset0, offset1;
bool error = sfnt_get_glyph_offset(pdr, pfont42, char_code, &offset0, &offset1);
glyph_length = (error ? -1 : offset1 - offset0);
if (buf != 0 && !error) {
sfnts_reader r;
sfnts_reader_init(&r, pdr);
r.seek(&r, offset0);
r.rstring(&r, buf, min(glyph_length, buf_length));
if (r.error)
glyph_length = -1;
}
}
}
return glyph_length;
}
private const FAPI_font ff_stub = {
0,
0,
NULL,
0,
0,
false,
false,
false,
0,
0,
0,
0,
0,
FAPI_FF_get_word,
FAPI_FF_get_long,
FAPI_FF_get_float,
FAPI_FF_get_subr,
FAPI_FF_get_glyph,
FAPI_FF_serialize_tt_font
};
private int FAPI_get_xlatmap(i_ctx_t *i_ctx_p, char **xlatmap)
{ ref *pref;
int code;
if ((code = dict_find_string(systemdict, ".xlatmap", &pref)) < 0)
return code;
if (r_type(pref) != t_string)
return_error(e_typecheck);
*xlatmap = (char *)pref->value.bytes;
return 0;
}
private int renderer_retcode(i_ctx_t *i_ctx_p, FAPI_server *I, FAPI_retcode rc)
{ if (rc == 0)
return 0;
eprintf2("Error: Font Renderer Plugin ( %s ) return code = %d\n", I->ig.d->subtype, rc);
return rc < 0 ? rc : e_invalidfont;
}
private int zFAPIavailable(i_ctx_t *i_ctx_p)
{ i_plugin_holder *h = i_plugin_get_list(i_ctx_p);
bool available = true;
os_ptr op = osp;
for (; h != 0; h = h->next)
if (!strcmp(h->I->d->type,"FAPI"))
goto found;
available = false;
found :
push(1);
make_bool(op, available);
return 0;
}
private int FAPI_find_plugin(i_ctx_t *i_ctx_p, const char *subtype, FAPI_server **pI)
{ i_plugin_holder *h = i_plugin_get_list(i_ctx_p);
int code;
for (; h != 0; h = h->next)
if (!strcmp(h->I->d->type,"FAPI"))
if (!strcmp(h->I->d->subtype, subtype)) {
FAPI_server *I = *pI = (FAPI_server *)h->I;
if ((code = renderer_retcode(i_ctx_p, I, I->ensure_open(I))) < 0)
return code;
return 0;
}
return_error(e_invalidfont);
}
private int FAPI_prepare_font(i_ctx_t *i_ctx_p, FAPI_server *I, ref *pdr, gs_font_base *pbfont,
const char *font_file_path, const FAPI_font_scale *font_scale,
const char *xlatmap, int BBox[4], const char **decodingID)
{
int code, bbox_set = 0, subfont = 0;
FAPI_font ff = ff_stub;
ref *SubfontId;
if (dict_find_string(pdr, "SubfontId", &SubfontId) >= 0 && r_has_type(SubfontId, t_integer))
subfont = SubfontId->value.intval;
ff.font_file_path = font_file_path;
ff.is_type1 = IsType1GlyphData(pbfont);
ff.memory = imemory;
ff.client_ctx_p = i_ctx_p;
ff.client_font_data = pbfont;
ff.client_font_data2 = pdr;
ff.server_font_data = pbfont->FAPI_font_data;
ff.is_cid = IsCIDFont(pbfont);
ff.is_mtx_skipped = (get_MetricsCount(&ff) != 0);
if ((code = renderer_retcode(i_ctx_p, I, I->get_scaled_font(I, &ff, subfont,
font_scale, xlatmap, false, FAPI_TOPLEVEL_BEGIN))) < 0)
return code;
pbfont->FAPI_font_data = ff.server_font_data;
if (ff.server_font_data != 0) {
if ((code = renderer_retcode(i_ctx_p, I, I->get_font_bbox(I, &ff, BBox))) < 0) {
I->release_typeface(I, ff.server_font_data);
return code;
}
bbox_set = 1;
}
if (xlatmap != NULL && pbfont->FAPI_font_data != NULL)
if ((code = renderer_retcode(i_ctx_p, I, I->get_decodingID(I, &ff, decodingID))) < 0) {
I->release_typeface(I, pbfont->FAPI_font_data);
pbfont->FAPI_font_data = 0;
return code;
}
if (font_file_path == NULL && ff.is_type1 && ff.is_cid) {
gs_font_cid0 *pfcid = (gs_font_cid0 *)pbfont;
gs_font_type1 **FDArray = pfcid->cidata.FDArray;
int i, n = pfcid->cidata.FDArray_size;
ref *rFDArray, f;
if (dict_find_string(pdr, "FDArray", &rFDArray) <= 0 || r_type(rFDArray) != t_array)
return_error(e_invalidfont);
ff = ff_stub;
ff.is_type1 = true;
ff.memory = imemory;
ff.client_ctx_p = i_ctx_p;
for (i = 0; i < n; i++) {
gs_font_type1 *pbfont1 = FDArray[i];
int BBox_temp[4];
pbfont1->FontBBox = pbfont->FontBBox;
if(array_get(imemory, rFDArray, i, &f) < 0 || r_type(&f) != t_dictionary)
return_error(e_invalidfont);
ff.client_font_data = pbfont1;
pbfont1->FAPI = pbfont->FAPI;
ff.client_font_data2 = &f;
ff.server_font_data = pbfont1->FAPI_font_data;
ff.is_cid = true;
ff.is_mtx_skipped = (get_MetricsCount(&ff) != 0);
if ((code = renderer_retcode(i_ctx_p, I, I->get_scaled_font(I, &ff, 0,
font_scale, NULL, false, i))) < 0)
break;
pbfont1->FAPI_font_data = ff.server_font_data;
if ((code = renderer_retcode(i_ctx_p, I, I->get_font_bbox(I, &ff, BBox_temp))) < 0)
break;
}
if (i == n) {
code = renderer_retcode(i_ctx_p, I, I->get_scaled_font(I, &ff, subfont,
font_scale, NULL, false, FAPI_TOPLEVEL_COMPLETE));
if (code >= 0)
return bbox_set;
}
for (i = 0; i < n; i++) {
gs_font_type1 *pbfont1 = FDArray[i];
if (pbfont1->FAPI_font_data != NULL)
I->release_typeface(I, pbfont1->FAPI_font_data);
pbfont1->FAPI_font_data = 0;
}
if (pbfont->FAPI_font_data != NULL)
I->release_typeface(I, pbfont->FAPI_font_data);
pbfont->FAPI_font_data = 0;
return_error(e_invalidfont);
} else {
code = renderer_retcode(i_ctx_p, I, I->get_scaled_font(I, &ff, subfont,
font_scale, xlatmap, false, FAPI_TOPLEVEL_COMPLETE));
if (code < 0) {
I->release_typeface(I, pbfont->FAPI_font_data);
pbfont->FAPI_font_data = 0;
return code;
}
return bbox_set;
}
}
private int FAPI_refine_font(i_ctx_t *i_ctx_p, os_ptr op, gs_font_base *pbfont, const char *font_file_path)
{ ref *pdr = op;
double size, size1;
int BBox[4], scale;
const char *decodingID = NULL;
char *xlatmap = NULL;
FAPI_server *I = pbfont->FAPI;
FAPI_font_scale font_scale = {{1, 0, 0, 1, 0, 0}, {0, 0}, {1, 1}, true};
int code;
if (font_file_path != NULL && pbfont->FAPI_font_data == NULL)
if ((code = FAPI_get_xlatmap(i_ctx_p, &xlatmap)) < 0)
return code;
scale = 1 << I->frac_shift;
size1 = size = 1 / hypot(pbfont->FontMatrix.xx, pbfont->FontMatrix.xy);
if (size < 1000)
size = 1000;
if (size1 > 100)
size1 = (int)(size1 + 0.5);
font_scale.matrix[0] = font_scale.matrix[3] = (int)(size * scale + 0.5);
font_scale.HWResolution[0] = (FracInt)(72 * scale);
font_scale.HWResolution[1] = (FracInt)(72 * scale);
code = FAPI_prepare_font(i_ctx_p, I, pdr, pbfont, font_file_path, &font_scale, xlatmap, BBox, &decodingID);
if (code < 0)
return code;
if (code > 0) {
ref *v, x0, y0, x1, y1;
pbfont->FontBBox.p.x = (double)BBox[0] * size1 / size;
pbfont->FontBBox.p.y = (double)BBox[1] * size1 / size;
pbfont->FontBBox.q.x = (double)BBox[2] * size1 / size;
pbfont->FontBBox.q.y = (double)BBox[3] * size1 / size;
if (dict_find_string(op, "FontBBox", &v) <= 0 || !r_has_type(v, t_array))
return_error(e_invalidfont);
if (r_size(v) < 4)
return_error(e_invalidfont);
make_real(&x0, pbfont->FontBBox.p.x);
make_real(&y0, pbfont->FontBBox.p.y);
make_real(&x1, pbfont->FontBBox.q.x);
make_real(&y1, pbfont->FontBBox.q.y);
ref_assign_old(v, v->value.refs + 0, &x0, "FAPI_refine_font_BBox");
ref_assign_old(v, v->value.refs + 1, &y0, "FAPI_refine_font_BBox");
ref_assign_old(v, v->value.refs + 2, &x1, "FAPI_refine_font_BBox");
ref_assign_old(v, v->value.refs + 3, &y1, "FAPI_refine_font_BBox");
}
if (decodingID != 0 && *decodingID) {
ref Decoding;
if (IsCIDFont(pbfont)) {
ref *CIDSystemInfo, *Ordering, SubstNWP;
byte buf[30];
int ordering_length, decodingID_length = min(strlen(decodingID), sizeof(buf) - 2);
if (dict_find_string(pdr, "CIDSystemInfo", &CIDSystemInfo) <= 0 || !r_has_type(CIDSystemInfo, t_dictionary))
return_error(e_invalidfont);
if (dict_find_string(CIDSystemInfo, "Ordering", &Ordering) <= 0 || !r_has_type(Ordering, t_string))
return_error(e_invalidfont);
ordering_length = min(r_size(Ordering), sizeof(buf) - 2 - decodingID_length);
memcpy(buf, Ordering->value.const_bytes, ordering_length);
if ((code = name_ref(imemory, buf, ordering_length, &SubstNWP, 0)) < 0)
return code;
if ((code = dict_put_string(pdr, "SubstNWP", &SubstNWP, NULL)) < 0)
return code;
buf[ordering_length] = '.';
memcpy(buf + ordering_length + 1, decodingID, decodingID_length);
buf[decodingID_length + 1 + ordering_length] = 0;
if ((code = name_ref(imemory, buf,
decodingID_length + 1 + ordering_length, &Decoding, 0)) < 0)
return code;
} else
if ((code = name_ref(imemory, (const byte *)decodingID,
strlen(decodingID), &Decoding, 0)) < 0)
return code;
if ((code = dict_put_string(pdr, "Decoding", &Decoding, NULL)) < 0)
return code;
}
return 0;
}
private int notify_remove_font(void *proc_data, void *event_data)
{
if (event_data == NULL) {
gs_font_base *pbfont = proc_data;
if (pbfont->FAPI_font_data != 0)
pbfont->FAPI->release_typeface(pbfont->FAPI, pbfont->FAPI_font_data);
}
return 0;
}
private int zFAPIrebuildfont(i_ctx_t *i_ctx_p)
{ os_ptr op = osp;
build_proc_refs build;
gs_font *pfont;
int code = font_param(op - 1, &pfont), code1;
gs_font_base *pbfont = (gs_font_base *)pfont;
ref *v;
char *font_file_path = NULL, FAPI_ID[20];
const byte *pchars;
uint len;
font_data *pdata;
if (code < 0)
return code;
check_type(*op, t_boolean);
if (pbfont->FAPI != 0) {
} else {
if (dict_find_string(op - 1, "FAPI", &v) <= 0 || !r_has_type(v, t_name))
return_error(e_invalidfont);
obj_string_data(imemory, v, &pchars, &len);
len = min(len, sizeof(FAPI_ID) - 1);
strncpy(FAPI_ID, (const char *)pchars, len);
FAPI_ID[len] = 0;
if ((code = FAPI_find_plugin(i_ctx_p, FAPI_ID, &pbfont->FAPI)) < 0)
return code;
}
pdata = (font_data *)pfont->client_data;
if (dict_find_string(op - 1, "Path", &v) <= 0 || !r_has_type(v, t_string))
v = NULL;
if (pfont->FontType == ft_CID_encrypted && v == NULL) {
if ((code = build_proc_name_refs(imemory, &build, ".FAPIBuildGlyph9", ".FAPIBuildGlyph9")) < 0)
return code;
} else
if ((code = build_proc_name_refs(imemory, &build, ".FAPIBuildChar", ".FAPIBuildGlyph")) < 0)
return code;
if (name_index(imemory, &pdata->BuildChar) == name_index(imemory, &build.BuildChar)) {
} else {
ref_assign_new(&pdata->BuildChar, &build.BuildChar);
ref_assign_new(&pdata->BuildGlyph, &build.BuildGlyph);
if (v != NULL)
font_file_path = ref_to_string(v, imemory_global, "font file path");
code = FAPI_refine_font(i_ctx_p, op - 1, pbfont, font_file_path);
if (font_file_path != NULL)
gs_free_string(imemory_global, (byte *)font_file_path, r_size(v) + 1, "font file path");
code1 = gs_notify_register(&pfont->notify_list, notify_remove_font, pbfont);
(void)code1;
}
pop(1);
return code;
}
private ulong array_find(const gs_memory_t *mem, ref *Encoding, ref *char_name) {
ulong n = r_size(Encoding), i;
ref v;
for (i = 0; i < n; i++)
if (array_get(mem, Encoding, i, &v) < 0)
break;
else if(r_type(char_name) == r_type(&v) && char_name->value.const_pname == v.value.const_pname)
return i;
return 0;
}
private int outline_char(i_ctx_t *i_ctx_p, FAPI_server *I, int import_shift_v, gs_show_enum *penum_s, struct gx_path_s *path, bool close_path)
{ FAPI_path path_interface = path_interface_stub;
FAPI_outline_handler olh;
int code;
olh.path = path;
olh.x0 = penum_s->pgs->ctm.tx_fixed;
olh.y0 = penum_s->pgs->ctm.ty_fixed;
olh.close_path = close_path;
olh.need_close = false;
path_interface.olh = &olh;
path_interface.shift = import_shift_v;
if ((code = renderer_retcode(i_ctx_p, I, I->get_char_outline(I, &path_interface))) < 0)
return code;
if (olh.need_close && olh.close_path)
if ((code = add_closepath(&path_interface)) < 0)
return code;
return 0;
}
private void compute_em_scale(const gs_font_base *pbfont, FAPI_metrics *metrics, double FontMatrix_div, double *em_scale_x, double *em_scale_y)
{
gs_matrix *m = &pbfont->base->FontMatrix;
int rounding_x, rounding_y;
double sx, sy;
sx = hypot(m->xx, m->xy) * metrics->em_x / FontMatrix_div;
sy = hypot(m->yx, m->yy) * metrics->em_y / FontMatrix_div;
rounding_x = (int)(0x00800000 / sx);
rounding_y = (int)(0x00800000 / sy);
*em_scale_x = (int)(sx * rounding_x + 0.5) / (double)rounding_x;
*em_scale_y = (int)(sy * rounding_y + 0.5) / (double)rounding_y;
}
private int fapi_copy_mono(gx_device *dev1, FAPI_raster *rast, int dx, int dy)
{ if ((rast->line_step & (align_bitmap_mod - 1)) == 0)
return dev_proc(dev1, copy_mono)(dev1, rast->p, 0, rast->line_step, 0, dx, dy, rast->width, rast->height, 0, 1);
else {
int line_step = bitmap_raster(rast->width), code;
byte *p = gs_alloc_byte_array(dev1->memory, rast->height, line_step, "fapi_copy_mono");
byte *q = p, *r = rast->p, *pe;
if (p == NULL)
return_error(e_VMerror);
pe = p + rast->height * line_step;
for (; q < pe; q+=line_step, r += rast->line_step)
memcpy(q, r, rast->line_step);
code = dev_proc(dev1, copy_mono)(dev1, p, 0, line_step, 0, dx, dy, rast->width, rast->height, 0, 1);
gs_free_object(dev1->memory, p, "fapi_copy_mono");
return code;
}
}
private const int frac_pixel_shift = 4;
private int fapi_finish_render_aux(i_ctx_t *i_ctx_p, gs_font_base *pbfont, FAPI_server *I)
{ gs_text_enum_t *penum = op_show_find(i_ctx_p);
gs_show_enum *penum_s = (gs_show_enum *)penum;
gs_state *pgs = penum_s->pgs;
gx_device *dev1 = gs_currentdevice_inline(pgs);
gx_device *dev = penum_s->dev;
const int import_shift_v = _fixed_shift - I->frac_shift;
FAPI_raster rast;
int code;
if (SHOW_IS(penum, TEXT_DO_NONE)) {
} else if (igs->in_charpath) {
if ((code = outline_char(i_ctx_p, I, import_shift_v, penum_s, pgs->show_gstate->path, !pbfont->PaintType)) < 0)
return code;
} else {
int code = I->get_char_raster(I, &rast);
if (code == e_limitcheck) {
gs_imager_state *pis = (gs_imager_state *)pgs->show_gstate;
gs_point pt;
if ((code = gs_currentpoint(pgs->show_gstate, &pt)) < 0)
return code;
if ((code = outline_char(i_ctx_p, I, import_shift_v, penum_s, pgs->show_gstate->path, !pbfont->PaintType)) < 0)
return code;
if ((code = gs_imager_setflat(pis, gs_char_flatness(pis, 1.0))) < 0)
return code;
if (pbfont->PaintType) {
if ((code = gs_stroke(pgs->show_gstate)) < 0)
return code;
} else
if ((code = gs_fill(pgs->show_gstate)) < 0)
return code;
if ((code = gs_moveto(pgs->show_gstate, pt.x, pt.y)) < 0)
return code;
} else {
int rast_orig_x = rast.orig_x - (int)(penum_s->fapi_glyph_shift.x * (1 << frac_pixel_shift));
int rast_orig_y = - rast.orig_y - (int)(penum_s->fapi_glyph_shift.y * (1 << frac_pixel_shift));
if ((code = renderer_retcode(i_ctx_p, I, code)) < 0)
return code;
if (pgs->in_cachedevice == CACHE_DEVICE_CACHING) {
int shift_rd = _fixed_shift - frac_pixel_shift;
int rounding = 1 << (frac_pixel_shift - 1);
int dx = arith_rshift_slow((pgs->ctm.tx_fixed >> shift_rd) + rast_orig_x + rounding, frac_pixel_shift);
int dy = arith_rshift_slow((pgs->ctm.ty_fixed >> shift_rd) + rast_orig_y + rounding, frac_pixel_shift);
if ((code = fapi_copy_mono(dev1, &rast, dx, dy)) < 0)
return code;
} else {
const gx_clip_path * pcpath = i_ctx_p->pgs->clip_path;
const gx_drawing_color * pdcolor = penum->pdcolor;
if ((code = dev_proc(dev, fill_mask)(dev, rast.p, 0, rast.line_step, 0,
(int)(penum_s->pgs->ctm.tx + (double)rast_orig_x / (1 << frac_pixel_shift) + 0.5),
(int)(penum_s->pgs->ctm.ty + (double)rast_orig_y / (1 << frac_pixel_shift) + 0.5),
rast.width, rast.height,
pdcolor, 1, rop3_default, pcpath)) < 0)
return code;
}
}
}
pop(2);
return 0;
}
private int fapi_finish_render(i_ctx_t *i_ctx_p)
{ os_ptr op = osp;
gs_font *pfont;
int code = font_param(op - 1, &pfont);
if (code == 0) {
gs_font_base *pbfont = (gs_font_base *) pfont;
FAPI_server *I = pbfont->FAPI;
code = fapi_finish_render_aux(i_ctx_p, pbfont, I);
I->release_char_data(I);
}
return code;
}
#define GET_U16_MSB(p) (((uint)((p)[0]) << 8) + (p)[1])
#define GET_S16_MSB(p) (int)((GET_U16_MSB(p) ^ 0x8000) - 0x8000)
private int FAPI_do_char(i_ctx_t *i_ctx_p, gs_font_base *pbfont, gx_device *dev, char *font_file_path, bool bBuildGlyph, ref *charstring)
{
os_ptr op = osp;
ref *pdr = op - 1;
gs_text_enum_t *penum = op_show_find(i_ctx_p);
gs_show_enum *penum_s = (gs_show_enum *)penum;
FAPI_char_ref cr = {0, false, NULL, 0, 0, 0, 0, 0, FAPI_METRICS_NOTDEF};
const gs_matrix * ctm = &ctm_only(igs);
int scale;
FAPI_font_scale font_scale = {{1, 0, 0, 1, 0, 0}, {0, 0}, {1, 1}, true};
FAPI_metrics metrics;
FAPI_server *I = pbfont->FAPI;
int client_char_code = 0;
ref char_name, *SubfontId;
int subfont = 0;
bool is_TT_from_type42 = (pbfont->FontType == ft_TrueType && font_file_path == NULL);
bool is_embedded_type1 = ((pbfont->FontType == ft_encrypted ||
pbfont->FontType == ft_encrypted2) &&
font_file_path == NULL);
bool bCID = (IsCIDFont(pbfont) || charstring != NULL);
bool bIsType1GlyphData = IsType1GlyphData(pbfont);
FAPI_font ff = ff_stub;
gs_log2_scale_point log2_scale = {0, 0};
int alpha_bits = (*dev_proc(dev, get_alpha_bits)) (dev, go_text);
double FontMatrix_div = (bCID && bIsType1GlyphData && font_file_path == NULL ? 1000 : 1);
bool bVertical = (gs_rootfont(igs)->WMode != 0), bVertical0 = bVertical;
double sbw[4] = {0, 0, 0, 0};
double em_scale_x, em_scale_y;
gs_rect char_bbox;
op_proc_t exec_cont = 0;
int code;
enum {
SBW_DONE,
SBW_SCALE,
SBW_FROM_RENDERER
} sbw_state = SBW_SCALE;
if(bBuildGlyph && !bCID) {
check_type(*op, t_name);
} else
check_type(*op, t_integer);
if (!SHOW_IS(penum, TEXT_DO_NONE) && !igs->in_charpath) {
gs_currentcharmatrix(igs, NULL, 1);
penum_s->fapi_log2_scale.x = -1;
gx_compute_text_oversampling(penum_s, (gs_font *)pbfont, alpha_bits, &log2_scale);
penum_s->fapi_log2_scale = log2_scale;
}
retry_oversampling:
font_scale.subpixels[0] = 1 << log2_scale.x;
font_scale.subpixels[1] = 1 << log2_scale.y;
font_scale.align_to_pixels = gs_currentaligntopixels(pbfont->dir);
if (penum == 0)
return_error(e_undefined);
scale = 1 << I->frac_shift;
{ gs_matrix *base_font_matrix = &pbfont->base->FontMatrix;
double dx = hypot(base_font_matrix->xx, base_font_matrix->xy);
double dy = hypot(base_font_matrix->yx, base_font_matrix->yy);
font_scale.matrix[0] = (FracInt)(ctm->xx * FontMatrix_div / dx * 72 / dev->HWResolution[0] * scale);
font_scale.matrix[1] = -(FracInt)(ctm->xy * FontMatrix_div / dy * 72 / dev->HWResolution[0] * scale);
font_scale.matrix[2] = (FracInt)(ctm->yx * FontMatrix_div / dx * 72 / dev->HWResolution[1] * scale);
font_scale.matrix[3] = -(FracInt)(ctm->yy * FontMatrix_div / dy * 72 / dev->HWResolution[1] * scale);
font_scale.matrix[4] = (FracInt)(ctm->tx * FontMatrix_div / dx * 72 / dev->HWResolution[0] * scale);
font_scale.matrix[5] = (FracInt)(ctm->ty * FontMatrix_div / dy * 72 / dev->HWResolution[1] * scale);
}
font_scale.HWResolution[0] = (FracInt)((double)dev->HWResolution[0] * font_scale.subpixels[0] * scale);
font_scale.HWResolution[1] = (FracInt)((double)dev->HWResolution[1] * font_scale.subpixels[1] * scale);
if (dict_find_string(pdr, "SubfontId", &SubfontId) > 0 && r_has_type(SubfontId, t_integer))
subfont = SubfontId->value.intval;
ff.memory = pbfont->memory;
ff.font_file_path = font_file_path;
ff.client_font_data = pbfont;
ff.client_font_data2 = pdr;
ff.server_font_data = pbfont->FAPI_font_data;
ff.is_type1 = bIsType1GlyphData;
ff.is_cid = bCID;
ff.is_mtx_skipped = (get_MetricsCount(&ff) != 0);
ff.client_ctx_p = i_ctx_p;
if ((code = renderer_retcode(i_ctx_p, I, I->get_scaled_font(I, &ff, subfont, &font_scale,
NULL, bVertical,
(!bCID || (pbfont->FontType != ft_encrypted &&
pbfont->FontType != ft_encrypted2)
? FAPI_TOPLEVEL_PREPARED : FAPI_DESCENDANT_PREPARED)))) < 0)
return code;
if (bCID) {
int_param(op, 0xFFFF, &client_char_code);
make_null(&char_name);
} else if (r_has_type(op, t_integer)) {
ref *Encoding;
int_param(op, 0xFF, &client_char_code);
if (dict_find_string(pdr, "Encoding", &Encoding) > 0 &&
(r_has_type(Encoding, t_array) || r_has_type(Encoding, t_shortarray))) {
if (array_get(imemory, Encoding, client_char_code, &char_name) < 0)
if ((code = name_ref(imemory, (const byte *)".notdef", 7, &char_name, -1)) < 0)
return code;
} else
return_error(e_invalidfont);
} else
char_name = *op;
if (bCID) {
if (font_file_path != NULL) {
ref *Decoding, *SubstNWP, src_type, dst_type;
uint c;
if (dict_find_string(pdr, "Decoding", &Decoding) <= 0 || !r_has_type(Decoding, t_dictionary))
return_error(e_invalidfont);
if (dict_find_string(pdr, "SubstNWP", &SubstNWP) <= 0 || !r_has_type(SubstNWP, t_array))
return_error(e_invalidfont);
code = cid_to_TT_charcode(imemory, Decoding, NULL, SubstNWP,
client_char_code, &c, &src_type, &dst_type);
if (code < 0)
return code;
cr.char_code = c;
cr.is_glyph_index = (code == 0);
} else
cr.char_code = client_char_code;
} else if (is_TT_from_type42) {
ref *CharStrings, *glyph_index;
if (dict_find_string(pdr, "CharStrings", &CharStrings) <= 0 || !r_has_type(CharStrings, t_dictionary))
return_error(e_invalidfont);
if (dict_find(CharStrings, &char_name, &glyph_index) < 0) {
cr.char_code = 0;
if ((code = name_ref(imemory, (const byte *)".notdef", 7, &char_name, -1)) < 0)
return code;
} else if (r_has_type(glyph_index, t_integer))
cr.char_code = glyph_index->value.intval;
else
return_error(e_invalidfont);
cr.is_glyph_index = true;
} else if (is_embedded_type1) {
if (r_has_type(op, t_integer))
cr.char_code = client_char_code;
else {
ref *Encoding;
if (dict_find_string(osp - 1, "Encoding", &Encoding) > 0)
cr.char_code = (uint)array_find(imemory, Encoding, op);
else
return_error(e_invalidfont);
}
} else {
bool can_retrieve_char_by_name = false;
obj_string_data(imemory, &char_name, &cr.char_name, &cr.char_name_length);
if ((code = renderer_retcode(i_ctx_p, I, I->can_retrieve_char_by_name(I, &ff, &cr, &can_retrieve_char_by_name))) < 0)
return code;
if (!can_retrieve_char_by_name) {
ref *Decoding, *char_code;
if (dict_find_string(osp - 1, "Decoding", &Decoding) > 0 && r_has_type(Decoding, t_dictionary)) {
if (dict_find(Decoding, &char_name, &char_code) >= 0 && r_has_type(char_code, t_integer))
int_param(char_code, 0xFFFF, &cr.char_code);
}
}
}
if (!ff.is_cid) {
ref sname;
name_string_ref(imemory, &char_name, &sname);
ff.char_data = sname.value.const_bytes;
ff.char_data_len = r_size(&sname);
} else if (ff.is_type1)
ff.char_data = charstring;
if(bCID && !bIsType1GlyphData) {
gs_font_cid2 *pfcid = (gs_font_cid2 *)pbfont;
int MetricsCount = pfcid->cidata.MetricsCount;
if (MetricsCount > 0) {
const byte *data_ptr;
int l = get_GlyphDirectory_data_ptr(imemory, pdr, cr.char_code, &data_ptr);
if (MetricsCount == 2 && l >= 4) {
if (!bVertical0) {
cr.sb_x = GET_S16_MSB(data_ptr + 2) * scale;
cr.aw_x = GET_U16_MSB(data_ptr + 0) * scale;
cr.metrics_type = FAPI_METRICS_REPLACE;
}
} else if (l >= 8){
cr.sb_y = GET_S16_MSB(data_ptr + 2) * scale;
cr.aw_y = GET_U16_MSB(data_ptr + 0) * scale;
cr.sb_x = GET_S16_MSB(data_ptr + 6) * scale;
cr.aw_x = GET_U16_MSB(data_ptr + 4) * scale;
cr.metrics_type = FAPI_METRICS_REPLACE;
}
}
}
if (cr.metrics_type != FAPI_METRICS_REPLACE && bVertical) {
double pwv[4];
code = zchar_get_metrics2(pbfont, &char_name, pwv);
if (code < 0)
return code;
if (code == metricsNone) {
if (bCID) {
cr.sb_x = fapi_round(sbw[2] / 2 * scale);
cr.sb_y = fapi_round(pbfont->FontBBox.q.y * scale);
cr.aw_y = fapi_round(- pbfont->FontBBox.q.x * scale);
cr.metrics_scale = (bIsType1GlyphData ? 1000 : 1);
cr.metrics_type = FAPI_METRICS_REPLACE;
sbw[0] = sbw[2] / 2;
sbw[1] = pbfont->FontBBox.q.y;
sbw[2] = 0;
sbw[3] = - pbfont->FontBBox.q.x;
sbw_state = SBW_DONE;
} else
bVertical = false;
} else {
cr.sb_x = fapi_round(pwv[2] * scale);
cr.sb_y = fapi_round(pwv[3] * scale);
cr.aw_x = fapi_round(pwv[0] * scale);
cr.aw_y = fapi_round(pwv[1] * scale);
cr.metrics_scale = (bIsType1GlyphData ? 1000 : 1);
cr.metrics_type = (code == metricsSideBearingAndWidth ?
FAPI_METRICS_REPLACE : FAPI_METRICS_REPLACE_WIDTH);
sbw[0] = pwv[2];
sbw[1] = pwv[3];
sbw[2] = pwv[0];
sbw[3] = pwv[1];
sbw_state = SBW_DONE;
}
}
if (cr.metrics_type == FAPI_METRICS_NOTDEF && !bVertical) {
code = zchar_get_metrics(pbfont, &char_name, sbw);
if (code < 0)
return code;
if (code == metricsNone) {
sbw_state = SBW_FROM_RENDERER;
if (pbfont->FontType == 2) {
gs_font_type1 *pfont1 = (gs_font_type1 *)pbfont;
cr.aw_x = export_shift(pfont1->data.defaultWidthX, _fixed_shift - I->frac_shift);
cr.metrics_scale = 1000;
cr.metrics_type = FAPI_METRICS_ADD;
}
} else {
cr.sb_x = fapi_round(sbw[2] * scale);
cr.sb_y = fapi_round(sbw[3] * scale);
cr.aw_x = fapi_round(sbw[0] * scale);
cr.aw_y = fapi_round(sbw[1] * scale);
cr.metrics_scale = (bIsType1GlyphData ? 1000 : 1);
cr.metrics_type = (code == metricsSideBearingAndWidth ?
FAPI_METRICS_REPLACE : FAPI_METRICS_REPLACE_WIDTH);
sbw_state = SBW_DONE;
}
}
if (SHOW_IS(penum, TEXT_DO_NONE)) {
if ((code = renderer_retcode(i_ctx_p, I, I->get_char_width(I, &ff, &cr, &metrics))) < 0)
return code;
} else if (igs->in_charpath) {
if ((code = renderer_retcode(i_ctx_p, I, I->get_char_outline_metrics(I, &ff, &cr, &metrics))) < 0)
return code;
} else {
code = I->get_char_raster_metrics(I, &ff, &cr, &metrics);
if (code == e_limitcheck) {
if(log2_scale.x > 0 || log2_scale.y > 0) {
penum_s->fapi_log2_scale.x = log2_scale.x = penum_s->fapi_log2_scale.y = log2_scale.y = 0;
I->release_char_data(I);
goto retry_oversampling;
}
if ((code = renderer_retcode(i_ctx_p, I, I->get_char_outline_metrics(I, &ff, &cr, &metrics))) < 0)
return code;
} else {
if ((code = renderer_retcode(i_ctx_p, I, code)) < 0)
return code;
}
}
compute_em_scale(pbfont, &metrics, FontMatrix_div, &em_scale_x, &em_scale_y);
char_bbox.p.x = metrics.bbox_x0 / em_scale_x;
char_bbox.p.y = metrics.bbox_y0 / em_scale_y;
char_bbox.q.x = metrics.bbox_x1 / em_scale_x;
char_bbox.q.y = metrics.bbox_y1 / em_scale_y;
penum_s->fapi_glyph_shift.x = penum_s->fapi_glyph_shift.y = 0;
if (sbw_state == SBW_FROM_RENDERER) {
sbw[2] = metrics.escapement / em_scale_x;
if (pbfont->FontType == 2) {
gs_font_type1 *pfont1 = (gs_font_type1 *)pbfont;
sbw[2] += fixed2float(pfont1->data.defaultWidthX);
}
} else if (sbw_state == SBW_SCALE) {
sbw[0] = (double)cr.sb_x / scale / em_scale_x;
sbw[1] = (double)cr.sb_y / scale / em_scale_y;
sbw[2] = (double)cr.aw_x / scale / em_scale_x;
sbw[3] = (double)cr.aw_y / scale / em_scale_y;
}
if (cr.metrics_type == FAPI_METRICS_REPLACE) {
int can_replace_metrics;
if ((code = renderer_retcode(i_ctx_p, I, I->can_replace_metrics(I, &ff, &cr, &can_replace_metrics))) < 0)
return code;
if (!can_replace_metrics) {
char_bbox.q.x -= char_bbox.p.x;
char_bbox.p.x = 0;
gs_distance_transform((metrics.bbox_x0 / em_scale_x - sbw[0]),
0, ctm, &penum_s->fapi_glyph_shift);
penum_s->fapi_glyph_shift.x *= font_scale.subpixels[0];
penum_s->fapi_glyph_shift.y *= font_scale.subpixels[1];
}
}
code = zchar_set_cache(i_ctx_p, pbfont, &char_name,
NULL, sbw + 2, &char_bbox,
fapi_finish_render, &exec_cont, sbw);
if (code >= 0 && exec_cont != 0)
code = (*exec_cont)(i_ctx_p);
if (code != 0) {
if (code < 0) {
I->release_char_data(I);
} else {
}
}
return code;
}
private int FAPI_char(i_ctx_t *i_ctx_p, bool bBuildGlyph, ref *charstring)
{
ref *v;
char *font_file_path = NULL;
gx_device *dev = gs_currentdevice_inline(igs);
gs_font *pfont;
int code = font_param(osp - 1, &pfont);
if (code == 0) {
gs_font_base *pbfont = (gs_font_base *) pfont;
if (dict_find_string(osp - 1, "Path", &v) > 0 && r_has_type(v, t_string))
font_file_path = ref_to_string(v, imemory, "font file path");
code = FAPI_do_char(i_ctx_p, pbfont, dev, font_file_path, bBuildGlyph, charstring);
if (font_file_path != NULL)
gs_free_string(imemory, (byte *)font_file_path, r_size(v) + 1, "font file path");
}
return code;
}
private int FAPIBuildGlyph9aux(i_ctx_t *i_ctx_p)
{ os_ptr op = osp;
ref font9 = *pfont_dict(gs_currentfont(igs));
ref *rFDArray, f;
int font_index;
int code;
if ((code = ztype9mapcid(i_ctx_p)) < 0)
return code;
font_index = op[0].value.intval;
if (dict_find_string(&font9, "FDArray", &rFDArray) <= 0 || r_type(rFDArray) != t_array)
return_error(e_invalidfont);
if(array_get(imemory, rFDArray, font_index, &f) < 0 || r_type(&f) != t_dictionary)
return_error(e_invalidfont);
op[0] = op[-2];
op[-2] = op[-1];
op[-1] = f;
if ((code = FAPI_char(i_ctx_p, true, op - 2)) < 0)
return code;
return 0;
}
private int zFAPIBuildChar(i_ctx_t *i_ctx_p)
{ return FAPI_char(i_ctx_p, false, NULL);
}
private int zFAPIBuildGlyph(i_ctx_t *i_ctx_p)
{ return FAPI_char(i_ctx_p, true, NULL);
}
private int zFAPIBuildGlyph9(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int cid, code;
avm_space s = ialloc_space(idmemory);
check_type(op[ 0], t_integer);
check_type(op[-1], t_dictionary);
cid = op[0].value.intval;
push(2);
op[-1] = *pfont_dict(gs_currentfont(igs));
op[0] = op[-2];
ialloc_set_space(idmemory, (r_is_local(op - 3) ? avm_global : avm_local));
code = FAPIBuildGlyph9aux(i_ctx_p);
if (code != 0) {
make_int(op - 2, cid);
pop(2);
} else {
pop(2);
}
ialloc_set_space(idmemory, s);
return code;
}
private int do_FAPIpassfont(i_ctx_t *i_ctx_p, char *font_file_path, bool *success)
{ ref *pdr = osp;
gs_font *pfont;
int code = font_param(osp, &pfont);
gs_font_base *pbfont;
int BBox[4];
i_plugin_holder *h = i_plugin_get_list(i_ctx_p);
char *xlatmap = NULL;
FAPI_font_scale font_scale = {{1, 0, 0, 1, 0, 0}, {0, 0}, {1, 1}, true};
const char *decodingID = NULL;
if (code < 0)
return code;
code = FAPI_get_xlatmap(i_ctx_p, &xlatmap);
if (code < 0)
return code;
pbfont = (gs_font_base *)pfont;
*success = false;
for (; h != 0; h = h->next) {
ref FAPI_ID;
FAPI_server *I;
if (strcmp(h->I->d->type, "FAPI"))
continue;
I = (FAPI_server *)h->I;
if ((code = renderer_retcode(i_ctx_p, I, I->ensure_open(I))) < 0)
return code;
font_scale.HWResolution[0] = font_scale.HWResolution[1] = 72 << I->frac_shift;
font_scale.matrix[0] = font_scale.matrix[3] = 1 << I->frac_shift;
code = FAPI_prepare_font(i_ctx_p, I, pdr, pbfont, font_file_path, &font_scale, xlatmap, BBox, &decodingID);
if (code < 0) {
continue;
}
pbfont->FAPI = I;
if ((code = name_ref(imemory, (const byte *)I->ig.d->subtype, strlen(I->ig.d->subtype), &FAPI_ID, false)) < 0)
return code;
if ((code = dict_put_string(pdr, "FAPI", &FAPI_ID, NULL)) < 0)
return code;
*success = true;
return 0;
}
return 0;
}
private int zFAPIpassfont(i_ctx_t *i_ctx_p)
{ os_ptr op = osp;
int code;
bool found;
char *font_file_path = NULL;
ref *v;
check_type(*op, t_dictionary);
if (dict_find_string(op, "Path", &v) > 0 && r_has_type(v, t_string))
font_file_path = ref_to_string(v, imemory_global, "font file path");
code = do_FAPIpassfont(i_ctx_p, font_file_path, &found);
if (font_file_path != NULL)
gs_free_string(imemory_global, (byte *)font_file_path, r_size(v) + 1, "font file path");
if(code != 0)
return code;
push(1);
make_bool(op, found);
return 0;
}
const op_def zfapi_op_defs[] =
{ {"2.FAPIavailable", zFAPIavailable},
{"2.FAPIpassfont", zFAPIpassfont},
{"2.FAPIrebuildfont", zFAPIrebuildfont},
{"2.FAPIBuildChar", zFAPIBuildChar},
{"2.FAPIBuildGlyph", zFAPIBuildGlyph},
{"2.FAPIBuildGlyph9", zFAPIBuildGlyph9},
op_def_end(0)
};