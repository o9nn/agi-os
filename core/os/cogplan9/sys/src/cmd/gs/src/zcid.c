#include "ghost.h"
#include "ierrors.h"
#include "gxcid.h"
#include "icid.h"
#include "idict.h"
#include "idparam.h"
#include "store.h"
#include "oper.h"
int
cid_system_info_param(gs_cid_system_info_t *pcidsi, const ref *prcidsi)
{
ref *pregistry;
ref *pordering;
int code;
if (!r_has_type(prcidsi, t_dictionary))
return_error(e_typecheck);
if (dict_find_string(prcidsi, "Registry", &pregistry) <= 0 ||
dict_find_string(prcidsi, "Ordering", &pordering) <= 0
)
return_error(e_rangecheck);
check_read_type_only(*pregistry, t_string);
check_read_type_only(*pordering, t_string);
pcidsi->Registry.data = pregistry->value.const_bytes;
pcidsi->Registry.size = r_size(pregistry);
pcidsi->Ordering.data = pordering->value.const_bytes;
pcidsi->Ordering.size = r_size(pordering);
code = dict_int_param(prcidsi, "Supplement", 0, max_int, -1,
&pcidsi->Supplement);
return (code < 0 ? code : 0);
}
private bool
TT_char_code_from_CID_no_subst(const gs_memory_t *mem,
const ref *Decoding, const ref *TT_cmap, uint nCID, uint *c)
{ ref *DecodingArray, char_code, ih, glyph_index;
make_int(&ih, nCID / 256);
if (dict_find(Decoding, &ih, &DecodingArray) <= 0 ||
!r_has_type(DecodingArray, t_array) ||
array_get(mem, DecodingArray, nCID % 256, &char_code) < 0 ||
!r_has_type(&char_code, t_integer)) {
return false;
}
if (TT_cmap == NULL) {
*c = char_code.value.intval;
return true;
}
if (array_get(mem, TT_cmap, char_code.value.intval, &glyph_index) < 0 ||
!r_has_type(&glyph_index, t_integer))
return false;
*c = glyph_index.value.intval;
return true;
}
int
cid_to_TT_charcode(const gs_memory_t *mem,
const ref *Decoding, const ref *TT_cmap, const ref *SubstNWP,
uint nCID, uint *c, ref *src_type, ref *dst_type)
{
int SubstNWP_length = r_size(SubstNWP), i, code;
if (TT_char_code_from_CID_no_subst(mem, Decoding, TT_cmap, nCID, c)) {
make_null(src_type);
return 1;
}
for (i = 0; i < SubstNWP_length; i += 5) {
ref rb, re, rs;
int nb, ne, ns;
if ((code = array_get(mem, SubstNWP, i + 1, &rb)) < 0)
return code;
if ((code = array_get(mem, SubstNWP, i + 2, &re)) < 0)
return code;
if ((code = array_get(mem, SubstNWP, i + 3, &rs)) < 0)
return code;
nb = rb.value.intval;
ne = re.value.intval;
ns = rs.value.intval;
if (nCID >= nb && nCID <= ne)
if (TT_char_code_from_CID_no_subst(mem, Decoding, TT_cmap, ns + (nCID - nb), c)) {
if ((code = array_get(mem, SubstNWP, i + 0, src_type)) < 0)
return code;
if ((code = array_get(mem, SubstNWP, i + 4, dst_type)) < 0)
return code;
return 1;
}
if (nCID >= ns && nCID <= ns + (ne - nb))
if (TT_char_code_from_CID_no_subst(mem, Decoding, TT_cmap, nb + (nCID - ns), c)) {
if ((code = array_get(mem, SubstNWP, i + 0, dst_type)) < 0)
return code;
if ((code = array_get(mem, SubstNWP, i + 4, src_type)) < 0)
return code;
return 1;
}
}
*c = 0;
return 0;
}
private int
set_CIDMap_element(const gs_memory_t *mem, ref *CIDMap, uint cid, uint glyph_index)
{
int offset = cid * 2;
int count = r_size(CIDMap), size, i;
ref s;
uchar *c;
if (glyph_index >= 65536)
return_error(e_rangecheck);
for (i = 0; i < count; i++) {
array_get(mem, CIDMap, i, &s);
size = r_size(&s) & ~1;
if (offset < size) {
c = s.value.bytes + offset;
c[0] = (uchar)(glyph_index >> 8);
c[1] = (uchar)(glyph_index & 255);
break;
}
offset -= size;
}
return 0;
}
int
cid_fill_CIDMap(const gs_memory_t *mem,
const ref *Decoding, const ref *TT_cmap, const ref *SubstNWP, int GDBytes,
ref *CIDMap)
{ int dict_enum;
ref el[2];
int count, i;
if (GDBytes != 2)
return_error(e_unregistered);
if (r_type(CIDMap) != t_array)
return_error(e_unregistered);
count = r_size(CIDMap);
for (i = 0; i < count; i++) {
ref s;
int code = array_get(mem, CIDMap, i, &s);
if (code < 0)
return code;
check_type(s, t_string);
}
dict_enum = dict_first(Decoding);
for (;;) {
int index, count, i;
if ((dict_enum = dict_next(Decoding, dict_enum, el)) == -1)
break;
if (!r_has_type(&el[0], t_integer))
continue;
if (!r_has_type(&el[1], t_array))
return_error(e_typecheck);
index = el[0].value.intval;
count = r_size(&el[1]);
for (i = 0; i < count; i++) {
uint cid = index * 256 + i, glyph_index;
ref src_type, dst_type;
int code = cid_to_TT_charcode(mem, Decoding, TT_cmap, SubstNWP,
cid, &glyph_index, &src_type, &dst_type);
if (code < 0)
return code;
if (code > 0) {
code = set_CIDMap_element(mem, CIDMap, cid, glyph_index);
if (code < 0)
return code;
}
}
}
return 0;
}