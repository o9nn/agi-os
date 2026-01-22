#include "memory_.h"
#include "gx.h"
#include "gsgdata.h"
#include "gserrors.h"
#include "gsmatrix.h"
#include "gsstruct.h"
#include "gxfont.h"
private ENUM_PTRS_WITH(gs_glyph_data_enum_ptrs, gs_glyph_data_t *pgd)
case 0: return ENUM_CONST_BYTESTRING(&pgd->bits);
case 1: return ENUM_OBJ(pgd->proc_data);
ENUM_PTRS_END
private RELOC_PTRS_WITH(gs_glyph_data_reloc_ptrs, gs_glyph_data_t *pgd)
{
RELOC_CONST_BYTESTRING_VAR(pgd->bits);
RELOC_OBJ_VAR(pgd->proc_data);
} RELOC_PTRS_END
gs_public_st_composite(st_glyph_data, gs_glyph_data_t, "gs_glyph_data_t",
gs_glyph_data_enum_ptrs, gs_glyph_data_reloc_ptrs);
int
gs_glyph_data_substring(gs_glyph_data_t *pgd, uint offset, uint size)
{
if (offset > pgd->bits.size || size > pgd->bits.size - offset)
return_error(gs_error_rangecheck);
return pgd->procs->substring(pgd, offset, size);
}
void
gs_glyph_data_free(gs_glyph_data_t *pgd, client_name_t cname)
{
pgd->procs->free(pgd, cname);
gs_glyph_data_from_null(pgd);
}
private void
glyph_data_free_permanent(gs_glyph_data_t *pgd, client_name_t cname)
{
}
private int
glyph_data_substring_permanent(gs_glyph_data_t *pgd, uint offset, uint size)
{
pgd->bits.data += offset;
pgd->bits.size = size;
return 0;
}
private void
glyph_data_free_by_font(gs_glyph_data_t *pgd, client_name_t cname)
{
gs_free_const_bytestring(((gs_font *)pgd->proc_data)->memory,
&pgd->bits, cname);
}
private int
glyph_data_substring_by_font(gs_glyph_data_t *pgd, uint offset, uint size)
{
gs_font *const font = pgd->proc_data;
byte *data = (byte *)pgd->bits.data;
if (pgd->bits.bytes)
return glyph_data_substring_permanent(pgd, offset, size);
if (offset > 0)
memmove(data, data + offset, size);
pgd->bits.data =
gs_resize_string(font->memory, data, pgd->bits.size, size,
"glyph_data_substring");
pgd->bits.size = size;
return 0;
}
private const gs_glyph_data_procs_t no_free_procs = {
glyph_data_free_permanent, glyph_data_substring_permanent
};
private const gs_glyph_data_procs_t free_by_font_procs = {
glyph_data_free_by_font, glyph_data_substring_by_font
};
void
gs_glyph_data_from_string(gs_glyph_data_t *pgd, const byte *data,
uint size, gs_font *font)
{
gs_bytestring_from_string(&pgd->bits, data, size);
pgd->proc_data = font;
pgd->procs = (font ? &free_by_font_procs : &no_free_procs);
}
void
gs_glyph_data_from_bytes(gs_glyph_data_t *pgd, const byte *bytes,
uint offset, uint size, gs_font *font)
{
gs_bytestring_from_bytes(&pgd->bits, bytes, offset, size);
pgd->proc_data = font;
pgd->procs = (font ? &free_by_font_procs : &no_free_procs);
}
void
gs_glyph_data_from_null(gs_glyph_data_t *pgd)
{
gs_glyph_data_from_string(pgd, NULL, 0, NULL);
}