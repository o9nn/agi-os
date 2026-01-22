#include "memory_.h"
#include "gx.h"
#include "gsdsrc.h"
#include "gserrors.h"
#include "stream.h"
public_st_data_source();
private
ENUM_PTRS_WITH(data_source_enum_ptrs, gs_data_source_t *psrc)
{
if (psrc->type == data_source_type_string)
ENUM_RETURN_CONST_STRING_PTR(gs_data_source_t, data.str);
else if (psrc->type == data_source_type_stream)
ENUM_RETURN_PTR(gs_data_source_t, data.strm);
else
ENUM_RETURN_PTR(gs_data_source_t, data.str.data);
}
ENUM_PTRS_END
private RELOC_PTRS_WITH(data_source_reloc_ptrs, gs_data_source_t *psrc)
{
if (psrc->type == data_source_type_string)
RELOC_CONST_STRING_PTR(gs_data_source_t, data.str);
else if (psrc->type == data_source_type_stream)
RELOC_PTR(gs_data_source_t, data.strm);
else
RELOC_PTR(gs_data_source_t, data.str.data);
}
RELOC_PTRS_END
int
data_source_access_string(const gs_data_source_t * psrc, ulong start,
uint length, byte * buf, const byte ** ptr)
{
const byte *p = psrc->data.str.data + start;
if (ptr)
*ptr = p;
else
memcpy(buf, p, length);
return 0;
}
int
data_source_access_bytes(const gs_data_source_t * psrc, ulong start,
uint length, byte * buf, const byte ** ptr)
{
const byte *p = psrc->data.str.data + start;
if (ptr)
*ptr = p;
else
memcpy(buf, p, length);
return 0;
}
int
data_source_access_stream(const gs_data_source_t * psrc, ulong start,
uint length, byte * buf, const byte ** ptr)
{
stream *s = psrc->data.strm;
const byte *p;
if (start >= s->position &&
(p = start - s->position + s->cbuf) + length <=
s->cursor.r.limit + 1
) {
if (ptr)
*ptr = p;
else
memcpy(buf, p, length);
} else {
uint nread;
int code = sseek(s, start);
if (code < 0)
return_error(gs_error_rangecheck);
code = sgets(s, buf, length, &nread);
if (code < 0)
return_error(gs_error_rangecheck);
if (nread != length)
return_error(gs_error_rangecheck);
if (ptr)
*ptr = buf;
}
return 0;
}