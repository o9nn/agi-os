#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gsparams.h"
typedef struct {
byte *buf;
byte *buf_end;
unsigned total_sizeof;
} WriteBuffer;
private void
ptr_align_to(
const byte ** src,
unsigned alignment
);
private void
wb_put_word(
unsigned source,
WriteBuffer * dest
);
private void
wb_put_bytes(
const byte * source,
unsigned source_sizeof,
WriteBuffer * dest
);
private void
wb_put_alignment(
unsigned alignment,
WriteBuffer * dest
);
private unsigned
buf_get_word(
const byte ** src
);
int
gs_param_list_serialize(
gs_param_list * list,
byte * buf,
int buf_sizeof
)
{
int code = 0;
int temp_code;
gs_param_enumerator_t key_enum;
gs_param_key_t key;
WriteBuffer write_buf;
write_buf.buf = buf;
write_buf.buf_end = buf + (buf ? buf_sizeof : 0);
write_buf.total_sizeof = 0;
param_init_enumerator(&key_enum);
while ((code = param_get_next_key(list, &key_enum, &key)) == 0) {
int value_top_sizeof;
int value_base_sizeof;
gs_param_typed_value value;
char string_key[256];
if (sizeof(string_key) < key.size + 1) {
code = gs_note_error(gs_error_rangecheck);
break;
}
memcpy(string_key, key.data, key.size);
string_key[key.size] = 0;
if ((code = param_read_typed(list, string_key, &value)) != 0) {
code = code > 0 ? gs_note_error(gs_error_unknownerror) : code;
break;
}
wb_put_word((unsigned)key.size + 1, &write_buf);
wb_put_word((unsigned)value.type, &write_buf);
wb_put_bytes((byte *) string_key, key.size + 1, &write_buf);
value_top_sizeof = gs_param_type_sizes[value.type];
value_base_sizeof = gs_param_type_base_sizes[value.type];
switch (value.type) {
case gs_param_type_null:
case gs_param_type_bool:
case gs_param_type_int:
case gs_param_type_long:
case gs_param_type_float:
wb_put_bytes((byte *) & value.value, value_top_sizeof, &write_buf);
break;
case gs_param_type_string:
case gs_param_type_name:
case gs_param_type_int_array:
case gs_param_type_float_array:
wb_put_bytes((byte *) & value.value, value_top_sizeof, &write_buf);
wb_put_alignment(value_base_sizeof, &write_buf);
value_base_sizeof *= value.value.s.size;
wb_put_bytes(value.value.s.data, value_base_sizeof, &write_buf);
break;
case gs_param_type_string_array:
case gs_param_type_name_array:
value_base_sizeof *= value.value.sa.size;
wb_put_bytes((const byte *)&value.value, value_top_sizeof, &write_buf);
wb_put_alignment(sizeof(void *), &write_buf);
wb_put_bytes((const byte *)value.value.sa.data, value_base_sizeof,
&write_buf);
{
int str_count;
const gs_param_string *sa;
for (str_count = value.value.sa.size,
sa = value.value.sa.data; str_count-- > 0; ++sa)
wb_put_bytes(sa->data, sa->size, &write_buf);
}
break;
case gs_param_type_dict:
case gs_param_type_dict_int_keys:
wb_put_word(value.value.d.size, &write_buf);
wb_put_alignment(sizeof(void *), &write_buf);
{
int bytes_written =
gs_param_list_serialize(value.value.d.list,
write_buf.buf,
write_buf.buf ? write_buf.buf_end - write_buf.buf : 0);
temp_code = param_end_read_dict(list,
(const char *)key.data,
&value.value.d);
if (bytes_written < 0)
code = bytes_written;
else {
code = temp_code;
if (bytes_written)
wb_put_bytes(write_buf.buf, bytes_written, &write_buf);
}
}
break;
default:
code = gs_note_error(gs_error_unknownerror);
break;
}
if (code < 0)
break;
}
if (code >= 0) {
wb_put_word(0, &write_buf);
code = write_buf.total_sizeof;
}
return code;
}
int
gs_param_list_unserialize(
gs_param_list * list,
const byte * buf
)
{
int code = 0;
const byte *orig_buf = buf;
do {
gs_param_typed_value typed;
gs_param_name key;
unsigned key_sizeof;
int value_top_sizeof;
int value_base_sizeof;
int temp_code;
gs_param_type type;
key_sizeof = buf_get_word(&buf);
if (key_sizeof == 0)
break;
type = (gs_param_type) buf_get_word(&buf);
key = (gs_param_name) buf;
buf += key_sizeof;
value_top_sizeof = gs_param_type_sizes[type];
value_base_sizeof = gs_param_type_base_sizes[type];
typed.type = type;
if (type != gs_param_type_dict && type != gs_param_type_dict_int_keys) {
memcpy(&typed.value, buf, value_top_sizeof);
buf += value_top_sizeof;
}
switch (type) {
case gs_param_type_null:
case gs_param_type_bool:
case gs_param_type_int:
case gs_param_type_long:
case gs_param_type_float:
break;
case gs_param_type_string:
case gs_param_type_name:
case gs_param_type_int_array:
case gs_param_type_float_array:
ptr_align_to(&buf, value_base_sizeof);
typed.value.s.data = buf;
typed.value.s.persistent = false;
buf += typed.value.s.size * value_base_sizeof;
break;
case gs_param_type_string_array:
case gs_param_type_name_array:
ptr_align_to(&buf, sizeof(void *));
typed.value.sa.data = (const gs_param_string *)buf;
typed.value.sa.persistent = false;
buf += typed.value.s.size * value_base_sizeof;
{
int str_count;
gs_param_string *sa;
for (str_count = typed.value.sa.size,
sa = (gs_param_string *) typed.value.sa.data;
str_count-- > 0; ++sa) {
sa->data = buf;
sa->persistent = false;
buf += sa->size;
}
}
break;
case gs_param_type_dict:
case gs_param_type_dict_int_keys:
typed.value.d.size = buf_get_word(&buf);
code = param_begin_write_dict
(list, key, &typed.value.d, type == gs_param_type_dict_int_keys);
if (code < 0)
break;
ptr_align_to(&buf, sizeof(void *));
code = gs_param_list_unserialize(typed.value.d.list, buf);
temp_code = param_end_write_dict(list, key, &typed.value.d);
if (code >= 0) {
buf += code;
code = temp_code;
}
break;
default:
code = gs_note_error(gs_error_unknownerror);
break;
}
if (code < 0)
break;
if (typed.type != gs_param_type_dict && typed.type != gs_param_type_dict_int_keys)
code = param_write_typed(list, key, &typed);
}
while (code >= 0);
return code >= 0 ? buf - orig_buf : code;
}
private void
ptr_align_to(
const byte ** src,
unsigned alignment
)
{
*src += -(int)ALIGNMENT_MOD(*src, alignment) & (alignment - 1);
}
private void
wb_put_word(
unsigned source,
WriteBuffer * dest
)
{
do {
byte chunk = source & 0x7f;
if (source >= 0x80)
chunk |= 0x80;
source >>= 7;
++dest->total_sizeof;
if (dest->buf && dest->buf < dest->buf_end)
*dest->buf++ = chunk;
}
while (source != 0);
}
private void
wb_put_bytes(
const byte * source,
unsigned source_sizeof,
WriteBuffer * dest
)
{
dest->total_sizeof += source_sizeof;
if (dest->buf && dest->buf + source_sizeof <= dest->buf_end) {
if (dest->buf != source)
memcpy(dest->buf, source, source_sizeof);
dest->buf += source_sizeof;
}
}
private void
wb_put_alignment(
unsigned alignment,
WriteBuffer * dest
)
{
static const byte zero =
{0};
while ((dest->total_sizeof & (alignment - 1)) != 0)
wb_put_bytes(&zero, 1, dest);
}
private unsigned
buf_get_word(
const byte ** src
)
{
unsigned dest = 0;
byte chunk;
unsigned shift = 0;
do {
chunk = *(*src)++;
dest |= (chunk & 0x7f) << shift;
shift += 7;
}
while (chunk & 0x80);
return dest;
}