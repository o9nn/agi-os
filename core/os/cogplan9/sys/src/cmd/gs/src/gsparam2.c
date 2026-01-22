#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gsparams.h"
#define MAX_PARAM_KEY 255
private int sput_word(stream *dest, uint value);
private int sput_bytes(stream *dest, const byte *data, uint size);
int
gs_param_list_puts(stream *dest, gs_param_list *list)
{
int code = 0;
gs_param_enumerator_t key_enum;
gs_param_key_t key;
param_init_enumerator(&key_enum);
while ((code = param_get_next_key(list, &key_enum, &key)) == 0) {
int value_top_sizeof;
int value_base_sizeof;
const void *data;
uint size;
gs_param_typed_value value;
char string_key[MAX_PARAM_KEY + 1];
if (sizeof(string_key) < key.size + 1) {
code = gs_note_error(gs_error_rangecheck);
break;
}
memcpy(string_key, key.data, key.size);
string_key[key.size] = 0;
if ((code = param_read_typed(list, string_key, &value)) != 0) {
if (code > 0)
code = gs_note_error(gs_error_unknownerror);
break;
}
sput_word(dest, (unsigned)key.size + 1);
sput_word(dest, (unsigned)value.type);
sput_bytes(dest, (byte *)string_key, key.size + 1);
value_top_sizeof = gs_param_type_sizes[value.type];
value_base_sizeof = gs_param_type_base_sizes[value.type];
switch (value.type) {
case gs_param_type_bool:
case gs_param_type_int:
case gs_param_type_long:
case gs_param_type_float:
sput_bytes(dest, (byte *)&value.value, value_top_sizeof);
case gs_param_type_null:
break;
case gs_param_type_string:
data = value.value.s.data, size = value.value.s.size;
goto scalar_array;
case gs_param_type_name:
data = value.value.n.data, size = value.value.n.size;
goto scalar_array;
case gs_param_type_int_array:
data = value.value.ia.data, size = value.value.ia.size;
goto scalar_array;
case gs_param_type_float_array:
data = value.value.fa.data, size = value.value.fa.size;
scalar_array:	sput_word(dest, size);
sput_bytes(dest, data, value_base_sizeof * size);
break;
case gs_param_type_string_array:
data = value.value.sa.data, size = value.value.sa.size;
goto string_array;
case gs_param_type_name_array:
data = value.value.na.data, size = value.value.na.size;
string_array:	sput_word(dest, size);
{
uint count;
const gs_param_string *sa;
for (count = size, sa = data; count-- > 0; ++sa) {
sput_word(dest, sa->size);
sput_bytes(dest, sa->data, sa->size);
}
}
break;
case gs_param_type_dict:
case gs_param_type_dict_int_keys:
case gs_param_type_array:
sput_word(dest, value.value.d.size);
code = gs_param_list_puts(dest, value.value.d.list);
{
int end_code =
param_end_read_dict(list, key.data, &value.value.d);
if (code >= 0)
code = end_code;
}
break;
default:
code = gs_note_error(gs_error_unknownerror);
break;
}
if (code < 0)
break;
}
return (code < 0 ? code : sput_word(dest, 0));
}
private int
sput_word(stream *dest, uint value)
{
int code = 0;
do {
byte chunk = value & 0x7f;
if ((value >>= 7) != 0)
chunk |= 0x80;
if ((code = sputc(dest, chunk)) < 0)
break;
}
while (value != 0);
return code;
}
private int
sput_bytes(stream *dest, const byte *data, uint size)
{
uint ignore_count;
return sputs(dest, data, size, &ignore_count);
}
private int sget_word(stream *src, uint *pvalue);
private int sget_bytes(stream *src, byte *data, uint size);
int
gs_param_list_gets(stream *src, gs_param_list *list, gs_memory_t *mem)
{
int code = 0;
do {
gs_param_typed_value typed;
uint key_sizeof;
int value_top_sizeof;
int value_base_sizeof;
uint temp;
void *data;
uint size;
gs_param_type type;
char string_key[MAX_PARAM_KEY + 1];
if ((code = sget_word(src, &key_sizeof)) < 0 ||
key_sizeof == 0 ||
(code = sget_word(src, &temp)) < 0)
break;
if (key_sizeof > sizeof(string_key)) {
code = gs_note_error(gs_error_rangecheck);
break;
}
code = sget_bytes(src, (byte *)string_key, key_sizeof);
if (code < 0)
break;
type = (gs_param_type)temp;
value_top_sizeof = gs_param_type_sizes[type];
value_base_sizeof = gs_param_type_base_sizes[type];
typed.type = type;
switch (type) {
case gs_param_type_bool:
case gs_param_type_int:
case gs_param_type_long:
case gs_param_type_float:
code = sget_bytes(src, (byte *)&typed.value, value_top_sizeof);
case gs_param_type_null:
goto put;
default:
;
}
code = sget_word(src, &size);
if (code < 0)
break;
switch (type) {
case gs_param_type_string:
case gs_param_type_name:
case gs_param_type_int_array:
case gs_param_type_float_array:
data =
(value_base_sizeof == 1 ?
gs_alloc_string(mem, size, "param string/name") :
gs_alloc_byte_array(mem, size, value_base_sizeof,
"param scalar array"));
if (data == 0) {
code = gs_note_error(gs_error_VMerror);
break;
}
typed.value.s.data = data;
typed.value.s.persistent = false;
typed.value.s.size = size;
code = sget_bytes(src, data, size * value_base_sizeof);
break;
case gs_param_type_string_array:
case gs_param_type_name_array:
data = gs_alloc_byte_array(mem, size, value_top_sizeof,
"param string/name array");
if (data == 0) {
code = gs_note_error(gs_error_VMerror);
break;
}
typed.value.sa.data = data;
typed.value.sa.persistent = false;
typed.value.sa.size = size;
{
gs_param_string *sa = data;
byte *str_data;
uint index, str_size;
for (index = 0; index < size; ++index)
sa[index].data = 0, sa[index].size = 0;
for (index = 0; index < size; ++index, ++sa) {
code = sget_word(src, &str_size);
if (code < 0)
break;
str_data = gs_alloc_string(mem, str_size,
"param string/name element");
if (str_data == 0) {
code = gs_note_error(gs_error_VMerror);
break;
}
code = sget_bytes(src, str_data, str_size);
if (code < 0)
break;
}
}
break;
case gs_param_type_dict:
case gs_param_type_dict_int_keys:
case gs_param_type_array:
typed.value.d.size = size;
code = param_begin_write_collection
(list, string_key, &typed.value.d,
type - gs_param_type_dict);
if (code < 0)
break;
code = gs_param_list_gets(src, typed.value.d.list, mem);
{
int end_code =
param_end_write_collection(list, string_key,
&typed.value.d);
if (code >= 0)
code = end_code;
}
break;
default:
code = gs_note_error(gs_error_unknownerror);
break;
}
put:	if (code < 0)
break;
if (typed.type != gs_param_type_dict &&
typed.type != gs_param_type_dict_int_keys &&
typed.type != gs_param_type_array
)
code = param_write_typed(list, string_key, &typed);
}
while (code >= 0);
return code;
}
private int
sget_word(stream *src, uint *pvalue)
{
uint value = 0;
int chunk;
uint shift = 0;
do {
chunk = sgetc(src);
if (chunk < 0)
return chunk;
value |= (chunk & 0x7f) << shift;
shift += 7;
}
while (chunk & 0x80);
*pvalue = value;
return 0;
}
private int
sget_bytes(stream *src, byte *data, uint size)
{
uint ignore_count;
int status = sgets(src, data, size, &ignore_count);
if (status < 0 && status != EOFC)
return_error(gs_error_ioerror);
};
return 0;
}