#include "lib.h"
#include "str.h"
#include "imap-arg.h"
#include "imap-quote.h"
#define QUOTED_MAX_ESCAPE_CHARS 4
void imap_append_string(string_t *dest, const char *src)
{
i_assert(src != NULL);
imap_append_nstring(dest, src);
}
void imap_append_astring(string_t *dest, const char *src)
{
unsigned int i;
i_assert(src != NULL);
for (i = 0; src[i] != '\0'; i++) {
if (!IS_ASTRING_CHAR(src[i])) {
imap_append_string(dest, src);
return;
}
}
if (i == 0 || strcasecmp(src, "NIL") == 0)
imap_append_string(dest, src);
else
str_append(dest, src);
}
static void
imap_append_literal(string_t *dest, const char *src, unsigned int pos)
{
size_t full_len = pos + strlen(src+pos);
str_printfa(dest, "{%zu}\r\n", full_len);
buffer_append(dest, src, full_len);
}
void imap_append_nstring(string_t *dest, const char *src)
{
unsigned int escape_count = 0;
size_t i;
if (src == NULL) {
str_append(dest, "NIL");
return;
}
for (i = 0; src[i] != '\0'; i++) {
switch (src[i]) {
case '"':
case '\\':
if (escape_count++ < QUOTED_MAX_ESCAPE_CHARS)
break;
case 13:
case 10:
imap_append_literal(dest, src, i);
return;
default:
if ((unsigned char)src[i] >= 0x80) {
imap_append_literal(dest, src, i);
return;
}
break;
}
}
imap_append_quoted(dest, src);
}
static void remove_newlines_and_append(string_t *dest, const char *src)
{
size_t src_len;
string_t *src_nolf;
src_len = strlen(src);
src_nolf = t_str_new(src_len);
for (size_t i = 0; i < src_len; ++i) {
if (src[i] != '\r' && src[i] != '\n') {
str_append_c(src_nolf, src[i]);
} else if (src[i+1] != ' ' &&
src[i+1] != '\t' &&
src[i+1] != '\r' &&
src[i+1] != '\n' &&
src[i+1] != '\0') {
str_append_c(src_nolf, ' ');
} else {
}
}
imap_append_nstring(dest, str_c(src_nolf));
}
void imap_append_nstring_nolf(string_t *dest, const char *src)
{
if (src == NULL || strpbrk(src, "\r\n") == NULL)
imap_append_nstring(dest, src);
else if (buffer_get_pool(dest)->datastack_pool)
remove_newlines_and_append(dest, src);
else T_BEGIN {
remove_newlines_and_append(dest, src);
} T_END;
}
void imap_append_quoted(string_t *dest, const char *src)
{
str_append_c(dest, '"');
for (; *src != '\0'; src++) {
switch (*src) {
case 13:
case 10:
break;
case '"':
case '\\':
str_append_c(dest, '\\');
str_append_c(dest, *src);
break;
default:
if ((unsigned char)*src >= 0x80) {
break;
}
str_append_c(dest, *src);
break;
}
}
str_append_c(dest, '"');
}
void imap_append_string_for_humans(string_t *dest,
const unsigned char *src, size_t size)
{
size_t i, pos, remove_count = 0;
bool whitespace_prefix = TRUE, last_lwsp = TRUE, modify = FALSE;
for (i = 0; i < size; i++) {
switch (src[i]) {
case 0:
last_lwsp = FALSE;
modify = TRUE;
break;
case 13:
case 10:
case '\t':
modify = TRUE;
case ' ':
if (last_lwsp) {
modify = TRUE;
remove_count++;
}
last_lwsp = TRUE;
break;
case '"':
case '\\':
modify = TRUE;
last_lwsp = FALSE;
break;
default:
if ((src[i] & 0x80) != 0)
modify = TRUE;
last_lwsp = FALSE;
break;
}
if (!last_lwsp)
whitespace_prefix = FALSE;
}
if (last_lwsp && i > 0 && !whitespace_prefix) {
modify = TRUE;
remove_count++;
}
if (!modify) {
str_append_c(dest, '"');
str_append_data(dest, src, size);
str_append_c(dest, '"');
return;
}
if (size == remove_count) {
str_append(dest, "\"\"");
return;
}
str_printfa(dest, "{%zu}\r\n", size - remove_count);
pos = str_len(dest);
last_lwsp = TRUE; whitespace_prefix = TRUE;
for (i = 0; i < size; i++) {
switch (src[i]) {
case 0:
str_append_c(dest, 128);
last_lwsp = FALSE;
break;
case 13:
case 10:
case '\t':
case ' ':
if (!last_lwsp)
str_append_c(dest, ' ');
last_lwsp = TRUE;
break;
default:
last_lwsp = FALSE;
str_append_c(dest, src[i]);
break;
}
if (!last_lwsp)
whitespace_prefix = FALSE;
}
if (last_lwsp && i > 0 && !whitespace_prefix)
str_truncate(dest, str_len(dest)-1);
i_assert(str_len(dest) - pos == size - remove_count);
}