#include "lib.h"
#include "str.h"
#include "unichar.h"
#include "base64.h"
#include "message-header-encode.h"
#define MIME_WRAPPER_LEN (strlen("=?utf-8?q?""?="))
#define MIME_MAX_LINE_LEN 76
#define IS_LWSP(c) \
((c) == ' ' || (c) == '\t' || (c) == '\n')
static bool
input_idx_need_encoding(const unsigned char *input, size_t i, size_t len)
{
switch (input[i]) {
case '\r':
if (i+1 == len || input[i+1] != '\n')
return TRUE;
i++;
case '\n':
if (i+1 == len) {
return TRUE;
}
i_assert(i+1 < len);
if (input[i+1] != '\t' && input[i+1] != ' ') {
return TRUE;
}
break;
case '\t':
break;
case '=':
if ((i == 0 || IS_LWSP(input[i-1])) && i+2 <= len &&
memcmp(input + i, "=?", 2) == 0)
return TRUE;
break;
default:
if ((input[i] & 0x80) != 0)
return TRUE;
if (input[i] < 32)
return TRUE;
break;
}
return FALSE;
}
void message_header_encode_q(const unsigned char *input, size_t len,
string_t *output, size_t first_line_len)
{
static const unsigned char *rep_char =
(const unsigned char *)UNICODE_REPLACEMENT_CHAR_UTF8;
static const unsigned int rep_char_len =
UNICODE_REPLACEMENT_CHAR_UTF8_LEN;
size_t line_len_left;
bool invalid_char = FALSE;
if (len == 0)
return;
line_len_left = MIME_MAX_LINE_LEN - MIME_WRAPPER_LEN;
if (first_line_len >= MIME_MAX_LINE_LEN - MIME_WRAPPER_LEN - 3) {
str_append(output, "\n\t");
line_len_left--;
} else {
line_len_left -= first_line_len;
}
str_append(output, "=?utf-8?q?");
for (;;) {
unichar_t ch;
int nch = 1;
size_t n_in, n_out = 0, j;
switch (input[0]) {
case ' ':
n_out = 1;
n_in = 1;
break;
case '=':
case '?':
case '_':
n_in = 1;
n_out = 3;
break;
default:
nch = uni_utf8_get_char_n(input, len, &ch);
if (nch <= 0) {
n_in = 1;
if (!invalid_char) {
n_out = rep_char_len * 3;
} else {
n_out = 0;
}
} else if (nch > 1) {
n_in = nch;
n_out = nch * 3;
} else if (ch < 0x20 || ch > 0x7e) {
i_assert(ch < 0x80);
n_in = 1;
n_out = 3;
} else {
n_in = 1;
n_out = 1;
}
}
invalid_char = (nch <= 0);
if (line_len_left < n_out) {
str_append(output, "?=\n\t=?utf-8?q?");
line_len_left = MIME_MAX_LINE_LEN -
MIME_WRAPPER_LEN - 1;
}
if (input[0] == ' ') {
str_append_c(output, '_');
} else if (invalid_char) {
for (j = 0; n_out > 0 && j < rep_char_len; j++)
str_printfa(output, "=%02X", rep_char[j]);
} else if (n_out > 1) {
for (j = 0; j < n_in; j++)
str_printfa(output, "=%02X", input[j]);
} else {
str_append_c(output, input[0]);
}
i_assert(len >= n_in);
line_len_left -= n_out;
input += n_in;
len -= n_in;
if (len == 0)
break;
}
str_append(output, "?=");
}
void message_header_encode_b(const unsigned char *input, size_t len,
string_t *output, size_t first_line_len)
{
static const unsigned char *rep_char =
(const unsigned char *)UNICODE_REPLACEMENT_CHAR_UTF8;
static const unsigned int rep_char_len =
UNICODE_REPLACEMENT_CHAR_UTF8_LEN;
struct base64_encoder b64enc;
size_t line_len_left;
if (len == 0)
return;
line_len_left = MIME_MAX_LINE_LEN - MIME_WRAPPER_LEN;
if (first_line_len >= MIME_MAX_LINE_LEN - MIME_WRAPPER_LEN - 3) {
str_append(output, "\n\t");
line_len_left--;
} else {
line_len_left -= first_line_len;
}
str_append(output, "=?utf-8?b?");
base64_encode_init(&b64enc, &base64_scheme, 0, 0);
for (;;) {
unichar_t ch;
size_t space, max, old_bufsize, n_in, n_out;
int nch = 1;
space = base64_encode_get_full_space(&b64enc, line_len_left);
max = I_MIN(space, len);
for (n_in = 0; n_in < max;) {
nch = uni_utf8_get_char_n(&input[n_in],
len - n_in, &ch);
if (nch <= 0)
break;
if ((n_in + nch) > max)
break;
n_in += nch;
}
if (n_in > 0) {
old_bufsize = output->used;
if (!base64_encode_more(&b64enc, input, n_in,
&n_in, output))
i_unreached();
n_out = output->used - old_bufsize;
i_assert(len >= n_in);
i_assert(line_len_left >= n_out);
input += n_in;
len -= n_in;
line_len_left -= n_out;
}
space = 0;
if (nch <= 0) {
space = base64_encode_get_full_space(
&b64enc, line_len_left);
}
if ((nch > 0 && len > 0) ||
(nch <= 0 && space < rep_char_len)) {
old_bufsize = output->used;
if (!base64_encode_finish(&b64enc, output))
i_unreached();
n_out = output->used - old_bufsize;
i_assert(line_len_left >= n_out);
str_append(output, "?=\n\t=?utf-8?b?");
line_len_left = MIME_MAX_LINE_LEN -
MIME_WRAPPER_LEN - 1;
base64_encode_reset(&b64enc);
}
n_in = 0;
n_out = 0;
if (nch <= 0) {
old_bufsize = output->used;
if (!base64_encode_more(&b64enc, rep_char, rep_char_len,
NULL, output))
i_unreached();
n_in = 1;
n_out = output->used - old_bufsize;
for (; n_in < len; n_in++) {
nch = uni_utf8_get_char_n(&input[n_in],
len - n_in, &ch);
if (nch > 0)
break;
}
}
i_assert(line_len_left >= n_out);
input += n_in;
len -= n_in;
line_len_left -= n_out;
if (len == 0)
break;
}
if (!base64_encode_finish(&b64enc, output))
i_unreached();
str_append(output, "?=");
}
void message_header_encode(const char *input, string_t *output)
{
message_header_encode_data((const void *)input, strlen(input), output);
}
void message_header_encode_data(const unsigned char *input, size_t len,
string_t *output)
{
size_t i, j, first_line_len, cur_line_len, last_idx;
size_t enc_chars, enc_len, base64_len, q_len;
const unsigned char *next_line_input;
size_t next_line_len = 0;
bool use_q, cr;
for (i = 0; i < len; i++) {
if (input_idx_need_encoding(input, i, len))
break;
}
if (i == len) {
str_append_data(output, input, len);
return;
}
if (input[i] != '\r' && input[i] != '\n') {
while (i > 0 && !IS_LWSP(input[i-1]))
i--;
}
str_append_data(output, input, i);
first_line_len = j = i;
while (j > 0 && input[j-1] != '\n') j--;
if (j != 0)
first_line_len = j;
input += i;
len -= i;
next_line_input = memchr(input, '\n', len);
if (next_line_input != NULL) {
cur_line_len = next_line_input - input;
if (cur_line_len > 0 && input[cur_line_len-1] == '\r') {
cur_line_len--;
next_line_input = input + cur_line_len;
}
next_line_len = len - cur_line_len;
len = cur_line_len;
}
last_idx = 0; enc_chars = 0;
for (i = 0; i < len; i++) {
if (input_idx_need_encoding(input, i, len)) {
last_idx = i + 1;
enc_chars++;
}
}
while (last_idx < len && !IS_LWSP(input[last_idx]))
last_idx++;
enc_len = last_idx;
base64_len = MAX_BASE64_ENCODED_SIZE(enc_len);
q_len = enc_len + enc_chars*3;
use_q = q_len*2/3 <= base64_len;
if (enc_len == 0)
;
else if (use_q)
message_header_encode_q(input, enc_len, output, first_line_len);
else
message_header_encode_b(input, enc_len, output, first_line_len);
str_append_data(output, input + last_idx, len - last_idx);
if (next_line_input != NULL) {
i = 0;
if (next_line_input[0] == '\r') {
cr = TRUE;
i++;
} else {
cr = FALSE;
}
i_assert(next_line_input[i] == '\n');
if (++i == next_line_len)
return;
if (cr)
str_append_c(output, '\r');
str_append_c(output, '\n');
if (next_line_input[i] == ' ' || next_line_input[i] == '\t') {
str_append_c(output, next_line_input[i]);
i++;
} else {
str_append_c(output, '\t');
}
message_header_encode_data(next_line_input+i, next_line_len-i,
output);
}
}