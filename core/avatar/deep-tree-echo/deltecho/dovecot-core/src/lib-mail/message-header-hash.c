#include "lib.h"
#include "hash-method.h"
#include "message-header-hash.h"
void message_header_hash_more(struct message_header_hash_context *ctx,
const struct hash_method *method, void *context,
unsigned int version,
const unsigned char *data, size_t size)
{
size_t i, start;
i_assert(version >= 1 && version <= MESSAGE_HEADER_HASH_MAX_VERSION);
if (version == 1) {
method->loop(context, data, size);
return;
}
for (i = start = 0; i < size; i++) {
bool cur_is_questionmark = FALSE;
switch (data[i]) {
case ' ':
if (version >= 3) {
method->loop(context, data + start, i-start);
start = i+1;
}
break;
case '\t':
if (version >= 4) {
method->loop(context, data + start, i-start);
start = i+1;
}
break;
case '\n':
break;
default:
if (data[i] < 0x20 || data[i] >= 0x7f || data[i] == '?') {
if (start < i || !ctx->prev_was_questionmark) {
method->loop(context, data + start, i-start);
method->loop(context, "?", 1);
}
start = i+1;
cur_is_questionmark = TRUE;
}
break;
}
ctx->prev_was_questionmark = cur_is_questionmark;
}
method->loop(context, data + start, i-start);
}