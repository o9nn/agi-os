#include "stdint_.h"
#include "memory_.h"
#include "stdio_.h"
#include "gserrors.h"
#include "gserror.h"
#include "gdebug.h"
#include "strimpl.h"
#include "sjbig2.h"
private_st_jbig2decode_state();
private int
s_jbig2decode_error(void *error_callback_data, const char *msg, Jbig2Severity severity,
int32_t seg_idx)
{
stream_jbig2decode_state *const state =
(stream_jbig2decode_state *) error_callback_data;
const char *type;
char segment[22];
int code = 0;
switch (severity) {
#ifdef JBIG2_DEBUG
case JBIG2_SEVERITY_DEBUG:
type = "DEBUG"; break;;
case JBIG2_SEVERITY_INFO:
type = "info"; break;;
case JBIG2_SEVERITY_WARNING:
type = "WARNING"; break;;
#else
case JBIG2_SEVERITY_DEBUG:
case JBIG2_SEVERITY_INFO:
case JBIG2_SEVERITY_WARNING:
return 0;
break;;
#endif
case JBIG2_SEVERITY_FATAL:
type = "FATAL ERROR decoding image:";
code = gs_error_ioerror;
if (state != NULL) state->error = code;
break;;
default: type = "unknown message:"; break;;
}
if (seg_idx == -1) segment[0] = '\0';
else sprintf(segment, "(segment 0x%02x)", seg_idx);
dlprintf3("jbig2dec %s %s %s\n", type, msg, segment);
return code;
}
private void
s_jbig2decode_invert_buffer(unsigned char *buf, int length)
{
int i;
for (i = 0; i < length; i++)
*buf++ ^= 0xFF;
}
public int
s_jbig2decode_make_global_ctx(byte *data, uint length, Jbig2GlobalCtx **global_ctx)
{
Jbig2Ctx *ctx = NULL;
int code;
if (length == 0) {
if_debug0('s', "[s] ignoring zero-length jbig2 global stream.\n");
*global_ctx = NULL;
return 0;
}
ctx = jbig2_ctx_new(NULL, JBIG2_OPTIONS_EMBEDDED, NULL,
s_jbig2decode_error, NULL);
code = jbig2_data_in(ctx, data, length);
if (code) {
*global_ctx = NULL;
return code;
}
*global_ctx = jbig2_make_global_ctx(ctx);
return 0;
}
public int
s_jbig2decode_set_global_ctx(stream_state *ss, Jbig2GlobalCtx *global_ctx)
{
stream_jbig2decode_state *state = (stream_jbig2decode_state*)ss;
state->global_ctx = global_ctx;
return 0;
}
private int
s_jbig2decode_init(stream_state * ss)
{
stream_jbig2decode_state *const state = (stream_jbig2decode_state *) ss;
Jbig2GlobalCtx *global_ctx = state->global_ctx;
state->decode_ctx = jbig2_ctx_new(NULL, JBIG2_OPTIONS_EMBEDDED,
global_ctx, s_jbig2decode_error, ss);
state->image = 0;
state->error = 0;
return 0;
}
private int
s_jbig2decode_process(stream_state * ss, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_jbig2decode_state *const state = (stream_jbig2decode_state *) ss;
Jbig2Image *image = state->image;
long in_size = pr->limit - pr->ptr;
long out_size = pw->limit - pw->ptr;
int status = 0;
if (in_size > 0) {
jbig2_data_in(state->decode_ctx, pr->ptr + 1, in_size);
pr->ptr += in_size;
if (last == 1) {
jbig2_complete_page(state->decode_ctx);
}
if (state->error) return state->error;
}
if (out_size > 0) {
if (image == NULL) {
image = jbig2_page_out(state->decode_ctx);
if (image != NULL) {
state->image = image;
state->offset = 0;
}
}
if (image != NULL) {
long image_size = image->height*image->stride;
long usable = min(image_size - state->offset, out_size);
memcpy(pw->ptr + 1, image->data + state->offset, usable);
s_jbig2decode_invert_buffer(pw->ptr + 1, usable);
state->offset += usable;
pw->ptr += usable;
status = (state->offset < image_size) ? 1 : 0;
}
}
return status;
}
private void
s_jbig2decode_release(stream_state *ss)
{
stream_jbig2decode_state *const state = (stream_jbig2decode_state *) ss;
if (state->decode_ctx) {
if (state->image) jbig2_release_page(state->decode_ctx, state->image);
jbig2_ctx_free(state->decode_ctx);
}
}
private void
s_jbig2decode_set_defaults(stream_state *ss)
{
stream_jbig2decode_state *const state = (stream_jbig2decode_state *) ss;
state->global_ctx = NULL;
state->decode_ctx = NULL;
state->image = NULL;
state->offset = 0;
state->error = 0;
}
const stream_template s_jbig2decode_template = {
&st_jbig2decode_state,
s_jbig2decode_init,
s_jbig2decode_process,
1, 1,
s_jbig2decode_release,
s_jbig2decode_set_defaults
};