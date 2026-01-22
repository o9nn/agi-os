#include "lib.h"
#include "array.h"
#include "str.h"
#include "strescape.h"
#include "ostream.h"
#include "doveadm.h"
#include "doveadm-print-private.h"
#include "client-connection.h"
#define DOVEADM_PRINT_FLUSH_TIMEOUT_SECS 60
struct doveadm_print_server_context {
unsigned int header_idx, header_count;
string_t *str;
};
static struct doveadm_print_server_context ctx;
static void doveadm_print_server_flush(void);
static void doveadm_print_server_init(void)
{
ctx.str = str_new(default_pool, 256);
}
static void doveadm_print_server_deinit(void)
{
str_free(&ctx.str);
}
static void
doveadm_print_server_header(const struct doveadm_print_header *hdr ATTR_UNUSED)
{
ctx.header_count++;
}
static void doveadm_print_server_print(const char *value)
{
str_append_tabescaped(ctx.str, value);
str_append_c(ctx.str, '\t');
if (++ctx.header_idx == ctx.header_count) {
ctx.header_idx = 0;
doveadm_print_server_flush();
}
}
static void
doveadm_print_server_print_stream(const unsigned char *value, size_t size)
{
if (size == 0) {
doveadm_print_server_print("");
return;
}
str_append_tabescaped_n(ctx.str, value, size);
if (str_len(ctx.str) >= IO_BLOCK_SIZE)
doveadm_print_server_flush();
}
static int flush_callback(struct doveadm_print_server_context *ctx ATTR_UNUSED)
{
int ret;
if ((ret = o_stream_flush(doveadm_print_ostream)) != 0)
io_loop_stop(current_ioloop);
return ret;
}
static void handle_flush_timeout(struct doveadm_print_server_context *ctx ATTR_UNUSED)
{
io_loop_stop(current_ioloop);
o_stream_close(doveadm_print_ostream);
i_error("write(%s) failed: Timed out after %u seconds",
o_stream_get_name(doveadm_print_ostream),
DOVEADM_PRINT_FLUSH_TIMEOUT_SECS);
}
static void doveadm_print_server_flush(void)
{
o_stream_nsend(doveadm_print_ostream,
str_data(ctx.str), str_len(ctx.str));
str_truncate(ctx.str, 0);
o_stream_uncork(doveadm_print_ostream);
if (o_stream_get_buffer_used_size(doveadm_print_ostream) < IO_BLOCK_SIZE ||
doveadm_print_ostream->stream_errno != 0)
return;
struct ioloop *prev_loop = current_ioloop;
struct ioloop *loop = io_loop_create();
struct timeout *to =
timeout_add(DOVEADM_PRINT_FLUSH_TIMEOUT_SECS*1000, handle_flush_timeout, &ctx);
o_stream_switch_ioloop_to(doveadm_print_ostream, loop);
o_stream_set_flush_callback(doveadm_print_ostream, flush_callback, &ctx);
io_loop_run(loop);
timeout_remove(&to);
o_stream_unset_flush_callback(doveadm_print_ostream);
o_stream_switch_ioloop_to(doveadm_print_ostream, prev_loop);
io_loop_destroy(&loop);
}
struct doveadm_print_vfuncs doveadm_print_server_vfuncs = {
DOVEADM_PRINT_TYPE_SERVER,
doveadm_print_server_init,
doveadm_print_server_deinit,
doveadm_print_server_header,
doveadm_print_server_print,
doveadm_print_server_print_stream,
doveadm_print_server_flush
};