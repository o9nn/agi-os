#include "lib.h"
#include "array.h"
#include "write-full.h"
#include "mail-index-private.h"
#include "mail-transaction-log-private.h"
void mail_transaction_log_append_add(struct mail_transaction_log_append_ctx *ctx,
enum mail_transaction_type type,
const void *data, size_t size)
{
struct mail_transaction_header hdr;
i_assert((type & MAIL_TRANSACTION_TYPE_MASK) != 0);
i_assert((size % 4) == 0);
if (size == 0)
return;
i_zero(&hdr);
hdr.type = type | ctx->trans_flags;
if (type == MAIL_TRANSACTION_EXPUNGE ||
type == MAIL_TRANSACTION_EXPUNGE_GUID)
hdr.type |= MAIL_TRANSACTION_EXPUNGE_PROT;
if (type == MAIL_TRANSACTION_BOUNDARY)
hdr.type |= MAIL_TRANSACTION_EXTERNAL;
hdr.size = sizeof(hdr) + size;
hdr.size = mail_index_uint32_to_offset(hdr.size);
buffer_append(ctx->output, &hdr, sizeof(hdr));
buffer_append(ctx->output, data, size);
mail_transaction_update_modseq(&hdr, data, &ctx->new_highest_modseq,
MAIL_TRANSACTION_LOG_HDR_VERSION(&ctx->log->head->hdr));
ctx->transaction_count++;
}
static int
log_buffer_move_to_memory(struct mail_transaction_log_append_ctx *ctx)
{
struct mail_transaction_log_file *file = ctx->log->head;
if (ftruncate(file->fd, file->sync_offset) < 0) {
mail_index_file_set_syscall_error(ctx->log->index,
file->filepath,
"ftruncate()");
}
if (mail_index_move_to_memory(ctx->log->index) < 0)
return -1;
i_assert(MAIL_TRANSACTION_LOG_FILE_IN_MEMORY(file));
i_assert(file->buffer_offset + file->buffer->used == file->sync_offset);
buffer_append_buf(file->buffer, ctx->output, 0, SIZE_MAX);
file->sync_offset = file->buffer_offset + file->buffer->used;
return 0;
}
static int log_buffer_write(struct mail_transaction_log_append_ctx *ctx)
{
struct mail_transaction_log_file *file = ctx->log->head;
if (ctx->output->used == 0)
return 0;
if (MAIL_TRANSACTION_LOG_FILE_IN_MEMORY(file)) {
if (file->buffer == NULL) {
file->buffer = buffer_create_dynamic(default_pool, 4096);
file->buffer_offset = sizeof(file->hdr);
}
buffer_append_buf(file->buffer, ctx->output, 0, SIZE_MAX);
file->sync_offset = file->buffer_offset + file->buffer->used;
return 0;
}
if (write_full(file->fd, ctx->output->data, ctx->output->used) < 0) {
mail_index_file_set_syscall_error(ctx->log->index,
file->filepath,
"write_full()");
return log_buffer_move_to_memory(ctx);
}
if ((ctx->want_fsync &&
file->log->index->set.fsync_mode != FSYNC_MODE_NEVER) ||
file->log->index->set.fsync_mode == FSYNC_MODE_ALWAYS) {
if (fdatasync(file->fd) < 0) {
mail_index_file_set_syscall_error(ctx->log->index,
file->filepath,
"fdatasync()");
return log_buffer_move_to_memory(ctx);
}
}
if (file->mmap_base == NULL && file->buffer != NULL) {
i_assert(file->buffer_offset +
file->buffer->used == file->sync_offset);
buffer_append(file->buffer, ctx->output->data,
ctx->output->used);
}
file->sync_offset += ctx->output->used;
if (ctx->sync_includes_this)
file->max_tail_offset = file->sync_offset;
return 0;
}
static void
log_append_sync_offset_if_needed(struct mail_transaction_log_append_ctx *ctx)
{
struct mail_transaction_log_file *file = ctx->log->head;
struct mail_transaction_header_update *u;
struct mail_transaction_header *hdr;
uint32_t offset;
buffer_t buf;
unsigned char update_data[sizeof(*u) + sizeof(offset)];
offset = file->max_tail_offset;
if (!ctx->index_sync_transaction) {
i_assert(!ctx->tail_offset_changed);
return;
} else if (file->max_tail_offset == file->sync_offset) {
if (ctx->output->used == 0 && !ctx->tail_offset_changed) {
return;
}
offset = file->max_tail_offset + ctx->output->used +
sizeof(*hdr) + sizeof(*u) + sizeof(offset);
ctx->sync_includes_this = TRUE;
} else {
}
if (file->last_read_hdr_tail_offset == offset)
return;
i_assert(offset > file->last_read_hdr_tail_offset);
buffer_create_from_data(&buf, update_data, sizeof(update_data));
u = buffer_append_space_unsafe(&buf, sizeof(*u));
u->offset = offsetof(struct mail_index_header, log_file_tail_offset);
u->size = sizeof(offset);
buffer_append(&buf, &offset, sizeof(offset));
mail_transaction_log_append_add(ctx, MAIL_TRANSACTION_HEADER_UPDATE,
buf.data, buf.used);
}
static int
mail_transaction_log_append_locked(struct mail_transaction_log_append_ctx *ctx)
{
struct mail_transaction_log_file *file = ctx->log->head;
struct mail_transaction_boundary *boundary;
if (file->sync_offset < file->last_size) {
buffer_set_used_size(file->buffer,
file->sync_offset - file->buffer_offset);
if (!MAIL_TRANSACTION_LOG_FILE_IN_MEMORY(file)) {
if (ftruncate(file->fd, file->sync_offset) < 0) {
mail_index_file_set_syscall_error(ctx->log->index,
file->filepath, "ftruncate()");
}
}
}
boundary = buffer_get_space_unsafe(ctx->output,
sizeof(struct mail_transaction_header),
sizeof(*boundary));
boundary->size = ctx->output->used;
if (ctx->transaction_count <= 2) {
unsigned int boundary_size =
sizeof(struct mail_transaction_header) +
sizeof(*boundary);
buffer_delete(ctx->output, 0, boundary_size);
}
log_append_sync_offset_if_needed(ctx);
if (log_buffer_write(ctx) < 0)
return -1;
file->sync_highest_modseq = ctx->new_highest_modseq;
return 0;
}
int mail_transaction_log_append_begin(struct mail_index *index,
enum mail_transaction_type flags,
struct mail_transaction_log_append_ctx **ctx_r)
{
struct mail_transaction_log_append_ctx *ctx;
struct mail_transaction_boundary boundary;
if (!index->log_sync_locked) {
if (mail_transaction_log_lock_head(index->log, "appending") < 0)
return -1;
}
ctx = i_new(struct mail_transaction_log_append_ctx, 1);
ctx->log = index->log;
ctx->output = buffer_create_dynamic(default_pool, 1024);
ctx->trans_flags = flags;
i_zero(&boundary);
mail_transaction_log_append_add(ctx, MAIL_TRANSACTION_BOUNDARY,
&boundary, sizeof(boundary));
*ctx_r = ctx;
return 0;
}
int mail_transaction_log_append_commit(struct mail_transaction_log_append_ctx **_ctx)
{
struct mail_transaction_log_append_ctx *ctx = *_ctx;
struct mail_index *index = ctx->log->index;
int ret = 0;
*_ctx = NULL;
ret = mail_transaction_log_append_locked(ctx);
if (!index->log_sync_locked)
mail_transaction_log_file_unlock(index->log->head, "appending");
buffer_free(&ctx->output);
i_free(ctx);
return ret;
}