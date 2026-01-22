#include "lib.h"
#include "ioloop.h"
#include "array.h"
#include "buffer.h"
#include "hostpid.h"
#include "istream.h"
#include "file-set-size.h"
#include "str.h"
#include "read-full.h"
#include "write-full.h"
#include "sleep.h"
#include "message-date.h"
#include "istream-raw-mbox.h"
#include "mbox-storage.h"
#include "index-sync-changes.h"
#include "mailbox-uidvalidity.h"
#include "mailbox-recent-flags.h"
#include "mbox-from.h"
#include "mbox-file.h"
#include "mbox-lock.h"
#include "mbox-sync-private.h"
#include <utime.h>
#include <sys/stat.h>
#define PSEUDO_MESSAGE_BODY \
"This text is part of the internal format of your mail folder, and is not\n" \
"a real message.  It is created automatically by the mail system software.\n" \
"If deleted, important folder data will be lost, and it will be re-created\n" \
"with the data reset to initial values.\n"
void mbox_sync_set_critical(struct mbox_sync_context *sync_ctx,
const char *fmt, ...)
{
va_list va;
sync_ctx->errors = TRUE;
if (sync_ctx->ext_modified) {
mailbox_set_critical(&sync_ctx->mbox->box,
"mbox was modified while we were syncing, "
"check your locking settings");
}
va_start(va, fmt);
mailbox_set_critical(&sync_ctx->mbox->box,
"Sync failed for mbox: %s",
t_strdup_vprintf(fmt, va));
va_end(va);
}
int mbox_sync_seek(struct mbox_sync_context *sync_ctx, uoff_t from_offset)
{
if (istream_raw_mbox_seek(sync_ctx->input, from_offset) < 0) {
mbox_sync_set_critical(sync_ctx,
"Unexpectedly lost From-line at offset %"PRIuUOFF_T,
from_offset);
return -1;
}
return 0;
}
void mbox_sync_file_update_ext_modified(struct mbox_sync_context *sync_ctx)
{
struct stat st;
if (fstat(sync_ctx->write_fd, &st) < 0) {
mbox_set_syscall_error(sync_ctx->mbox, "fstat()");
return;
}
if (st.st_size != sync_ctx->last_stat.st_size ||
(sync_ctx->last_stat.st_mtime != 0 &&
!CMP_ST_MTIME(&st, &sync_ctx->last_stat)))
sync_ctx->ext_modified = TRUE;
sync_ctx->last_stat = st;
}
void mbox_sync_file_updated(struct mbox_sync_context *sync_ctx, bool dirty)
{
if (dirty) {
sync_ctx->last_stat.st_mtime = 0;
return;
}
if (fstat(sync_ctx->write_fd, &sync_ctx->last_stat) < 0)
mbox_set_syscall_error(sync_ctx->mbox, "fstat()");
i_stream_sync(sync_ctx->input);
}
static int
mbox_sync_read_next_mail(struct mbox_sync_context *sync_ctx,
struct mbox_sync_mail_context *mail_ctx)
{
uoff_t offset;
(void)istream_raw_mbox_get_header_offset(sync_ctx->input, &offset);
if (istream_raw_mbox_is_eof(sync_ctx->input))
return 0;
p_clear(sync_ctx->mail_keyword_pool);
i_zero(mail_ctx);
mail_ctx->sync_ctx = sync_ctx;
mail_ctx->seq = ++sync_ctx->seq;
mail_ctx->header = sync_ctx->header;
mail_ctx->mail.from_offset =
istream_raw_mbox_get_start_offset(sync_ctx->input);
if (istream_raw_mbox_get_header_offset(sync_ctx->input, &mail_ctx->mail.offset) < 0) {
mbox_sync_set_critical(sync_ctx,
"Couldn't get header offset for seq=%u", mail_ctx->seq);
return -1;
}
if (mbox_sync_parse_next_mail(sync_ctx->input, mail_ctx) < 0)
return -1;
if (istream_raw_mbox_is_corrupted(sync_ctx->input))
return -1;
i_assert(sync_ctx->input->v_offset != mail_ctx->mail.from_offset ||
sync_ctx->input->eof);
if (istream_raw_mbox_get_body_size(sync_ctx->input,
mail_ctx->content_length,
&mail_ctx->mail.body_size) < 0) {
mbox_sync_set_critical(sync_ctx,
"Couldn't get body size for seq=%u", mail_ctx->seq);
return -1;
}
i_assert(mail_ctx->mail.body_size < OFF_T_MAX);
if ((mail_ctx->mail.flags & MAIL_RECENT) != 0 &&
!mail_ctx->mail.pseudo) {
if (!sync_ctx->keep_recent) {
mail_ctx->need_rewrite = TRUE;
}
mail_ctx->recent = TRUE;
}
return 1;
}
static void mbox_sync_read_index_syncs(struct mbox_sync_context *sync_ctx,
uint32_t uid, bool *sync_expunge_r)
{
guid_128_t expunged_guid_128;
if (uid == 0 || sync_ctx->index_reset) {
uid = (uint32_t)-1;
}
index_sync_changes_read(sync_ctx->sync_changes, uid, sync_expunge_r,
expunged_guid_128);
if (sync_ctx->readonly) {
*sync_expunge_r = FALSE;
}
}
static bool
mbox_sync_read_index_rec(struct mbox_sync_context *sync_ctx,
uint32_t uid, const struct mail_index_record **rec_r)
{
const struct mail_index_record *rec = NULL;
uint32_t messages_count;
bool ret = FALSE;
if (sync_ctx->index_reset) {
*rec_r = NULL;
return TRUE;
}
messages_count =
mail_index_view_get_messages_count(sync_ctx->sync_view);
while (sync_ctx->idx_seq <= messages_count) {
rec = mail_index_lookup(sync_ctx->sync_view, sync_ctx->idx_seq);
if (uid <= rec->uid)
break;
mail_index_expunge(sync_ctx->t, sync_ctx->idx_seq);
sync_ctx->idx_seq++;
rec = NULL;
}
if (rec == NULL && uid < sync_ctx->idx_next_uid) {
mbox_sync_set_critical(sync_ctx,
"Expunged message reappeared to mailbox "
"(UID %u < %u, seq=%u, idx_msgs=%u)",
uid, sync_ctx->idx_next_uid,
sync_ctx->seq, messages_count);
ret = FALSE; rec = NULL;
} else if (rec != NULL && rec->uid != uid) {
mbox_sync_set_critical(sync_ctx,
"UID inserted in the middle of mailbox "
"(%u > %u, seq=%u, idx_msgs=%u)",
rec->uid, uid, sync_ctx->seq, messages_count);
ret = FALSE; rec = NULL;
} else {
ret = TRUE;
}
*rec_r = rec;
return ret;
}
static void mbox_sync_find_index_md5(struct mbox_sync_context *sync_ctx,
unsigned char hdr_md5_sum[],
const struct mail_index_record **rec_r)
{
const struct mail_index_record *rec = NULL;
uint32_t messages_count;
const void *data;
if (sync_ctx->index_reset) {
*rec_r = NULL;
return;
}
messages_count =
mail_index_view_get_messages_count(sync_ctx->sync_view);
while (sync_ctx->idx_seq <= messages_count) {
rec = mail_index_lookup(sync_ctx->sync_view, sync_ctx->idx_seq);
mail_index_lookup_ext(sync_ctx->sync_view,
sync_ctx->idx_seq,
sync_ctx->mbox->md5hdr_ext_idx,
&data, NULL);
if (data != NULL && memcmp(data, hdr_md5_sum, 16) == 0)
break;
mail_index_expunge(sync_ctx->t, sync_ctx->idx_seq);
sync_ctx->idx_seq++;
rec = NULL;
}
*rec_r = rec;
}
static void
mbox_sync_update_from_offset(struct mbox_sync_context *sync_ctx,
struct mbox_sync_mail *mail,
bool nocheck)
{
const void *data;
uint64_t offset;
if (!nocheck) {
mail_index_lookup_ext(sync_ctx->sync_view, sync_ctx->idx_seq,
sync_ctx->mbox->mbox_ext_idx,
&data, NULL);
if (data != NULL &&
*((const uint64_t *)data) == mail->from_offset)
return;
}
offset = mail->from_offset;
mail_index_update_ext(sync_ctx->t, sync_ctx->idx_seq,
sync_ctx->mbox->mbox_ext_idx, &offset, NULL);
}
static void
mbox_sync_update_index_keywords(struct mbox_sync_mail_context *mail_ctx)
{
struct mbox_sync_context *sync_ctx = mail_ctx->sync_ctx;
struct mail_index *index = sync_ctx->mbox->box.index;
struct mail_keywords *keywords;
keywords = !array_is_created(&mail_ctx->mail.keywords) ?
mail_index_keywords_create(index, NULL) :
mail_index_keywords_create_from_indexes(index,
&mail_ctx->mail.keywords);
mail_index_update_keywords(sync_ctx->t, sync_ctx->idx_seq,
MODIFY_REPLACE, keywords);
mail_index_keywords_unref(&keywords);
}
static void
mbox_sync_update_md5_if_changed(struct mbox_sync_mail_context *mail_ctx)
{
struct mbox_sync_context *sync_ctx = mail_ctx->sync_ctx;
const void *ext_data;
mail_index_lookup_ext(sync_ctx->sync_view, sync_ctx->idx_seq,
sync_ctx->mbox->md5hdr_ext_idx, &ext_data, NULL);
if (ext_data == NULL ||
memcmp(mail_ctx->hdr_md5_sum, ext_data, 16) != 0) {
mail_index_update_ext(sync_ctx->t, sync_ctx->idx_seq,
sync_ctx->mbox->md5hdr_ext_idx,
mail_ctx->hdr_md5_sum, NULL);
}
}
static void mbox_sync_get_dirty_flags(struct mbox_sync_mail_context *mail_ctx,
const struct mail_index_record *rec)
{
struct mbox_sync_context *sync_ctx = mail_ctx->sync_ctx;
ARRAY_TYPE(keyword_indexes) idx_keywords;
uint8_t idx_flags, mbox_flags;
mail_ctx->mail.flags &= ENUM_NEGATE(MAIL_INDEX_MAIL_FLAG_DIRTY);
idx_flags = rec->flags & MAIL_FLAGS_NONRECENT;
mbox_flags = mail_ctx->mail.flags & MAIL_FLAGS_NONRECENT;
if (idx_flags != mbox_flags) {
mail_ctx->need_rewrite = TRUE;
mail_ctx->mail.flags = (mail_ctx->mail.flags & MAIL_RECENT) |
idx_flags | MAIL_INDEX_MAIL_FLAG_DIRTY;
}
t_array_init(&idx_keywords, 32);
mail_index_lookup_keywords(sync_ctx->sync_view, sync_ctx->idx_seq,
&idx_keywords);
if (!index_keyword_array_cmp(&idx_keywords, &mail_ctx->mail.keywords)) {
mail_ctx->need_rewrite = TRUE;
mail_ctx->mail.flags |= MAIL_INDEX_MAIL_FLAG_DIRTY;
if (!array_is_created(&mail_ctx->mail.keywords)) {
p_array_init(&mail_ctx->mail.keywords,
sync_ctx->mail_keyword_pool,
array_count(&idx_keywords));
}
array_clear(&mail_ctx->mail.keywords);
array_append_array(&mail_ctx->mail.keywords, &idx_keywords);
}
}
static void mbox_sync_update_flags(struct mbox_sync_mail_context *mail_ctx,
const struct mail_index_record *rec)
{
struct mbox_sync_context *sync_ctx = mail_ctx->sync_ctx;
struct mailbox *box = &sync_ctx->mbox->box;
struct mbox_sync_mail *mail = &mail_ctx->mail;
enum mail_index_sync_type sync_type;
ARRAY_TYPE(keyword_indexes) orig_keywords = ARRAY_INIT;
uint8_t flags, orig_flags;
if (rec != NULL) {
if ((rec->flags & MAIL_INDEX_MAIL_FLAG_DIRTY) != 0) {
mbox_sync_get_dirty_flags(mail_ctx, rec);
}
}
flags = orig_flags = mail->flags & MAIL_FLAGS_NONRECENT;
if (array_is_created(&mail->keywords)) {
t_array_init(&orig_keywords, 32);
array_append_array(&orig_keywords, &mail->keywords);
}
index_sync_changes_apply(sync_ctx->sync_changes,
sync_ctx->mail_keyword_pool,
&flags, &mail->keywords, &sync_type);
if (flags != orig_flags ||
!index_keyword_array_cmp(&mail->keywords, &orig_keywords)) {
mail_ctx->need_rewrite = TRUE;
mail->flags = flags | (mail->flags & MAIL_RECENT) |
MAIL_INDEX_MAIL_FLAG_DIRTY;
}
if (sync_type != 0) {
mailbox_sync_notify(box, mail_ctx->mail.uid,
index_sync_type_convert(sync_type));
}
}
static void mbox_sync_update_index(struct mbox_sync_mail_context *mail_ctx,
const struct mail_index_record *rec)
{
struct mbox_sync_context *sync_ctx = mail_ctx->sync_ctx;
struct mbox_sync_mail *mail = &mail_ctx->mail;
ARRAY_TYPE(keyword_indexes) idx_keywords;
uint8_t mbox_flags;
mbox_flags = mail->flags & ENUM_NEGATE(MAIL_RECENT);
if (!sync_ctx->delay_writes) {
mbox_flags &= ENUM_NEGATE(MAIL_INDEX_MAIL_FLAG_DIRTY);
} else if (mail_ctx->need_rewrite) {
mbox_flags |= MAIL_INDEX_MAIL_FLAG_DIRTY;
}
if (rec == NULL) {
mail_index_append(sync_ctx->t, mail->uid, &sync_ctx->idx_seq);
mail_index_update_flags(sync_ctx->t, sync_ctx->idx_seq,
MODIFY_REPLACE, mbox_flags);
mbox_sync_update_index_keywords(mail_ctx);
if (sync_ctx->mbox->mbox_save_md5) {
mail_index_update_ext(sync_ctx->t, sync_ctx->idx_seq,
sync_ctx->mbox->md5hdr_ext_idx,
mail_ctx->hdr_md5_sum, NULL);
}
} else {
if ((rec->flags & MAIL_FLAGS_NONRECENT) !=
(mbox_flags & MAIL_FLAGS_NONRECENT)) {
mail_index_update_flags(sync_ctx->t, sync_ctx->idx_seq,
MODIFY_REPLACE, mbox_flags);
} else if (((rec->flags ^ mbox_flags) &
MAIL_INDEX_MAIL_FLAG_DIRTY) != 0) {
bool dirty;
dirty = (mbox_flags & MAIL_INDEX_MAIL_FLAG_DIRTY) != 0;
mail_index_update_flags(sync_ctx->t, sync_ctx->idx_seq,
dirty ? MODIFY_ADD : MODIFY_REMOVE,
(enum mail_flags)MAIL_INDEX_MAIL_FLAG_DIRTY);
}
t_array_init(&idx_keywords, 32);
mail_index_lookup_keywords(sync_ctx->sync_view,
sync_ctx->idx_seq, &idx_keywords);
if (!index_keyword_array_cmp(&idx_keywords, &mail->keywords))
mbox_sync_update_index_keywords(mail_ctx);
if (sync_ctx->mbox->mbox_save_md5)
mbox_sync_update_md5_if_changed(mail_ctx);
}
if (!mail_ctx->recent) {
sync_ctx->last_nonrecent_uid = mail->uid;
}
if (sync_ctx->need_space_seq == 0) {
bool nocheck = rec == NULL || sync_ctx->expunged_space > 0;
mbox_sync_update_from_offset(sync_ctx, mail, nocheck);
}
}
static int mbox_read_from_line(struct mbox_sync_mail_context *ctx)
{
struct istream *input = ctx->sync_ctx->file_input;
const unsigned char *data;
size_t size, from_line_size;
buffer_set_used_size(ctx->sync_ctx->from_line, 0);
from_line_size = ctx->hdr_offset - ctx->mail.from_offset;
i_stream_seek(input, ctx->mail.from_offset);
for (;;) {
data = i_stream_get_data(input, &size);
if (size >= from_line_size)
size = from_line_size;
buffer_append(ctx->sync_ctx->from_line, data, size);
i_stream_skip(input, size);
from_line_size -= size;
if (from_line_size == 0)
break;
if (i_stream_read(input) < 0)
return -1;
}
return 0;
}
static int mbox_rewrite_base_uid_last(struct mbox_sync_context *sync_ctx)
{
unsigned char buf[10];
const char *str;
uint32_t uid_last;
unsigned int i;
int ret;
i_assert(sync_ctx->base_uid_last_offset != 0);
ret = pread_full(sync_ctx->write_fd, buf, sizeof(buf),
sync_ctx->base_uid_last_offset);
if (ret < 0) {
mbox_set_syscall_error(sync_ctx->mbox, "pread_full()");
return -1;
}
if (ret == 0) {
mbox_sync_set_critical(sync_ctx,
"X-IMAPbase uid-last offset unexpectedly outside mbox");
return -1;
}
for (i = 0, uid_last = 0; i < sizeof(buf); i++) {
if (buf[i] < '0' || buf[i] > '9') {
uid_last = (uint32_t)-1;
break;
}
uid_last = uid_last * 10 + (buf[i] - '0');
}
if (uid_last != sync_ctx->base_uid_last) {
mbox_sync_set_critical(sync_ctx,
"X-IMAPbase uid-last unexpectedly lost");
return -1;
}
str = t_strdup_printf("%010u", sync_ctx->next_uid - 1);
if (pwrite_full(sync_ctx->write_fd, str, 10,
sync_ctx->base_uid_last_offset) < 0) {
mbox_set_syscall_error(sync_ctx->mbox, "pwrite_full()");
return -1;
}
mbox_sync_file_updated(sync_ctx, FALSE);
sync_ctx->base_uid_last = sync_ctx->next_uid - 1;
return 0;
}
static int
mbox_write_from_line(struct mbox_sync_mail_context *ctx)
{
string_t *str = ctx->sync_ctx->from_line;
if (pwrite_full(ctx->sync_ctx->write_fd, str_data(str), str_len(str),
ctx->mail.from_offset) < 0) {
mbox_set_syscall_error(ctx->sync_ctx->mbox, "pwrite_full()");
return -1;
}
mbox_sync_file_updated(ctx->sync_ctx, FALSE);
return 0;
}
static void update_from_offsets(struct mbox_sync_context *sync_ctx)
{
const struct mbox_sync_mail *mails;
unsigned int i, count;
uint32_t ext_idx;
uint64_t offset;
ext_idx = sync_ctx->mbox->mbox_ext_idx;
mails = array_get(&sync_ctx->mails, &count);
for (i = 0; i < count; i++) {
if (mails[i].idx_seq == 0 || mails[i].expunged)
continue;
sync_ctx->moved_offsets = TRUE;
offset = mails[i].from_offset;
mail_index_update_ext(sync_ctx->t, mails[i].idx_seq,
ext_idx, &offset, NULL);
}
}
static void mbox_sync_handle_expunge(struct mbox_sync_mail_context *mail_ctx)
{
struct mbox_sync_context *sync_ctx = mail_ctx->sync_ctx;
struct mailbox *box = &sync_ctx->mbox->box;
mailbox_sync_notify(box, mail_ctx->mail.uid,
MAILBOX_SYNC_TYPE_EXPUNGE);
mail_index_expunge(sync_ctx->t, mail_ctx->mail.idx_seq);
mail_ctx->mail.expunged = TRUE;
mail_ctx->mail.offset = mail_ctx->mail.from_offset;
mail_ctx->mail.space =
mail_ctx->body_offset - mail_ctx->mail.from_offset +
mail_ctx->mail.body_size;
mail_ctx->mail.body_size = 0;
mail_ctx->mail.uid = 0;
if (sync_ctx->seq == 1) {
mail_ctx->mail.space++;
if (istream_raw_mbox_has_crlf_ending(sync_ctx->input)) {
mail_ctx->mail.space++;
sync_ctx->first_mail_crlf_expunged = TRUE;
}
sync_ctx->base_uid_last_offset = 0;
}
sync_ctx->expunged_space += mail_ctx->mail.space;
}
static int mbox_sync_handle_header(struct mbox_sync_mail_context *mail_ctx)
{
struct mbox_sync_context *sync_ctx = mail_ctx->sync_ctx;
uoff_t orig_from_offset, postlf_from_offset = UOFF_T_MAX;
off_t move_diff;
int ret;
if (sync_ctx->expunged_space > 0 && sync_ctx->need_space_seq == 0) {
move_diff = -sync_ctx->expunged_space;
orig_from_offset = mail_ctx->mail.from_offset;
if (sync_ctx->dest_first_mail) {
mail_ctx->mail.from_offset++;
if (sync_ctx->first_mail_crlf_expunged)
mail_ctx->mail.from_offset++;
}
postlf_from_offset = mail_ctx->mail.from_offset;
if (mbox_read_from_line(mail_ctx) < 0)
return -1;
i_assert((off_t)mail_ctx->mail.from_offset + move_diff != 1 &&
(off_t)mail_ctx->mail.from_offset + move_diff != 2);
mbox_sync_update_header(mail_ctx);
ret = mbox_sync_try_rewrite(mail_ctx, move_diff);
if (ret < 0)
return -1;
if (ret > 0) {
i_assert((off_t)mail_ctx->mail.from_offset >=
-move_diff);
mail_ctx->mail.from_offset = (off_t)mail_ctx->mail.from_offset + move_diff;
mail_ctx->mail.offset = (off_t)mail_ctx->mail.offset + move_diff;
if (mbox_write_from_line(mail_ctx) < 0)
return -1;
} else {
if (sync_ctx->dest_first_mail) {
mail_ctx->mail.from_offset = orig_from_offset;
}
}
} else if (mail_ctx->need_rewrite) {
mbox_sync_update_header(mail_ctx);
if (sync_ctx->delay_writes && sync_ctx->need_space_seq == 0) {
mail_ctx->dirty = TRUE;
return 0;
}
if ((ret = mbox_sync_try_rewrite(mail_ctx, 0)) < 0)
return -1;
} else {
return 0;
}
if (ret == 0 && sync_ctx->need_space_seq == 0) {
sync_ctx->need_space_seq = sync_ctx->seq;
sync_ctx->space_diff = 0;
if (sync_ctx->expunged_space > 0) {
struct mbox_sync_mail mail;
i_assert(postlf_from_offset != UOFF_T_MAX);
mail_ctx->mail.from_offset = postlf_from_offset;
i_zero(&mail);
mail.expunged = TRUE;
mail.offset = mail.from_offset =
mail_ctx->mail.from_offset -
sync_ctx->expunged_space;
mail.space = sync_ctx->expunged_space;
sync_ctx->space_diff = sync_ctx->expunged_space;
sync_ctx->expunged_space = 0;
i_assert(sync_ctx->space_diff < -mail_ctx->mail.space);
sync_ctx->need_space_seq--;
array_push_back(&sync_ctx->mails, &mail);
}
}
return 0;
}
static int
mbox_sync_handle_missing_space(struct mbox_sync_mail_context *mail_ctx)
{
struct mbox_sync_context *sync_ctx = mail_ctx->sync_ctx;
uoff_t end_offset, move_diff, extra_space, needed_space;
uint32_t last_seq;
ARRAY_TYPE(keyword_indexes) keywords_copy;
i_assert(mail_ctx->mail.uid == 0 || mail_ctx->mail.space > 0 ||
mail_ctx->mail.offset == mail_ctx->hdr_offset);
if (array_is_created(&mail_ctx->mail.keywords)) {
p_array_init(&keywords_copy, sync_ctx->saved_keywords_pool,
array_count(&mail_ctx->mail.keywords));
array_append_array(&keywords_copy, &mail_ctx->mail.keywords);
mail_ctx->mail.keywords = keywords_copy;
}
array_push_back(&sync_ctx->mails, &mail_ctx->mail);
sync_ctx->space_diff += mail_ctx->mail.space;
if (sync_ctx->space_diff < 0) {
if (sync_ctx->expunged_space > 0) {
i_assert(sync_ctx->expunged_space ==
mail_ctx->mail.space);
sync_ctx->expunged_space = 0;
}
return 0;
}
if (mail_ctx->mail.uid == 0) {
i_assert(mail_ctx->mail.space >= sync_ctx->space_diff);
extra_space = MBOX_HEADER_PADDING *
(sync_ctx->seq - sync_ctx->need_space_seq + 1);
needed_space = mail_ctx->mail.space - sync_ctx->space_diff;
if ((uoff_t)sync_ctx->space_diff > needed_space + extra_space) {
move_diff = needed_space + extra_space;
sync_ctx->expunged_space =
mail_ctx->mail.space - move_diff;
} else {
move_diff = mail_ctx->mail.space;
extra_space = sync_ctx->space_diff;
sync_ctx->expunged_space = 0;
}
last_seq = sync_ctx->seq - 1;
array_pop_back(&sync_ctx->mails);
end_offset = mail_ctx->mail.from_offset;
} else {
sync_ctx->expunged_space = 0;
last_seq = sync_ctx->seq;
end_offset = mail_ctx->body_offset;
move_diff = 0;
extra_space = sync_ctx->space_diff;
}
mbox_sync_file_update_ext_modified(sync_ctx);
if (mbox_sync_rewrite(sync_ctx,
last_seq == sync_ctx->seq ? mail_ctx : NULL,
end_offset, move_diff, extra_space,
sync_ctx->need_space_seq, last_seq) < 0)
return -1;
update_from_offsets(sync_ctx);
i_zero(mail_ctx);
sync_ctx->need_space_seq = 0;
sync_ctx->space_diff = 0;
array_clear(&sync_ctx->mails);
p_clear(sync_ctx->saved_keywords_pool);
return 0;
}
static int
mbox_sync_seek_to_seq(struct mbox_sync_context *sync_ctx, uint32_t seq)
{
struct mbox_mailbox *mbox = sync_ctx->mbox;
uoff_t old_offset, offset;
uint32_t uid;
int ret;
bool deleted;
if (seq == 0) {
if (istream_raw_mbox_seek(mbox->mbox_stream, 0) < 0) {
mbox->invalid_mbox_file = TRUE;
mail_storage_set_error(&mbox->storage->storage,
MAIL_ERROR_NOTPOSSIBLE,
"Mailbox isn't a valid mbox file");
return -1;
}
seq++;
} else {
old_offset = istream_raw_mbox_get_start_offset(sync_ctx->input);
ret = mbox_file_seek(mbox, sync_ctx->sync_view, seq, &deleted);
if (ret < 0) {
if (deleted) {
mbox_sync_set_critical(sync_ctx,
"Message was expunged unexpectedly");
}
return -1;
}
if (ret == 0) {
if (istream_raw_mbox_seek(mbox->mbox_stream,
old_offset) < 0) {
mbox_sync_set_critical(sync_ctx,
"Error seeking back to original "
"offset %s", dec2str(old_offset));
return -1;
}
return 0;
}
}
if (seq <= 1)
uid = 0;
else
mail_index_lookup_uid(sync_ctx->sync_view, seq-1, &uid);
sync_ctx->prev_msg_uid = uid;
sync_ctx->seq = seq-1;
if (sync_ctx->seq == 0 &&
istream_raw_mbox_get_start_offset(sync_ctx->input) != 0) {
sync_ctx->seq++;
}
sync_ctx->idx_seq = seq;
sync_ctx->dest_first_mail = sync_ctx->seq == 0;
if (istream_raw_mbox_get_body_offset(sync_ctx->input, &offset) < 0) {
mbox_sync_set_critical(sync_ctx,
"Message body offset lookup failed");
return -1;
}
return 1;
}
static int
mbox_sync_seek_to_uid(struct mbox_sync_context *sync_ctx, uint32_t uid)
{
struct mail_index_view *sync_view = sync_ctx->sync_view;
uint32_t seq1, seq2;
uoff_t size;
int ret;
i_assert(!sync_ctx->index_reset);
if (!mail_index_lookup_seq_range(sync_view, uid, (uint32_t)-1,
&seq1, &seq2)) {
ret = i_stream_get_size(sync_ctx->file_input, TRUE, &size);
if (ret < 0) {
mbox_istream_set_syscall_error(sync_ctx->mbox,
sync_ctx->file_input, "i_stream_get_size()");
return -1;
}
i_assert(ret != 0);
if (istream_raw_mbox_seek(sync_ctx->mbox->mbox_stream,
size) < 0) {
mbox_sync_set_critical(sync_ctx,
"Error seeking to end of mbox");
return -1;
}
sync_ctx->idx_seq =
mail_index_view_get_messages_count(sync_view) + 1;
return 1;
}
return mbox_sync_seek_to_seq(sync_ctx, seq1);
}
static int mbox_sync_partial_seek_next(struct mbox_sync_context *sync_ctx,
uint32_t next_uid, bool *partial,
bool *skipped_mails)
{
uint32_t messages_count, uid;
int ret;
i_assert(!sync_ctx->index_reset);
index_sync_changes_delete_to(sync_ctx->sync_changes, next_uid);
if (index_sync_changes_have(sync_ctx->sync_changes))
return 1;
if (sync_ctx->hdr->first_recent_uid <= next_uid &&
!sync_ctx->keep_recent) {
return 1;
}
uid = index_sync_changes_get_next_uid(sync_ctx->sync_changes);
if (sync_ctx->hdr->first_recent_uid < sync_ctx->hdr->next_uid &&
(uid > sync_ctx->hdr->first_recent_uid || uid == 0) &&
!sync_ctx->keep_recent) {
uid = sync_ctx->hdr->first_recent_uid;
}
if (uid != 0) {
if (uid != next_uid) {
*skipped_mails = TRUE;
next_uid = uid;
}
ret = mbox_sync_seek_to_uid(sync_ctx, next_uid);
} else {
if (sync_ctx->mbox->mbox_hdr.dirty_flag == 0)
return 0;
messages_count =
mail_index_view_get_messages_count(sync_ctx->sync_view);
if (sync_ctx->seq + 1 != messages_count) {
ret = mbox_sync_seek_to_seq(sync_ctx, messages_count);
*skipped_mails = TRUE;
} else {
ret = 1;
}
*partial = FALSE;
}
if (ret == 0) {
*partial = FALSE;
ret = 1;
}
return ret;
}
static void mbox_sync_hdr_update(struct mbox_sync_context *sync_ctx,
struct mbox_sync_mail_context *mail_ctx)
{
const struct mailbox_update *update = sync_ctx->mbox->sync_hdr_update;
if (update->uid_validity != 0) {
sync_ctx->base_uid_validity = update->uid_validity;
mail_ctx->imapbase_rewrite = TRUE;
mail_ctx->need_rewrite = TRUE;
}
if (update->min_next_uid != 0 &&
sync_ctx->base_uid_last+1 < update->min_next_uid) {
i_assert(sync_ctx->next_uid <= update->min_next_uid);
sync_ctx->base_uid_last = update->min_next_uid-1;
sync_ctx->next_uid = update->min_next_uid;
mail_ctx->imapbase_rewrite = TRUE;
mail_ctx->need_rewrite = TRUE;
}
}
static bool mbox_sync_imapbase(struct mbox_sync_context *sync_ctx,
struct mbox_sync_mail_context *mail_ctx)
{
struct event *event = sync_ctx->mbox->box.event;
if (sync_ctx->base_uid_validity != 0 &&
sync_ctx->hdr->uid_validity != 0 &&
sync_ctx->base_uid_validity != sync_ctx->hdr->uid_validity) {
e_warning(event,
"UIDVALIDITY changed (%u -> %u)",
sync_ctx->hdr->uid_validity,
sync_ctx->base_uid_validity);
sync_ctx->index_reset = TRUE;
return TRUE;
}
if (sync_ctx->mbox->sync_hdr_update != NULL)
mbox_sync_hdr_update(sync_ctx, mail_ctx);
return FALSE;
}
static int mbox_sync_loop(struct mbox_sync_context *sync_ctx,
struct mbox_sync_mail_context *mail_ctx,
bool partial)
{
const struct mail_index_record *rec;
uint32_t uid, messages_count;
uoff_t offset;
int ret;
bool expunged, skipped_mails, uids_broken;
messages_count =
mail_index_view_get_messages_count(sync_ctx->sync_view);
ret = mbox_sync_seek_to_seq(sync_ctx, 0);
if (ret <= 0)
return ret;
if (sync_ctx->renumber_uids) {
while (sync_ctx->idx_seq <= messages_count) {
mail_index_expunge(sync_ctx->t,
sync_ctx->idx_seq++);
}
}
skipped_mails = uids_broken = FALSE;
while ((ret = mbox_sync_read_next_mail(sync_ctx, mail_ctx)) > 0) {
uid = mail_ctx->mail.uid;
if (mail_ctx->seq == 1) {
if (mbox_sync_imapbase(sync_ctx, mail_ctx)) {
sync_ctx->mbox->mbox_hdr.dirty_flag = 1;
return 0;
}
}
if (mail_ctx->mail.uid_broken && partial) {
if (sync_ctx->mbox->mbox_hdr.dirty_flag != 0)
return 0;
mbox_sync_set_critical(sync_ctx,
"UIDs broken with partial sync");
sync_ctx->mbox->mbox_hdr.dirty_flag = 1;
return 0;
}
if (mail_ctx->mail.uid_broken)
uids_broken = TRUE;
if (mail_ctx->mail.pseudo)
uid = 0;
rec = NULL; ret = 1;
if (uid != 0) {
if (!mbox_sync_read_index_rec(sync_ctx, uid, &rec))
ret = 0;
}
if (ret == 0) {
uid = 0;
} else if (uid == 0 &&
!mail_ctx->mail.pseudo &&
(sync_ctx->delay_writes ||
sync_ctx->idx_seq <= messages_count)) {
sync_ctx->mbox->mbox_save_md5 = TRUE;
mbox_sync_find_index_md5(sync_ctx,
mail_ctx->hdr_md5_sum, &rec);
if (rec != NULL)
uid = mail_ctx->mail.uid = rec->uid;
}
mbox_sync_read_index_syncs(sync_ctx,
mail_ctx->mail.pseudo ? 1 : uid,
&expunged);
if (mail_ctx->mail.pseudo) {
expunged = FALSE;
} else {
if (rec == NULL) {
partial = FALSE;
}
}
if (uid == 0 && !mail_ctx->mail.pseudo) {
while (sync_ctx->idx_seq <= messages_count) {
mail_index_expunge(sync_ctx->t,
sync_ctx->idx_seq++);
}
if (sync_ctx->next_uid == (uint32_t)-1) {
mailbox_set_critical(&sync_ctx->mbox->box,
"Out of UIDs, renumbering them in mbox");
sync_ctx->renumber_uids = TRUE;
return 0;
}
mail_ctx->need_rewrite = TRUE;
mail_ctx->mail.uid = sync_ctx->next_uid++;
}
sync_ctx->prev_msg_uid = mail_ctx->mail.uid;
if (!mail_ctx->mail.pseudo)
mail_ctx->mail.idx_seq = sync_ctx->idx_seq;
if (!expunged) {
if (!mail_ctx->mail.pseudo) T_BEGIN {
mbox_sync_update_flags(mail_ctx, rec);
} T_END;
if (mbox_sync_handle_header(mail_ctx) < 0)
return -1;
sync_ctx->dest_first_mail = FALSE;
} else {
mbox_sync_handle_expunge(mail_ctx);
}
if (!mail_ctx->mail.pseudo) {
if (!expunged) T_BEGIN {
mbox_sync_update_index(mail_ctx, rec);
} T_END;
sync_ctx->idx_seq++;
}
if (istream_raw_mbox_next(sync_ctx->input,
mail_ctx->mail.body_size) < 0)
return -1;
offset = istream_raw_mbox_get_start_offset(sync_ctx->input);
if (sync_ctx->need_space_seq != 0) {
if (mbox_sync_handle_missing_space(mail_ctx) < 0)
return -1;
if (mbox_sync_seek(sync_ctx, offset) < 0)
return -1;
} else if (sync_ctx->expunged_space > 0) {
if (!expunged) {
mbox_sync_file_update_ext_modified(sync_ctx);
if (mbox_move(sync_ctx,
mail_ctx->body_offset -
sync_ctx->expunged_space,
mail_ctx->body_offset,
mail_ctx->mail.body_size) < 0)
return -1;
if (mbox_sync_seek(sync_ctx, offset) < 0)
return -1;
}
} else if (partial) {
ret = mbox_sync_partial_seek_next(sync_ctx, uid + 1,
&partial,
&skipped_mails);
if (ret <= 0)
break;
}
}
if (ret < 0)
return -1;
if (istream_raw_mbox_is_eof(sync_ctx->input)) {
while (sync_ctx->idx_seq <= messages_count)
mail_index_expunge(sync_ctx->t, sync_ctx->idx_seq++);
}
if (!skipped_mails)
sync_ctx->mbox->mbox_hdr.dirty_flag = 0;
sync_ctx->mbox->mbox_broken_offsets = FALSE;
if (uids_broken && sync_ctx->delay_writes) {
sync_ctx->mbox->mbox_hdr.dirty_flag = 1;
}
return 1;
}
static int mbox_write_pseudo(struct mbox_sync_context *sync_ctx, bool force)
{
string_t *str;
unsigned int uid_validity;
i_assert(sync_ctx->write_fd != -1);
if (sync_ctx->mbox->sync_hdr_update != NULL) {
const struct mailbox_update *update =
sync_ctx->mbox->sync_hdr_update;
bool change = FALSE;
if (update->uid_validity != 0) {
sync_ctx->base_uid_validity = update->uid_validity;
change = TRUE;
}
if (update->min_next_uid != 0) {
sync_ctx->base_uid_last = update->min_next_uid-1;
change = TRUE;
}
if (!change && !force)
return 0;
}
uid_validity = sync_ctx->base_uid_validity != 0 ?
sync_ctx->base_uid_validity : sync_ctx->hdr->uid_validity;
i_assert(uid_validity != 0);
str = t_str_new(1024);
str_printfa(str, "%sDate: %s\n"
"From: Mail System Internal Data <MAILER-DAEMON@%s>\n"
"Subject: DON'T DELETE THIS MESSAGE -- FOLDER INTERNAL DATA"
"\nMessage-ID: <%s@%s>\n"
"X-IMAP: %u %010u\n"
"Status: RO\n"
"\n"
PSEUDO_MESSAGE_BODY
"\n",
mbox_from_create("MAILER_DAEMON", ioloop_time),
message_date_create(ioloop_time),
my_hostname, dec2str(ioloop_time), my_hostname,
uid_validity, sync_ctx->next_uid-1);
if (pwrite_full(sync_ctx->write_fd,
str_data(str), str_len(str), 0) < 0) {
if (!ENOSPACE(errno)) {
mbox_set_syscall_error(sync_ctx->mbox,
"pwrite_full()");
return -1;
}
if (ftruncate(sync_ctx->write_fd, 0) < 0)
mbox_set_syscall_error(sync_ctx->mbox, "ftruncate()");
}
sync_ctx->base_uid_validity = uid_validity;
sync_ctx->base_uid_last_offset = 0;
sync_ctx->base_uid_last = sync_ctx->next_uid-1;
return 0;
}
static int mbox_append_zero(struct mbox_sync_context *sync_ctx,
uoff_t orig_file_size, uoff_t count)
{
char block[IO_BLOCK_SIZE];
uoff_t offset = orig_file_size;
ssize_t ret = 0;
memset(block, 0, I_MIN(sizeof(block), count));
while (count > 0) {
ret = pwrite(sync_ctx->write_fd, block,
I_MIN(sizeof(block), count), offset);
if (ret < 0)
break;
offset += ret;
count -= ret;
}
if (ret < 0) {
mbox_set_syscall_error(sync_ctx->mbox, "pwrite()");
if (ftruncate(sync_ctx->write_fd, orig_file_size) < 0)
mbox_set_syscall_error(sync_ctx->mbox, "ftruncate()");
return -1;
}
return 0;
}
static int mbox_sync_handle_eof_updates(struct mbox_sync_context *sync_ctx,
struct mbox_sync_mail_context *mail_ctx)
{
uoff_t file_size, offset, trailer_size;
int ret;
if (!istream_raw_mbox_is_eof(sync_ctx->input)) {
i_assert(sync_ctx->need_space_seq == 0);
i_assert(sync_ctx->expunged_space == 0);
return 0;
}
ret = i_stream_get_size(sync_ctx->file_input, TRUE, &file_size);
if (ret < 0) {
mbox_istream_set_syscall_error(sync_ctx->mbox,
sync_ctx->file_input, "i_stream_get_size()");
return -1;
}
if (ret == 0) {
return 0;
}
if (file_size < sync_ctx->file_input->v_offset) {
mbox_sync_set_critical(sync_ctx,
"file size unexpectedly shrank "
"(%"PRIuUOFF_T" vs %"PRIuUOFF_T")", file_size,
sync_ctx->file_input->v_offset);
return -1;
}
trailer_size = file_size - sync_ctx->file_input->v_offset;
i_assert(trailer_size <= 2);
if (sync_ctx->need_space_seq != 0) {
i_assert(sync_ctx->write_fd != -1);
i_assert(sync_ctx->space_diff < 0);
off_t padding = MBOX_HEADER_PADDING *
(off_t)(sync_ctx->seq - sync_ctx->need_space_seq + 1);
sync_ctx->space_diff -= padding;
i_assert(sync_ctx->expunged_space <= -sync_ctx->space_diff);
sync_ctx->space_diff += sync_ctx->expunged_space;
sync_ctx->expunged_space = 0;
if (mail_ctx->have_eoh && !mail_ctx->updated)
str_append_c(mail_ctx->header, '\n');
i_assert(sync_ctx->space_diff < 0);
if (mbox_append_zero(sync_ctx, file_size,
-sync_ctx->space_diff) < 0)
return -1;
mbox_sync_file_updated(sync_ctx, FALSE);
if (mbox_sync_rewrite(sync_ctx, mail_ctx, file_size,
-sync_ctx->space_diff, padding,
sync_ctx->need_space_seq,
sync_ctx->seq) < 0)
return -1;
update_from_offsets(sync_ctx);
sync_ctx->need_space_seq = 0;
array_clear(&sync_ctx->mails);
p_clear(sync_ctx->saved_keywords_pool);
}
if (sync_ctx->expunged_space > 0) {
i_assert(sync_ctx->write_fd != -1);
mbox_sync_file_update_ext_modified(sync_ctx);
file_size = sync_ctx->last_stat.st_size;
if (file_size == (uoff_t)sync_ctx->expunged_space) {
trailer_size = 0;
} else if (sync_ctx->expunged_space == (off_t)file_size + 1 ||
sync_ctx->expunged_space == (off_t)file_size + 2) {
trailer_size = 0;
sync_ctx->expunged_space = file_size;
}
i_assert(file_size >= sync_ctx->expunged_space + trailer_size);
offset = file_size - sync_ctx->expunged_space - trailer_size;
i_assert(offset == 0 || offset > 31);
if (mbox_move(sync_ctx, offset,
offset + sync_ctx->expunged_space,
trailer_size) < 0)
return -1;
if (ftruncate(sync_ctx->write_fd,
offset + trailer_size) < 0) {
mbox_set_syscall_error(sync_ctx->mbox, "ftruncate()");
return -1;
}
if (offset == 0) {
if (mbox_write_pseudo(sync_ctx, TRUE) < 0)
return -1;
}
sync_ctx->expunged_space = 0;
mbox_sync_file_updated(sync_ctx, FALSE);
} else {
if (file_size == 0 && sync_ctx->mbox->sync_hdr_update != NULL) {
if (mbox_write_pseudo(sync_ctx, FALSE) < 0)
return -1;
}
}
return 0;
}
static void
mbox_sync_index_update_ext_header(struct mbox_mailbox *mbox,
struct mail_index_transaction *trans)
{
const struct mailbox_update *update = mbox->sync_hdr_update;
const void *data;
size_t data_size;
if (update != NULL && !guid_128_is_empty(update->mailbox_guid)) {
memcpy(mbox->mbox_hdr.mailbox_guid, update->mailbox_guid,
sizeof(mbox->mbox_hdr.mailbox_guid));
} else if (guid_128_is_empty(mbox->mbox_hdr.mailbox_guid)) {
guid_128_generate(mbox->mbox_hdr.mailbox_guid);
}
mail_index_get_header_ext(mbox->box.view, mbox->mbox_ext_idx,
&data, &data_size);
if (data_size != sizeof(mbox->mbox_hdr) ||
memcmp(data, &mbox->mbox_hdr, data_size) != 0) {
if (data_size != sizeof(mbox->mbox_hdr)) {
mail_index_ext_resize(trans, mbox->mbox_ext_idx,
sizeof(mbox->mbox_hdr),
sizeof(uint64_t),
sizeof(uint64_t));
}
mail_index_update_header_ext(trans, mbox->mbox_ext_idx,
0, &mbox->mbox_hdr,
sizeof(mbox->mbox_hdr));
}
}
static uint32_t mbox_get_uidvalidity_next(struct mailbox_list *list)
{
const char *path;
path = mailbox_list_get_root_forced(list, MAILBOX_LIST_PATH_TYPE_CONTROL);
path = t_strconcat(path, "/"MBOX_UIDVALIDITY_FNAME, NULL);
return mailbox_uidvalidity_next(list, path);
}
static int mbox_sync_update_index_header(struct mbox_sync_context *sync_ctx)
{
struct mail_index_view *view;
const struct stat *st;
uint32_t first_recent_uid, seq, seq2;
if (i_stream_stat(sync_ctx->file_input, FALSE, &st) < 0) {
mbox_istream_set_syscall_error(sync_ctx->mbox,
sync_ctx->file_input, "i_stream_stat()");
return -1;
}
if (sync_ctx->moved_offsets &&
((uint64_t)st->st_size == sync_ctx->mbox->mbox_hdr.sync_size ||
(uint64_t)st->st_size == sync_ctx->orig_size)) {
while (sync_ctx->orig_mtime == st->st_mtime) {
i_sleep_msecs(500);
if (utime(mailbox_get_path(&sync_ctx->mbox->box), NULL) < 0) {
mbox_set_syscall_error(sync_ctx->mbox,
"utime()");
return -1;
}
if (i_stream_stat(sync_ctx->file_input, FALSE, &st) < 0) {
mbox_istream_set_syscall_error(sync_ctx->mbox,
sync_ctx->file_input, "i_stream_stat()");
return -1;
}
}
}
sync_ctx->mbox->mbox_hdr.sync_mtime = st->st_mtime;
sync_ctx->mbox->mbox_hdr.sync_size = st->st_size;
mbox_sync_index_update_ext_header(sync_ctx->mbox, sync_ctx->t);
i_assert(sync_ctx->base_uid_validity != 0 || st->st_size <= 0);
if (sync_ctx->base_uid_validity == 0) {
sync_ctx->base_uid_validity = sync_ctx->hdr->uid_validity != 0 ?
sync_ctx->hdr->uid_validity :
mbox_get_uidvalidity_next(sync_ctx->mbox->box.list);
}
if (sync_ctx->base_uid_validity != sync_ctx->hdr->uid_validity) {
mail_index_update_header(sync_ctx->t,
offsetof(struct mail_index_header, uid_validity),
&sync_ctx->base_uid_validity,
sizeof(sync_ctx->base_uid_validity), TRUE);
}
if (istream_raw_mbox_is_eof(sync_ctx->input) &&
sync_ctx->next_uid != sync_ctx->hdr->next_uid) {
i_assert(sync_ctx->next_uid != 0);
mail_index_update_header(sync_ctx->t,
offsetof(struct mail_index_header, next_uid),
&sync_ctx->next_uid, sizeof(sync_ctx->next_uid), FALSE);
}
if (sync_ctx->last_nonrecent_uid < sync_ctx->hdr->first_recent_uid) {
sync_ctx->last_nonrecent_uid =
sync_ctx->hdr->first_recent_uid - 1;
}
view = mail_index_transaction_open_updated_view(sync_ctx->t);
if (mail_index_lookup_seq_range(view, sync_ctx->last_nonrecent_uid + 1,
(uint32_t)-1, &seq, &seq2)) {
mailbox_recent_flags_set_seqs(&sync_ctx->mbox->box,
view, seq, seq2);
}
mail_index_view_close(&view);
first_recent_uid = !sync_ctx->keep_recent ?
sync_ctx->next_uid : sync_ctx->last_nonrecent_uid + 1;
if (sync_ctx->hdr->first_recent_uid < first_recent_uid) {
mail_index_update_header(sync_ctx->t,
offsetof(struct mail_index_header, first_recent_uid),
&first_recent_uid, sizeof(first_recent_uid), FALSE);
}
return 0;
}
static void mbox_sync_restart(struct mbox_sync_context *sync_ctx)
{
sync_ctx->base_uid_validity = 0;
sync_ctx->base_uid_last = 0;
sync_ctx->base_uid_last_offset = 0;
array_clear(&sync_ctx->mails);
p_clear(sync_ctx->saved_keywords_pool);
index_sync_changes_reset(sync_ctx->sync_changes);
mail_index_sync_reset(sync_ctx->index_sync_ctx);
mail_index_transaction_reset(sync_ctx->t);
if (sync_ctx->index_reset) {
mail_index_reset(sync_ctx->t);
sync_ctx->reset_hdr.next_uid = 1;
sync_ctx->hdr = &sync_ctx->reset_hdr;
mailbox_recent_flags_reset(&sync_ctx->mbox->box);
}
sync_ctx->prev_msg_uid = 0;
sync_ctx->next_uid = sync_ctx->hdr->next_uid;
sync_ctx->idx_next_uid = sync_ctx->hdr->next_uid;
sync_ctx->seq = 0;
sync_ctx->idx_seq = 1;
sync_ctx->need_space_seq = 0;
sync_ctx->expunged_space = 0;
sync_ctx->space_diff = 0;
sync_ctx->dest_first_mail = TRUE;
sync_ctx->ext_modified = FALSE;
sync_ctx->errors = FALSE;
}
static int mbox_sync_do(struct mbox_sync_context *sync_ctx,
enum mbox_sync_flags flags)
{
struct mbox_index_header *mbox_hdr = &sync_ctx->mbox->mbox_hdr;
struct mbox_sync_mail_context mail_ctx;
const struct stat *st;
unsigned int i;
bool partial;
int ret;
if (i_stream_stat(sync_ctx->file_input, FALSE, &st) < 0) {
mbox_istream_set_syscall_error(sync_ctx->mbox,
sync_ctx->file_input, "i_stream_stat()");
return -1;
}
sync_ctx->last_stat = *st;
sync_ctx->orig_size = st->st_size;
sync_ctx->orig_atime = st->st_atime;
sync_ctx->orig_mtime = st->st_mtime;
if ((flags & MBOX_SYNC_FORCE_SYNC) != 0) {
partial = FALSE;
mbox_hdr->dirty_flag = 1;
} else if ((uint32_t)st->st_mtime == mbox_hdr->sync_mtime &&
(uint64_t)st->st_size == mbox_hdr->sync_size) {
if (mbox_hdr->dirty_flag != 0 && (flags & MBOX_SYNC_UNDIRTY) != 0)
partial = FALSE;
else
partial = TRUE;
} else if ((flags & MBOX_SYNC_UNDIRTY) != 0 ||
(uint64_t)st->st_size == mbox_hdr->sync_size) {
partial = FALSE;
sync_ctx->mbox->mbox_hdr.dirty_flag = 1;
} else {
partial = TRUE;
sync_ctx->mbox->mbox_hdr.dirty_flag = 1;
}
mbox_sync_restart(sync_ctx);
for (i = 0;;) {
ret = mbox_sync_loop(sync_ctx, &mail_ctx, partial);
if (ret > 0 && !sync_ctx->errors)
break;
if (ret < 0)
return -1;
if (sync_ctx->delay_writes &&
(sync_ctx->errors || sync_ctx->renumber_uids)) {
if (!sync_ctx->readonly)
sync_ctx->delay_writes = FALSE;
}
if (++i == 3)
break;
mbox_sync_restart(sync_ctx);
partial = FALSE;
}
if (mbox_sync_handle_eof_updates(sync_ctx, &mail_ctx) < 0)
return -1;
index_sync_changes_reset(sync_ctx->sync_changes);
if (sync_ctx->base_uid_last != sync_ctx->next_uid-1 &&
ret > 0 && !sync_ctx->delay_writes &&
sync_ctx->base_uid_last_offset != 0) {
ret = mbox_rewrite_base_uid_last(sync_ctx);
} else {
ret = 0;
}
if (mbox_sync_update_index_header(sync_ctx) < 0)
return -1;
return ret;
}
int mbox_sync_header_refresh(struct mbox_mailbox *mbox)
{
const void *data;
size_t data_size;
if (mail_index_refresh(mbox->box.index) < 0) {
mailbox_set_index_error(&mbox->box);
return -1;
}
mail_index_get_header_ext(mbox->box.view, mbox->mbox_ext_idx,
&data, &data_size);
if (data_size == 0) {
i_zero(&mbox->mbox_hdr);
return 0;
}
memcpy(&mbox->mbox_hdr, data, I_MIN(sizeof(mbox->mbox_hdr), data_size));
if (mbox->mbox_broken_offsets)
mbox->mbox_hdr.dirty_flag = 1;
return 0;
}
int mbox_sync_get_guid(struct mbox_mailbox *mbox)
{
struct mail_index_transaction *trans;
unsigned int lock_id;
int ret;
if (mbox_lock(mbox, F_WRLCK, &lock_id) <= 0)
return -1;
ret = mbox_sync_header_refresh(mbox);
if (ret == 0) {
trans = mail_index_transaction_begin(mbox->box.view,
MAIL_INDEX_TRANSACTION_FLAG_EXTERNAL);
mbox_sync_index_update_ext_header(mbox, trans);
ret = mail_index_transaction_commit(&trans);
}
mbox_unlock(mbox, lock_id);
return ret;
}
int mbox_sync_has_changed(struct mbox_mailbox *mbox, bool leave_dirty)
{
const struct stat *st;
struct stat statbuf;
if (mbox->mbox_file_stream != NULL && mbox->mbox_fd == -1) {
if (i_stream_stat(mbox->mbox_file_stream, FALSE, &st) < 0) {
if (errno == ENOENT) {
mailbox_set_deleted(&mbox->box);
return 0;
}
mbox_istream_set_syscall_error(mbox,
mbox->mbox_file_stream, "i_stream_stat()");
return -1;
}
} else {
if (stat(mailbox_get_path(&mbox->box), &statbuf) < 0) {
if (errno == ENOENT) {
mailbox_set_deleted(&mbox->box);
return 0;
}
mbox_set_syscall_error(mbox, "stat()");
return -1;
}
st = &statbuf;
}
if (mbox_sync_header_refresh(mbox) < 0)
return -1;
if (guid_128_is_empty(mbox->mbox_hdr.mailbox_guid)) {
return 1;
}
if ((uint32_t)st->st_mtime == mbox->mbox_hdr.sync_mtime &&
(uint64_t)st->st_size == mbox->mbox_hdr.sync_size) {
if (mbox->mbox_hdr.dirty_flag != 0 || leave_dirty)
return 0;
}
return 1;
}
static void mbox_sync_context_free(struct mbox_sync_context *sync_ctx)
{
index_sync_changes_deinit(&sync_ctx->sync_changes);
index_storage_expunging_deinit(&sync_ctx->mbox->box);
if (sync_ctx->index_sync_ctx != NULL)
mail_index_sync_rollback(&sync_ctx->index_sync_ctx);
pool_unref(&sync_ctx->mail_keyword_pool);
pool_unref(&sync_ctx->saved_keywords_pool);
str_free(&sync_ctx->header);
str_free(&sync_ctx->from_line);
array_free(&sync_ctx->mails);
}
static int mbox_sync_int(struct mbox_mailbox *mbox, enum mbox_sync_flags flags,
unsigned int *lock_id)
{
struct mail_index_sync_ctx *index_sync_ctx;
struct mail_index_view *sync_view;
struct mail_index_transaction *trans;
struct mbox_sync_context sync_ctx;
enum mail_index_sync_flags sync_flags;
int ret;
bool changed, delay_writes, readonly;
readonly = mbox_is_backend_readonly(mbox) ||
(flags & MBOX_SYNC_READONLY) != 0;
delay_writes = readonly ||
((flags & MBOX_SYNC_REWRITE) == 0 &&
mbox->storage->set->mbox_lazy_writes);
if (!mbox->storage->set->mbox_dirty_syncs &&
!mbox->storage->set->mbox_very_dirty_syncs)
flags |= MBOX_SYNC_UNDIRTY;
if ((flags & MBOX_SYNC_LOCK_READING) != 0) {
if (mbox_lock(mbox, F_RDLCK, lock_id) <= 0)
return -1;
}
if ((flags & MBOX_SYNC_HEADER) != 0 ||
(flags & MBOX_SYNC_FORCE_SYNC) != 0) {
if (mbox_sync_header_refresh(mbox) < 0)
return -1;
changed = TRUE;
} else {
bool leave_dirty = (flags & MBOX_SYNC_UNDIRTY) == 0;
if ((ret = mbox_sync_has_changed(mbox, leave_dirty)) < 0)
return -1;
changed = ret > 0;
}
if ((flags & MBOX_SYNC_LOCK_READING) != 0) {
if (!changed)
return 0;
mbox_unlock(mbox, *lock_id);
*lock_id = 0;
}
if (mbox->mbox_stream != NULL)
i_stream_sync(mbox->mbox_stream);
if (mbox->mbox_file_stream != NULL)
i_stream_sync(mbox->mbox_file_stream);
again:
if (changed) {
int lock_type = readonly ? F_RDLCK : F_WRLCK;
if ((ret = mbox_lock(mbox, lock_type, lock_id)) <= 0) {
if (ret == 0 || lock_type == F_RDLCK)
return -1;
if (mbox_lock(mbox, F_RDLCK, lock_id) <= 0)
return -1;
mbox->backend_readonly = readonly = TRUE;
mbox->backend_readonly_set = TRUE;
delay_writes = TRUE;
}
}
sync_flags = index_storage_get_sync_flags(&mbox->box);
if ((flags & MBOX_SYNC_REWRITE) != 0)
sync_flags |= MAIL_INDEX_SYNC_FLAG_FLUSH_DIRTY;
ret = index_storage_expunged_sync_begin(&mbox->box, &index_sync_ctx,
&sync_view, &trans, sync_flags);
if (ret <= 0)
return ret;
if ((mbox->box.flags & MAILBOX_FLAG_DROP_RECENT) != 0) {
sync_ctx.hdr = mail_index_get_header(sync_view);
if (sync_ctx.hdr->first_recent_uid < sync_ctx.hdr->next_uid)
changed = TRUE;
}
if (!changed && !mail_index_sync_have_more(index_sync_ctx)) {
nothing_to_do:
index_storage_expunging_deinit(&mbox->box);
if (mail_index_sync_commit(&index_sync_ctx) < 0) {
mailbox_set_index_error(&mbox->box);
return -1;
}
return 0;
}
i_zero(&sync_ctx);
sync_ctx.mbox = mbox;
sync_ctx.keep_recent =
(mbox->box.flags & MAILBOX_FLAG_DROP_RECENT) == 0;
sync_ctx.hdr = mail_index_get_header(sync_view);
sync_ctx.from_line = str_new(default_pool, 256);
sync_ctx.header = str_new(default_pool, 4096);
sync_ctx.index_sync_ctx = index_sync_ctx;
sync_ctx.sync_view = sync_view;
sync_ctx.t = trans;
sync_ctx.mail_keyword_pool =
pool_alloconly_create("mbox keywords", 512);
sync_ctx.saved_keywords_pool =
pool_alloconly_create("mbox saved keywords", 4096);
(void)mail_index_get_keywords(mbox->box.index);
i_array_init(&sync_ctx.mails, 64);
sync_ctx.flags = flags;
sync_ctx.readonly = readonly;
sync_ctx.delay_writes = delay_writes;
sync_ctx.sync_changes =
index_sync_changes_init(index_sync_ctx, sync_view, trans,
sync_ctx.delay_writes);
if (!changed && delay_writes) {
bool expunged;
uint32_t uid;
mbox_sync_read_index_syncs(&sync_ctx, 1, &expunged);
uid = expunged ? 1 :
index_sync_changes_get_next_uid(sync_ctx.sync_changes);
if (uid == 0) {
sync_ctx.index_sync_ctx = NULL;
mbox_sync_context_free(&sync_ctx);
goto nothing_to_do;
}
}
if (*lock_id == 0) {
mbox_sync_context_free(&sync_ctx);
changed = TRUE;
goto again;
}
if (mbox_file_open_stream(mbox) < 0) {
mbox_sync_context_free(&sync_ctx);
return -1;
}
sync_ctx.file_input = sync_ctx.mbox->mbox_file_stream;
sync_ctx.input = sync_ctx.mbox->mbox_stream;
sync_ctx.write_fd = sync_ctx.mbox->mbox_lock_type != F_WRLCK ? -1 :
sync_ctx.mbox->mbox_fd;
ret = mbox_sync_do(&sync_ctx, flags);
if (ret < 0)
mail_index_sync_rollback(&index_sync_ctx);
else if (mail_index_sync_commit(&index_sync_ctx) < 0) {
mailbox_set_index_error(&mbox->box);
ret = -1;
}
sync_ctx.t = NULL;
sync_ctx.index_sync_ctx = NULL;
if (ret == 0 && mbox->mbox_fd != -1 && sync_ctx.keep_recent &&
!readonly) {
struct utimbuf buf;
struct stat st;
if (fstat(mbox->mbox_fd, &st) < 0)
mbox_set_syscall_error(mbox, "fstat()");
else {
buf.modtime = st.st_mtime;
buf.actime = sync_ctx.orig_atime;
if (utime(mailbox_get_path(&mbox->box), &buf) < 0 &&
!ENOACCESS(errno))
mbox_set_syscall_error(mbox, "utime()");
}
}
i_assert(*lock_id != 0);
if (mbox->storage->storage.set->mail_nfs_storage &&
mbox->mbox_fd != -1) {
if (fdatasync(mbox->mbox_fd) < 0) {
mbox_set_syscall_error(mbox, "fdatasync()");
ret = -1;
}
}
mbox_sync_context_free(&sync_ctx);
return ret;
}
int mbox_sync(struct mbox_mailbox *mbox, enum mbox_sync_flags flags)
{
unsigned int lock_id = 0;
int ret;
i_assert(mbox->mbox_lock_type != F_RDLCK ||
(flags & MBOX_SYNC_READONLY) != 0);
mbox->syncing = TRUE;
ret = mbox_sync_int(mbox, flags, &lock_id);
mbox->syncing = FALSE;
if (lock_id != 0) {
if (ret < 0) {
mbox_unlock(mbox, lock_id);
} else if ((flags & MBOX_SYNC_LOCK_READING) == 0) {
if (mbox_unlock(mbox, lock_id) < 0)
ret = -1;
} else if (mbox->mbox_lock_type != F_RDLCK) {
unsigned int read_lock_id = 0;
if (mbox_lock(mbox, F_RDLCK, &read_lock_id) <= 0)
ret = -1;
if (mbox_unlock(mbox, lock_id) < 0)
ret = -1;
}
}
mailbox_sync_notify(&mbox->box, 0, 0);
return ret;
}
struct mailbox_sync_context *
mbox_storage_sync_init(struct mailbox *box, enum mailbox_sync_flags flags)
{
struct mbox_mailbox *mbox = MBOX_MAILBOX(box);
enum mbox_sync_flags mbox_sync_flags = 0;
int ret = 0;
if (index_mailbox_want_full_sync(&mbox->box, flags)) {
if ((flags & MAILBOX_SYNC_FLAG_FULL_READ) != 0 &&
!mbox->storage->set->mbox_very_dirty_syncs)
mbox_sync_flags |= MBOX_SYNC_UNDIRTY;
if ((flags & MAILBOX_SYNC_FLAG_FULL_WRITE) != 0)
mbox_sync_flags |= MBOX_SYNC_REWRITE;
if ((flags & MAILBOX_SYNC_FLAG_FORCE_RESYNC) != 0) {
mbox_sync_flags |= MBOX_SYNC_UNDIRTY |
MBOX_SYNC_REWRITE | MBOX_SYNC_FORCE_SYNC;
}
ret = mbox_sync(mbox, mbox_sync_flags);
}
return index_mailbox_sync_init(box, flags, ret < 0);
}