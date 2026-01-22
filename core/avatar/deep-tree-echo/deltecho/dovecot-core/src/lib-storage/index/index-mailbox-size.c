#include "lib.h"
#include "str.h"
#include "strescape.h"
#include "net.h"
#include "write-full.h"
#include "mail-search-build.h"
#include "index-storage.h"
#include "index-mailbox-size.h"
#define VSIZE_LOCK_SUFFIX "dovecot-vsize.lock"
#define VSIZE_UPDATE_MAX_LOCK_SECS 10
#define INDEXER_SOCKET_NAME "indexer"
#define INDEXER_HANDSHAKE "VERSION\tindexer-client\t1\t0\n"
struct mailbox_vsize_update {
struct mailbox *box;
struct mail_index_view *view;
struct mailbox_index_vsize vsize_hdr, orig_vsize_hdr;
struct file_lock *lock;
bool lock_failed;
bool skip_write;
bool rebuild;
bool written;
bool finish_in_background;
};
static void vsize_header_refresh(struct mailbox_vsize_update *update)
{
const void *data;
size_t size;
if (update->view != NULL)
mail_index_view_close(&update->view);
(void)mail_index_refresh(update->box->index);
update->view = mail_index_view_open(update->box->index);
mail_index_get_header_ext(update->view, update->box->vsize_hdr_ext_id,
&data, &size);
if (size > 0) {
memcpy(&update->orig_vsize_hdr, data,
I_MIN(size, sizeof(update->orig_vsize_hdr)));
}
if (size == sizeof(update->vsize_hdr))
memcpy(&update->vsize_hdr, data, sizeof(update->vsize_hdr));
else {
if (size != 0) {
mailbox_set_critical(update->box,
"vsize-hdr has invalid size: %zu",
size);
}
update->rebuild = TRUE;
i_zero(&update->vsize_hdr);
}
}
static void
index_mailbox_vsize_check_rebuild(struct mailbox_vsize_update *update)
{
uint32_t seq1, seq2;
if (update->vsize_hdr.highest_uid == 0)
return;
if (!mail_index_lookup_seq_range(update->view, 1,
update->vsize_hdr.highest_uid,
&seq1, &seq2))
seq2 = 0;
if (update->vsize_hdr.message_count != seq2) {
if (update->vsize_hdr.message_count < seq2) {
mailbox_set_critical(update->box,
"vsize-hdr has invalid message-count (%u < %u)",
update->vsize_hdr.message_count, seq2);
} else {
}
i_zero(&update->vsize_hdr);
update->rebuild = TRUE;
}
}
struct mailbox_vsize_update *
index_mailbox_vsize_update_init(struct mailbox *box)
{
struct mailbox_vsize_update *update;
i_assert(box->opened);
update = i_new(struct mailbox_vsize_update, 1);
update->box = box;
vsize_header_refresh(update);
return update;
}
static bool vsize_update_lock_full(struct mailbox_vsize_update *update,
unsigned int lock_secs)
{
struct mailbox *box = update->box;
const char *error;
int ret;
if (update->lock != NULL)
return TRUE;
if (update->lock_failed)
return FALSE;
if (MAIL_INDEX_IS_IN_MEMORY(box->index))
return FALSE;
ret = mailbox_lock_file_create(box, VSIZE_LOCK_SUFFIX, lock_secs,
&update->lock, &error);
if (ret <= 0) {
if (ret < 0)
mailbox_set_critical(box, "%s", error);
update->lock_failed = TRUE;
return FALSE;
}
update->rebuild = FALSE;
vsize_header_refresh(update);
index_mailbox_vsize_check_rebuild(update);
return TRUE;
}
bool index_mailbox_vsize_update_try_lock(struct mailbox_vsize_update *update)
{
return vsize_update_lock_full(update, 0);
}
bool index_mailbox_vsize_update_wait_lock(struct mailbox_vsize_update *update)
{
return vsize_update_lock_full(update, VSIZE_UPDATE_MAX_LOCK_SECS);
}
bool index_mailbox_vsize_want_updates(struct mailbox_vsize_update *update)
{
return update->vsize_hdr.highest_uid > 0;
}
static void
index_mailbox_vsize_update_write_to_index(struct mailbox_vsize_update *update)
{
struct mail_index_transaction *trans;
trans = mail_index_transaction_begin(update->view,
MAIL_INDEX_TRANSACTION_FLAG_EXTERNAL);
mail_index_update_header_ext(trans, update->box->vsize_hdr_ext_id,
0, &update->vsize_hdr,
sizeof(update->vsize_hdr));
(void)mail_index_transaction_commit(&trans);
}
static void
index_mailbox_vsize_update_write(struct mailbox_vsize_update *update)
{
if (update->written)
return;
update->written = TRUE;
if (update->rebuild == FALSE &&
memcmp(&update->orig_vsize_hdr, &update->vsize_hdr,
sizeof(update->vsize_hdr)) == 0) {
return;
}
index_mailbox_vsize_update_write_to_index(update);
}
static void index_mailbox_vsize_notify_indexer(struct mailbox *box)
{
string_t *str = t_str_new(256);
const char *path;
int fd;
path = t_strconcat(box->storage->user->set->base_dir,
"/"INDEXER_SOCKET_NAME, NULL);
fd = net_connect_unix(path);
if (fd == -1) {
mailbox_set_critical(box,
"Can't start vsize building on background: "
"net_connect_unix(%s) failed: %m", path);
return;
}
str_append(str, INDEXER_HANDSHAKE);
str_append(str, "APPEND\t0\t");
str_append_tabescaped(str, box->storage->user->username);
str_append_c(str, '\t');
str_append_tabescaped(str, box->vname);
str_append_c(str, '\n');
if (write_full(fd, str_data(str), str_len(str)) < 0) {
mailbox_set_critical(box,
"Can't start vsize building on background: "
"write(%s) failed: %m", path);
}
i_close_fd(&fd);
}
void index_mailbox_vsize_update_deinit(struct mailbox_vsize_update **_update)
{
struct mailbox_vsize_update *update = *_update;
*_update = NULL;
if ((update->lock != NULL || update->rebuild) && !update->skip_write)
index_mailbox_vsize_update_write(update);
file_lock_free(&update->lock);
if (update->finish_in_background)
index_mailbox_vsize_notify_indexer(update->box);
mail_index_view_close(&update->view);
i_free(update);
}
void index_mailbox_vsize_hdr_expunge(struct mailbox_vsize_update *update,
uint32_t uid, uoff_t vsize)
{
i_assert(update->lock != NULL);
if (uid > update->vsize_hdr.highest_uid)
return;
if (update->vsize_hdr.message_count == 0) {
mailbox_set_critical(update->box,
"vsize-hdr's message_count shrank below 0");
i_zero(&update->vsize_hdr);
return;
}
update->vsize_hdr.message_count--;
if (update->vsize_hdr.vsize < vsize) {
mailbox_set_critical(update->box,
"vsize-hdr's vsize shrank below 0");
i_zero(&update->vsize_hdr);
return;
}
update->vsize_hdr.vsize -= vsize;
}
static void
index_mailbox_vsize_finish_bg(struct mailbox_vsize_update *update,
bool require_result)
{
mail_storage_set_error(update->box->storage, MAIL_ERROR_INUSE,
"Finishing vsize calculation on background");
if (require_result)
update->finish_in_background = TRUE;
}
static int
index_mailbox_vsize_hdr_add_missing(struct mailbox_vsize_update *update,
bool require_result)
{
struct mailbox_index_vsize *vsize_hdr = &update->vsize_hdr;
struct mailbox_transaction_context *trans;
struct mail_search_context *search_ctx;
struct mail_search_args *search_args;
struct mailbox_status status;
struct mail *mail;
unsigned int idx, mails_left;
uint32_t seq1, seq2;
uoff_t vsize;
int ret = 0;
mailbox_get_open_status(update->box, STATUS_UIDNEXT, &status);
if (vsize_hdr->highest_uid + 1 >= status.uidnext) {
return 0;
}
search_args = mail_search_build_init();
if (!mail_index_lookup_seq_range(update->box->view,
vsize_hdr->highest_uid + 1,
status.uidnext-1, &seq1, &seq2)) {
vsize_hdr->highest_uid = status.uidnext - 1;
mail_search_args_unref(&search_args);
return 0;
}
mail_search_build_add_seqset(search_args, seq1, seq2);
if (!mail_index_map_get_ext_idx(update->box->view->map,
update->box->vsize_hdr_ext_id, &idx)) {
index_mailbox_vsize_update_write_to_index(update);
}
trans = mailbox_transaction_begin(update->box, 0, "vsize update");
search_ctx = mailbox_search_init(trans, search_args, NULL,
MAIL_FETCH_VIRTUAL_SIZE, NULL);
if (!require_result)
mails_left = 0;
else if (update->box->storage->set->mail_vsize_bg_after_count == 0)
mails_left = UINT_MAX;
else
mails_left = update->box->storage->set->mail_vsize_bg_after_count;
while (mailbox_search_next(search_ctx, &mail)) {
if (mails_left == 0) {
if (mail->mail_stream_accessed) {
index_mailbox_vsize_finish_bg(update,
require_result);
ret = -1;
break;
}
mail->lookup_abort = MAIL_LOOKUP_ABORT_NOT_IN_CACHE;
}
ret = mail_get_virtual_size(mail, &vsize);
mail->lookup_abort = MAIL_LOOKUP_ABORT_NEVER;
if (ret < 0 &&
mailbox_get_last_mail_error(update->box) == MAIL_ERROR_LOOKUP_ABORTED) {
i_assert(mails_left == 0);
index_mailbox_vsize_finish_bg(update, require_result);
break;
}
if (mail->mail_stream_accessed ||
mail->mail_metadata_accessed) {
i_assert(mails_left > 0);
mails_left--;
}
if (ret < 0) {
if (mail->expunged)
continue;
ret = -1;
break;
}
vsize_hdr->vsize += vsize;
vsize_hdr->highest_uid = mail->uid;
vsize_hdr->message_count++;
}
if (mailbox_search_deinit(&search_ctx) < 0)
ret = -1;
mail_search_args_unref(&search_args);
if (ret == 0) {
vsize_hdr->highest_uid = status.uidnext - 1;
} else {
}
(void)mailbox_transaction_commit(&trans);
return ret;
}
int index_mailbox_get_virtual_size(struct mailbox *box,
struct mailbox_metadata *metadata_r)
{
struct mailbox_vsize_update *update;
struct mailbox_status status;
int ret;
mailbox_get_open_status(box, STATUS_MESSAGES | STATUS_UIDNEXT, &status);
update = index_mailbox_vsize_update_init(box);
if (update->vsize_hdr.highest_uid + 1 == status.uidnext &&
update->vsize_hdr.message_count == status.messages) {
metadata_r->virtual_size = update->vsize_hdr.vsize;
index_mailbox_vsize_update_deinit(&update);
return 0;
}
(void)index_mailbox_vsize_update_wait_lock(update);
struct event_reason *reason = event_reason_begin("mailbox:vsize");
ret = index_mailbox_vsize_hdr_add_missing(update, TRUE);
event_reason_end(&reason);
metadata_r->virtual_size = update->vsize_hdr.vsize;
index_mailbox_vsize_update_deinit(&update);
return ret;
}
int index_mailbox_get_physical_size(struct mailbox *box,
struct mailbox_metadata *metadata_r)
{
struct mailbox_transaction_context *trans;
struct mail_search_context *ctx;
struct mail *mail;
struct mail_search_args *search_args;
uoff_t size;
int ret = 0;
if (box->mail_vfuncs->get_physical_size ==
box->mail_vfuncs->get_virtual_size) {
if (index_mailbox_get_virtual_size(box, metadata_r) < 0)
return -1;
metadata_r->physical_size = metadata_r->virtual_size;
return 0;
}
if (mailbox_sync(box, MAILBOX_SYNC_FLAG_FULL_READ) < 0)
return -1;
trans = mailbox_transaction_begin(box, 0, "mailbox physical size");
search_args = mail_search_build_init();
mail_search_build_add_all(search_args);
ctx = mailbox_search_init(trans, search_args, NULL,
MAIL_FETCH_PHYSICAL_SIZE, NULL);
mail_search_args_unref(&search_args);
metadata_r->physical_size = 0;
while (mailbox_search_next(ctx, &mail)) {
if (mail_get_physical_size(mail, &size) == 0)
metadata_r->physical_size += size;
else {
const char *errstr;
enum mail_error error;
errstr = mailbox_get_last_internal_error(box, &error);
if (error != MAIL_ERROR_EXPUNGED) {
e_error(mail_event(mail),
"Couldn't get size of mail: %s", errstr);
ret = -1;
break;
}
}
}
if (mailbox_search_deinit(&ctx) < 0) {
e_error(mail_event(mail), "Listing mails failed: %s",
mailbox_get_last_internal_error(box, NULL));
ret = -1;
}
(void)mailbox_transaction_commit(&trans);
return ret;
}
void index_mailbox_vsize_update_appends(struct mailbox *box)
{
struct mailbox_vsize_update *update;
struct mailbox_status status;
update = index_mailbox_vsize_update_init(box);
if (update->rebuild) {
update->skip_write = TRUE;
}
index_mailbox_vsize_check_rebuild(update);
if (index_mailbox_vsize_want_updates(update)) {
mailbox_get_open_status(update->box, STATUS_UIDNEXT, &status);
if (update->vsize_hdr.highest_uid + 1 != status.uidnext &&
index_mailbox_vsize_update_try_lock(update)) {
struct event_reason *reason =
event_reason_begin("mailbox:vsize");
(void)index_mailbox_vsize_hdr_add_missing(update, FALSE);
event_reason_end(&reason);
}
}
index_mailbox_vsize_update_deinit(&update);
}