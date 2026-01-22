#include "lib.h"
#include "array.h"
#include "test-common.h"
#include "test-mail-index.h"
#include "mail-transaction-log-private.h"
static void test_mail_index_rotate(void)
{
struct mail_index *index, *index2;
struct mail_index_view *view;
struct mail_index_transaction *trans;
struct mail_transaction_log_file *file;
const char *reason;
test_begin("mail index rotate");
index = test_mail_index_init(TRUE);
index2 = test_mail_index_open(FALSE);
view = mail_index_view_open(index);
trans = mail_index_transaction_begin(view, 0);
mail_index_reset(trans);
test_assert(mail_index_transaction_commit(&trans) == 0);
trans = mail_index_transaction_begin(view, 0);
mail_index_reset(trans);
test_assert(mail_index_transaction_commit(&trans) == 0);
test_assert(mail_transaction_log_find_file(index2->log, 3, FALSE, &file, &reason) == 0);
mail_index_view_close(&view);
test_mail_index_deinit(&index);
test_mail_index_deinit(&index2);
test_end();
}
static void
test_mail_index_new_extension_rotate_write(struct mail_index *index2,
uint32_t uid)
{
struct mail_index_view *view2;
struct mail_index_transaction *trans;
uint32_t hdr_ext_id, rec_ext_id, file_seq, seq, rec_ext = 0x12345678;
uoff_t file_offset;
test_assert(mail_transaction_log_sync_lock(index2->log, "test",
&file_seq, &file_offset) == 0);
mail_index_write(index2, TRUE, "test");
mail_transaction_log_sync_unlock(index2->log, "test");
hdr_ext_id = mail_index_ext_register(index2, "test",
sizeof(hdr_ext_id), 0, 0);
rec_ext_id = mail_index_ext_register(index2, "test-rec", 0,
sizeof(uint32_t), sizeof(uint32_t));
view2 = mail_index_view_open(index2);
trans = mail_index_transaction_begin(view2,
MAIL_INDEX_TRANSACTION_FLAG_EXTERNAL);
mail_index_update_header_ext(trans, hdr_ext_id, 0,
&hdr_ext_id, sizeof(hdr_ext_id));
mail_index_append(trans, uid, &seq);
mail_index_update_ext(trans, seq, rec_ext_id, &rec_ext, NULL);
test_assert(mail_index_transaction_commit(&trans) == 0);
mail_index_view_close(&view2);
}
static void test_mail_index_new_extension_sync(struct mail_index_view *view)
{
struct mail_index_view_sync_ctx *sync_ctx;
struct mail_index_view_sync_rec sync_rec;
bool delayed_expunges;
test_assert(mail_index_refresh(view->index) == 0);
sync_ctx = mail_index_view_sync_begin(view,
MAIL_INDEX_VIEW_SYNC_FLAG_NOEXPUNGES);
test_assert(!mail_index_view_sync_next(sync_ctx, &sync_rec));
test_assert(mail_index_view_sync_commit(&sync_ctx, &delayed_expunges) == 0);
}
static void test_mail_index_new_extension(void)
{
struct mail_index *index, *index2;
struct mail_index_view *view, *view2;
struct mail_index_transaction *trans;
uint32_t seq, rec_ext_id, rec_ext = 0x12345678;
test_begin("mail index new extension");
index = test_mail_index_init(TRUE);
index2 = test_mail_index_open(FALSE);
view = mail_index_view_open(index);
rec_ext_id = mail_index_ext_register(index, "test-rec", 0,
sizeof(uint32_t), sizeof(uint32_t));
uint32_t uid_validity = 123456;
trans = mail_index_transaction_begin(view,
MAIL_INDEX_TRANSACTION_FLAG_EXTERNAL);
mail_index_update_header(trans,
offsetof(struct mail_index_header, uid_validity),
&uid_validity, sizeof(uid_validity), TRUE);
mail_index_append(trans, 1, &seq);
mail_index_update_ext(trans, seq, rec_ext_id, &rec_ext, NULL);
mail_index_append(trans, 2, &seq);
mail_index_update_ext(trans, seq, rec_ext_id, &rec_ext, NULL);
test_assert(mail_index_transaction_commit(&trans) == 0);
test_assert(mail_index_refresh(index2) == 0);
mail_index_view_close(&view);
view = mail_index_view_open(index);
view2 = mail_index_view_open(index2);
trans = mail_index_transaction_begin(view2,
MAIL_INDEX_TRANSACTION_FLAG_EXTERNAL);
mail_index_expunge(trans, 1);
test_assert(mail_index_transaction_commit(&trans) == 0);
mail_index_view_close(&view2);
test_mail_index_new_extension_sync(view);
for (unsigned int i = 0; i < 3; i++)
test_mail_index_new_extension_rotate_write(index2, 3 + i);
test_expect_error_string("generating missing logs");
test_mail_index_new_extension_sync(view);
test_expect_no_more_errors();
test_assert(mail_index_get_header(view)->messages_count == 5);
for (seq = 1; seq <= 5; seq++) {
const void *data;
bool expunged;
mail_index_lookup_ext(view, seq, rec_ext_id, &data, &expunged);
test_assert_idx(memcmp(data, &rec_ext, sizeof(rec_ext)) == 0, seq);
}
test_mail_index_new_extension_rotate_write(index2, 6);
test_mail_index_new_extension_sync(view);
mail_index_view_close(&view);
test_mail_index_deinit(&index);
test_mail_index_deinit(&index2);
test_end();
}
int main(void)
{
static void (*const test_functions[])(void) = {
test_mail_index_rotate,
test_mail_index_new_extension,
NULL
};
return test_run(test_functions);
}