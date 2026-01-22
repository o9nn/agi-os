#ifndef DBOX_STORAGE_H
#define DBOX_STORAGE_H
#include "mail-storage-private.h"
struct dbox_file;
struct dbox_mail;
struct dbox_storage;
struct dbox_save_context;
#define DBOX_SUBSCRIPTION_FILE_NAME "subscriptions"
#define DBOX_UIDVALIDITY_FILE_NAME "dovecot-uidvalidity"
#define DBOX_TEMP_FILE_PREFIX ".temp."
#define DBOX_ALT_SYMLINK_NAME "dbox-alt-root"
#define DBOX_MAILBOX_DIR_NAME "mailboxes"
#define DBOX_MAILDIR_NAME "dbox-Mails"
#define DBOX_TMP_DELETE_SECS (36*60*60)
#define DBOX_INDEX_FLAG_ALT MAIL_INDEX_MAIL_FLAG_BACKEND
enum dbox_index_header_flags {
DBOX_INDEX_HEADER_FLAG_HAVE_POP3_UIDLS = 0x01,
DBOX_INDEX_HEADER_FLAG_HAVE_POP3_ORDERS = 0x02
};
struct dbox_storage_vfuncs {
void (*file_unrefed)(struct dbox_file *file);
int (*file_create_fd)(struct dbox_file *file, const char *path,
bool parents);
int (*mail_file_set)(struct dbox_mail *mail);
int (*mail_open)(struct dbox_mail *mail, uoff_t *offset_r,
struct dbox_file **file_r);
int (*mailbox_create_indexes)(struct mailbox *box,
const struct mailbox_update *update,
struct mail_index_transaction *trans);
const char *(*get_attachment_path_suffix)(struct dbox_file *file);
void (*set_mailbox_corrupted)(struct mailbox *box, const char *reason);
void (*set_file_corrupted)(struct dbox_file *file, const char *reason);
};
struct dbox_storage {
struct mail_storage storage;
struct dbox_storage_vfuncs v;
struct fs *attachment_fs;
const char *attachment_dir;
};
#define DBOX_STORAGE(s) container_of(s, struct dbox_storage, storage)
void dbox_storage_get_list_settings(const struct mail_namespace *ns,
struct mailbox_list_settings *set);
int dbox_storage_create(struct mail_storage *storage,
struct mail_namespace *ns,
const char **error_r);
void dbox_storage_destroy(struct mail_storage *storage);
uint32_t dbox_get_uidvalidity_next(struct mailbox_list *list);
void dbox_notify_changes(struct mailbox *box);
int dbox_mailbox_check_existence(struct mailbox *box);
int dbox_mailbox_open(struct mailbox *box);
void dbox_mailbox_close(struct mailbox *box);
void dbox_mailbox_close_cleanup(struct mailbox *box);
int dbox_mailbox_list_cleanup(struct mail_user *user, const char *path,
time_t last_temp_file_scan);
int dbox_mailbox_create(struct mailbox *box,
const struct mailbox_update *update, bool directory);
int dbox_mailbox_create_indexes(struct mailbox *box,
const struct mailbox_update *update);
int dbox_verify_alt_storage(struct mailbox_list *list);
bool dbox_header_have_flag(struct mailbox *box, uint32_t ext_id,
unsigned int flags_offset, uint8_t flag);
#endif