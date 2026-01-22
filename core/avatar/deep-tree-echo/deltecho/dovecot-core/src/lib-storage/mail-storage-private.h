#ifndef MAIL_STORAGE_PRIVATE_H
#define MAIL_STORAGE_PRIVATE_H
#include "module-context.h"
#include "unichar.h"
#include "file-lock.h"
#include "str-sanitize.h"
#include "mail-storage.h"
#include "mail-storage-hooks.h"
#include "mail-storage-settings.h"
#include "mailbox-attribute-private.h"
#include "mail-index-private.h"
struct file_lock;
struct file_create_settings;
struct fs;
#define MAIL_INDEX_PREFIX "dovecot.index"
#define MAIL_READ_HDR_BLOCK_SIZE (1024*4)
#define MAIL_READ_FULL_BLOCK_SIZE IO_BLOCK_SIZE
#define MAIL_SHARED_STORAGE_NAME "shared"
#define MAIL_STORAGE_LOST_MAILBOX_PREFIX "recovered-lost-folder-"
enum mail_storage_list_index_rebuild_reason {
MAIL_STORAGE_LIST_INDEX_REBUILD_REASON_CORRUPTED,
MAIL_STORAGE_LIST_INDEX_REBUILD_REASON_NO_INBOX,
MAIL_STORAGE_LIST_INDEX_REBUILD_REASON_FORCE_RESYNC,
};
struct mail_storage_module_register {
unsigned int id;
};
struct mail_module_register {
unsigned int id;
};
struct mail_storage_vfuncs {
struct mail_storage *(*alloc)(void);
int (*create)(struct mail_storage *storage, struct mail_namespace *ns,
const char **error_r);
void (*destroy)(struct mail_storage *storage);
void (*add_list)(struct mail_storage *storage,
struct mailbox_list *list);
void (*get_list_settings)(const struct mail_namespace *ns,
struct mailbox_list_settings *set);
bool (*autodetect)(const struct mail_namespace *ns,
struct mailbox_list_settings *set);
struct mailbox *(*mailbox_alloc)(struct mail_storage *storage,
struct mailbox_list *list,
const char *vname,
enum mailbox_flags flags);
int (*purge)(struct mail_storage *storage);
int (*list_index_rebuild)(struct mail_storage *storage,
enum mail_storage_list_index_rebuild_reason reason);
};
union mail_storage_module_context {
struct mail_storage_vfuncs super;
struct mail_storage_module_register *reg;
};
enum mail_storage_class_flags {
MAIL_STORAGE_CLASS_FLAG_MAILBOX_IS_FILE	= 0x01,
MAIL_STORAGE_CLASS_FLAG_UNIQUE_ROOT	= 0x02,
MAIL_STORAGE_CLASS_FLAG_OPEN_STREAMS	= 0x04,
MAIL_STORAGE_CLASS_FLAG_NOQUOTA		= 0x08,
MAIL_STORAGE_CLASS_FLAG_NO_ROOT		= 0x10,
MAIL_STORAGE_CLASS_FLAG_FILE_PER_MSG	= 0x20,
MAIL_STORAGE_CLASS_FLAG_HAVE_MAIL_GUIDS	= 0x40,
MAIL_STORAGE_CLASS_FLAG_HAVE_MAIL_SAVE_GUIDS	= 0x80,
MAIL_STORAGE_CLASS_FLAG_BINARY_DATA	= 0x100,
MAIL_STORAGE_CLASS_FLAG_HAVE_MAIL_GUID128 = 0x200,
MAIL_STORAGE_CLASS_FLAG_NO_LIST_DELETES	= 0x400,
MAIL_STORAGE_CLASS_FLAG_SECONDARY_INDEX	= 0x800,
};
struct mail_binary_cache {
struct timeout *to;
struct mailbox *box;
uint32_t uid;
uoff_t orig_physical_pos;
bool include_hdr;
struct istream *input;
uoff_t size;
};
struct mail_storage_error {
char *error_string;
enum mail_error error;
char *last_internal_error;
char *last_internal_error_mailbox;
uint32_t last_internal_error_mail_uid;
bool last_error_is_internal;
};
struct mail_storage {
const char *name;
enum mail_storage_class_flags class_flags;
enum mail_fetch_field nonbody_access_fields;
struct event_category *event_category;
struct mail_storage_vfuncs v, *vlast;
pool_t pool;
struct mail_storage *prev, *next;
int refcount;
int obj_refcount;
struct mailbox *mailboxes;
const char *unique_root_dir;
const char *lost_mailbox_prefix;
char *last_internal_error;
char *last_internal_error_mailbox;
uint32_t last_internal_error_mail_uid;
char *error_string;
enum mail_error error;
ARRAY(struct mail_storage_error) error_stack;
struct event *event;
const struct mail_storage *storage_class;
struct mail_user *user;
const char *temp_path_prefix;
const struct mail_storage_settings *set;
enum mail_storage_flags flags;
struct mail_storage_callbacks callbacks;
void *callback_context;
struct mail_binary_cache binary_cache;
struct dict *_shared_attr_dict;
struct fs *mailboxes_fs;
ARRAY(union mail_storage_module_context *) module_contexts;
bool shared_attr_dict_failed:1;
bool last_error_is_internal:1;
bool rebuilding_list_index:1;
bool rebuild_list_index:1;
};
struct mail_attachment_part {
struct message_part *part;
const char *content_type, *content_disposition;
};
struct virtual_mailbox_vfuncs {
void (*get_virtual_uids)(struct mailbox *box,
struct mailbox *backend_mailbox,
const ARRAY_TYPE(seq_range) *backend_uids,
ARRAY_TYPE(seq_range) *virtual_uids_r);
void (*get_virtual_uid_map)(struct mailbox *box,
struct mailbox *backend_mailbox,
const ARRAY_TYPE(seq_range) *backend_uids,
ARRAY_TYPE(uint32_t) *virtual_uids_r);
void (*get_virtual_backend_boxes)(struct mailbox *box,
ARRAY_TYPE(mailboxes) *mailboxes,
bool only_with_msgs);
uint32_t (*get_virtual_backend_last_uid)(struct mailbox *box,
struct mailbox *bbox);
void (*get_virtual_backend_mail_uid)(struct mailbox *box, uint32_t seq,
struct mailbox **backend_box_r,
uint32_t *backend_uid_r);
};
struct mailbox_vfuncs {
bool (*is_readonly)(struct mailbox *box);
int (*enable)(struct mailbox *box, enum mailbox_feature features);
int (*exists)(struct mailbox *box, bool auto_boxes,
enum mailbox_existence *existence_r);
int (*open)(struct mailbox *box);
void (*close)(struct mailbox *box);
void (*free)(struct mailbox *box);
int (*create_box)(struct mailbox *box,
const struct mailbox_update *update, bool directory);
int (*update_box)(struct mailbox *box,
const struct mailbox_update *update);
int (*delete_box)(struct mailbox *box);
int (*rename_box)(struct mailbox *src, struct mailbox *dest);
int (*get_status)(struct mailbox *box, enum mailbox_status_items items,
struct mailbox_status *status_r);
int (*get_metadata)(struct mailbox *box,
enum mailbox_metadata_items items,
struct mailbox_metadata *metadata_r);
int (*set_subscribed)(struct mailbox *box, bool set);
int (*attribute_set)(struct mailbox_transaction_context *t,
enum mail_attribute_type type_flags,
const char *key,
const struct mail_attribute_value *value);
int (*attribute_get)(struct mailbox *box,
enum mail_attribute_type type_flags,
const char *key,
struct mail_attribute_value *value_r);
struct mailbox_attribute_iter *
(*attribute_iter_init)(struct mailbox *box,
enum mail_attribute_type type_flags,
const char *prefix);
const char *(*attribute_iter_next)(struct mailbox_attribute_iter *iter);
int (*attribute_iter_deinit)(struct mailbox_attribute_iter *iter);
int (*list_index_has_changed)(struct mailbox *box,
struct mail_index_view *list_view,
uint32_t seq, bool quick,
const char **reason_r);
void (*list_index_update_sync)(struct mailbox *box,
struct mail_index_transaction *trans,
uint32_t seq);
struct mailbox_sync_context *
(*sync_init)(struct mailbox *box,
enum mailbox_sync_flags flags);
bool (*sync_next)(struct mailbox_sync_context *ctx,
struct mailbox_sync_rec *sync_rec_r);
int (*sync_deinit)(struct mailbox_sync_context *ctx,
struct mailbox_sync_status *status_r);
void (*sync_notify)(struct mailbox *box, uint32_t uid,
enum mailbox_sync_type sync_type);
void (*notify_changes)(struct mailbox *box);
struct mailbox_transaction_context *
(*transaction_begin)(struct mailbox *box,
enum mailbox_transaction_flags flags,
const char *reason);
int (*transaction_commit)(struct mailbox_transaction_context *t,
struct mail_transaction_commit_changes *changes_r);
void (*transaction_rollback)(struct mailbox_transaction_context *t);
enum mail_flags (*get_private_flags_mask)(struct mailbox *box);
struct mail *
(*mail_alloc)(struct mailbox_transaction_context *t,
enum mail_fetch_field wanted_fields,
struct mailbox_header_lookup_ctx *wanted_headers);
struct mail_search_context *
(*search_init)(struct mailbox_transaction_context *t,
struct mail_search_args *args,
const enum mail_sort_type *sort_program,
enum mail_fetch_field wanted_fields,
struct mailbox_header_lookup_ctx *wanted_headers);
int (*search_deinit)(struct mail_search_context *ctx);
bool (*search_next_nonblock)(struct mail_search_context *ctx,
struct mail **mail_r, bool *tryagain_r);
bool (*search_next_update_seq)(struct mail_search_context *ctx);
int (*search_next_match_mail)(struct mail_search_context *ctx,
struct mail *mail);
struct mail_save_context *
(*save_alloc)(struct mailbox_transaction_context *t);
int (*save_begin)(struct mail_save_context *ctx, struct istream *input);
int (*save_continue)(struct mail_save_context *ctx);
int (*save_finish)(struct mail_save_context *ctx);
void (*save_cancel)(struct mail_save_context *ctx);
int (*copy)(struct mail_save_context *ctx, struct mail *mail);
int (*transaction_save_commit_pre)(struct mail_save_context *save_ctx);
void (*transaction_save_commit_post)
(struct mail_save_context *save_ctx,
struct mail_index_transaction_commit_result *result_r);
void (*transaction_save_rollback)(struct mail_save_context *save_ctx);
bool (*is_inconsistent)(struct mailbox *box);
};
union mailbox_module_context {
struct mailbox_vfuncs super;
struct mail_storage_module_register *reg;
};
struct mail_msgpart_partial_cache {
uint32_t uid;
uoff_t physical_start;
uoff_t physical_pos, virtual_pos;
};
struct mailbox_index_vsize {
uint64_t vsize;
uint32_t highest_uid;
uint32_t message_count;
};
struct mailbox_index_pop3_uidl {
uint32_t max_uid_with_pop3_uidl;
};
struct mailbox_index_first_saved {
uint32_t uid;
uint32_t timestamp;
};
struct mailbox {
const char *name;
const char *vname;
struct mail_storage *storage;
struct mailbox_list *list;
struct event *event;
struct mailbox_vfuncs v, *vlast;
const struct virtual_mailbox_vfuncs *virtual_vfuncs;
pool_t pool;
struct mailbox *prev, *next;
struct mail_index *index;
struct mail_index_view *view;
struct mail_cache *cache;
struct mail_index *index_pvt;
struct mail_index_view *view_pvt;
struct mailbox_permissions _perm;
const char *_path;
const char *_index_path;
const struct mail_vfuncs *mail_vfuncs;
const struct mailbox_settings *set;
enum mail_error open_error;
struct istream *input;
const char *index_prefix;
enum mailbox_flags flags;
unsigned int transaction_count;
unsigned int attribute_iter_count;
enum mailbox_feature enabled_features;
struct mail_msgpart_partial_cache partial_cache;
uint32_t vsize_hdr_ext_id;
uint32_t pop3_uidl_hdr_ext_id;
uint32_t box_name_hdr_ext_id;
uint32_t box_last_rename_stamp_ext_id;
uint32_t mail_vsize_ext_id;
ARRAY_TYPE(seq_range) recent_flags;
uint32_t recent_flags_prev_uid;
uint32_t recent_flags_count;
struct mail_index_view *tmp_sync_view;
mailbox_notify_callback_t *notify_callback;
void *notify_context;
struct timeout *to_notify, *to_notify_delay;
struct mailbox_notify_file *notify_files;
unsigned int generation_sequence;
ARRAY(struct mail_search_result *) search_results;
ARRAY(union mailbox_module_context *) module_contexts;
bool opened:1;
bool mailbox_deleted:1;
bool creating:1;
bool deleting:1;
bool mailbox_undeleting:1;
bool delete_sync_check:1;
bool deleting_must_be_empty:1;
bool delete_skip_empty_check:1;
bool marked_deleted:1;
bool inbox_user:1;
bool inbox_any:1;
bool disable_reflink_copy_to:1;
bool disallow_new_keywords:1;
bool synced:1;
bool mail_cache_disabled:1;
bool update_first_saved:1;
bool skip_create_name_restrictions:1;
bool corrupted_mailbox_name:1;
bool acl_no_lookup_right:1;
bool mailbox_not_original:1;
};
struct mail_vfuncs {
void (*close)(struct mail *mail);
void (*free)(struct mail *mail);
void (*set_seq)(struct mail *mail, uint32_t seq, bool saving);
bool (*set_uid)(struct mail *mail, uint32_t uid);
void (*set_uid_cache_updates)(struct mail *mail, bool set);
bool (*prefetch)(struct mail *mail);
int (*precache)(struct mail *mail);
void (*add_temp_wanted_fields)(struct mail *mail,
enum mail_fetch_field fields,
struct mailbox_header_lookup_ctx *headers);
enum mail_flags (*get_flags)(struct mail *mail);
const char *const *(*get_keywords)(struct mail *mail);
const ARRAY_TYPE(keyword_indexes) *
(*get_keyword_indexes)(struct mail *mail);
uint64_t (*get_modseq)(struct mail *mail);
uint64_t (*get_pvt_modseq)(struct mail *mail);
int (*get_parts)(struct mail *mail,
struct message_part **parts_r);
int (*get_date)(struct mail *mail, time_t *date_r, int *timezone_r);
int (*get_received_date)(struct mail *mail, time_t *date_r);
int (*get_save_date)(struct mail *mail, time_t *date_r);
int (*get_virtual_size)(struct mail *mail, uoff_t *size_r);
int (*get_physical_size)(struct mail *mail, uoff_t *size_r);
int (*get_first_header)(struct mail *mail, const char *field,
bool decode_to_utf8, const char **value_r);
int (*get_headers)(struct mail *mail, const char *field,
bool decode_to_utf8, const char *const **value_r);
int (*get_header_stream)(struct mail *mail,
struct mailbox_header_lookup_ctx *headers,
struct istream **stream_r);
int (*get_stream)(struct mail *mail, bool get_body,
struct message_size *hdr_size,
struct message_size *body_size,
struct istream **stream_r);
int (*get_binary_stream)(struct mail *mail,
const struct message_part *part,
bool include_hdr, uoff_t *size_r,
unsigned int *lines_r, bool *binary_r,
struct istream **stream_r);
int (*get_special)(struct mail *mail, enum mail_fetch_field field,
const char **value_r);
int (*get_backend_mail)(struct mail *mail, struct mail **real_mail_r);
void (*update_flags)(struct mail *mail, enum modify_type modify_type,
enum mail_flags flags);
void (*update_keywords)(struct mail *mail, enum modify_type modify_type,
struct mail_keywords *keywords);
void (*update_modseq)(struct mail *mail, uint64_t min_modseq);
void (*update_pvt_modseq)(struct mail *mail, uint64_t min_pvt_modseq);
void (*update_pop3_uidl)(struct mail *mail, const char *uidl);
void (*expunge)(struct mail *mail);
void (*set_cache_corrupted)(struct mail *mail,
enum mail_fetch_field field,
const char *reason);
int (*istream_opened)(struct mail *mail, struct istream **input);
};
union mail_module_context {
struct mail_vfuncs super;
struct mail_module_register *reg;
};
struct mail_private {
struct mail mail;
struct mail_vfuncs v, *vlast;
struct mail *vmail;
struct event *_event;
uint32_t seq_pvt;
enum mail_fetch_field wanted_fields;
struct mailbox_header_lookup_ctx *wanted_headers;
pool_t pool, data_pool;
ARRAY(union mail_module_context *) module_contexts;
const char *get_stream_reason;
bool autoexpunged:1;
bool search_mail:1;
};
struct mailbox_list_context {
struct mail_storage *storage;
enum mailbox_list_flags flags;
bool failed;
};
union mailbox_transaction_module_context {
struct mail_storage_module_register *reg;
};
struct mailbox_transaction_stats {
unsigned long open_lookup_count;
unsigned long stat_lookup_count;
unsigned long fstat_lookup_count;
unsigned long files_read_count;
unsigned long long files_read_bytes;
unsigned long cache_hit_count;
};
struct mail_save_private_changes {
unsigned int mailnum;
enum mail_flags flags;
};
struct mailbox_transaction_context {
struct mailbox *box;
enum mailbox_transaction_flags flags;
char *reason;
union mail_index_transaction_module_context module_ctx;
struct mail_index_transaction_vfuncs super;
int mail_ref_count;
struct mail_index_transaction *itrans;
struct dict_transaction_context *attr_pvt_trans, *attr_shared_trans;
struct mail_index_view *view;
struct mail_index_transaction *itrans_pvt;
struct mail_index_view *view_pvt;
struct mail_cache_view *cache_view;
struct mail_cache_transaction_ctx *cache_trans;
struct mail_transaction_commit_changes *changes;
ARRAY(union mailbox_transaction_module_context *) module_contexts;
uint32_t prev_pop3_uidl_tracking_seq;
uint32_t highest_pop3_uidl_uid;
struct mail_save_context *save_ctx;
unsigned int save_count;
ARRAY(struct mail_save_private_changes) pvt_saves;
struct mailbox_transaction_stats stats;
bool stats_track:1;
};
union mail_search_module_context {
struct mail_storage_module_register *reg;
};
struct mail_search_context {
struct mailbox_transaction_context *transaction;
struct mail_search_args *args;
struct mail_search_sort_program *sort_program;
enum mail_fetch_field wanted_fields;
struct mailbox_header_lookup_ctx *wanted_headers;
normalizer_func_t *normalizer;
struct timeval search_start_time, last_notify;
unsigned int search_notify_passes;
struct mail_search_result *update_result;
ARRAY(struct mail_search_result *) results;
uint32_t seq;
uint32_t progress_cur, progress_max;
ARRAY(struct mail *) mails;
unsigned int unused_mail_idx;
unsigned int max_mails;
ARRAY(union mail_search_module_context *) module_contexts;
bool seen_lost_data:1;
bool progress_hidden:1;
};
struct mail_save_data {
enum mail_flags flags;
enum mail_flags pvt_flags;
struct mail_keywords *keywords;
uint64_t min_modseq;
time_t received_date, save_date;
int received_tz_offset;
uint32_t uid;
char *guid, *pop3_uidl, *from_envelope;
uint32_t pop3_order;
struct ostream *output;
struct mail_save_attachment *attach;
};
struct mail_save_context {
struct mailbox_transaction_context *transaction;
struct mail *dest_mail;
struct mail *copy_src_mail;
struct mail_save_data data;
bool (*part_is_attachment)(struct mail_save_context *ctx,
const struct mail_attachment_part *part);
bool unfinished:1;
bool finishing:1;
bool copying_via_save:1;
bool saving:1;
bool moving:1;
bool copying_or_moving:1;
};
struct mailbox_sync_context {
struct mailbox *box;
enum mailbox_sync_flags flags;
bool open_failed;
};
struct mailbox_header_lookup_ctx {
struct mailbox *box;
pool_t pool;
int refcount;
unsigned int count;
const char *const *name;
unsigned int *idx;
};
extern struct mail_storage_module_register mail_storage_module_register;
extern struct mail_module_register mail_module_register;
extern struct event_category event_category_storage;
extern struct event_category event_category_mailbox;
extern struct event_category event_category_mail;
#define MAIL_STORAGE_CONTEXT(obj) \
MODULE_CONTEXT(obj, mail_storage_mail_index_module)
#define MAIL_STORAGE_CONTEXT_REQUIRE(obj) \
MODULE_CONTEXT_REQUIRE(obj, mail_storage_mail_index_module)
extern MODULE_CONTEXT_DEFINE(mail_storage_mail_index_module,
&mail_index_module_register);
void mail_storage_obj_ref(struct mail_storage *storage);
void mail_storage_obj_unref(struct mail_storage *storage);
void mail_storage_clear_error(struct mail_storage *storage);
void mail_storage_set_error(struct mail_storage *storage,
enum mail_error error, const char *string);
void mail_storage_set_critical(struct mail_storage *storage,
const char *fmt, ...) ATTR_FORMAT(2, 3);
void mailbox_set_critical(struct mailbox *box,
const char *fmt, ...) ATTR_FORMAT(2, 3);
void mail_set_critical(struct mail *mail,
const char *fmt, ...) ATTR_FORMAT(2, 3);
void mail_storage_set_internal_error(struct mail_storage *storage);
void mailbox_set_index_error(struct mailbox *box);
void mail_storage_set_index_error(struct mail_storage *storage,
struct mail_index *index);
bool mail_storage_set_error_from_errno(struct mail_storage *storage);
void mail_storage_copy_list_error(struct mail_storage *storage,
struct mailbox_list *list);
void mail_storage_copy_error(struct mail_storage *dest,
struct mail_storage *src);
void mail_set_mail_cache_corrupted(struct mail *mail, const char *fmt, ...)
ATTR_FORMAT(2, 3);
void mail_autoexpunge(struct mail *mail);
void mail_event_create(struct mail *mail);
bool mail_prefetch(struct mail *mail);
void mail_set_aborted(struct mail *mail);
void mail_set_expunged(struct mail *mail);
void mail_set_seq_saving(struct mail *mail, uint32_t seq);
bool mail_has_attachment_keywords(struct mail *mail);
int mail_set_attachment_keywords(struct mail *mail);
bool mail_stream_access_start(struct mail *mail);
bool mail_metadata_access_start(struct mail *mail);
void mail_opened_event(struct mail *mail);
void mail_metadata_accessed_event(struct event *mail_event);
void mail_expunge_requested_event(struct mail *mail);
void mailbox_set_deleted(struct mailbox *box);
int mailbox_mark_index_deleted(struct mailbox *box, bool del);
const char *mailbox_get_path(struct mailbox *box) ATTR_PURE;
const char *mailbox_get_index_path(struct mailbox *box) ATTR_PURE;
int mailbox_get_path_to(struct mailbox *box, enum mailbox_list_path_type type,
const char **path_r);
const struct mailbox_permissions *mailbox_get_permissions(struct mailbox *box);
void mailbox_refresh_permissions(struct mailbox *box);
int mailbox_open_index_pvt(struct mailbox *box);
int mailbox_create_missing_dir(struct mailbox *box,
enum mailbox_list_path_type type);
bool mailbox_is_autocreated(struct mailbox *box);
bool mailbox_is_autosubscribed(struct mailbox *box);
int mailbox_create_fd(struct mailbox *box, const char *path, int flags,
int *fd_r);
int mail_storage_lock_create(const char *lock_path,
const struct file_create_settings *lock_set,
const struct mail_storage_settings *mail_set,
struct file_lock **lock_r, const char **error_r);
int mailbox_lock_file_create(struct mailbox *box, const char *lock_fname,
unsigned int lock_secs, struct file_lock **lock_r,
const char **error_r);
unsigned int mail_storage_get_lock_timeout(struct mail_storage *storage,
unsigned int secs);
void mail_storage_free_binary_cache(struct mail_storage *storage);
enum mail_index_open_flags
mail_storage_settings_to_index_flags(const struct mail_storage_settings *set);
void mailbox_save_context_deinit(struct mail_save_context *ctx);
void mailbox_sync_notify(struct mailbox *box, uint32_t uid,
enum mailbox_sync_type sync_type);
static inline const char *mailbox_name_sanitize(const char *name)
{
return str_sanitize(name, 128);
}
struct event *
mail_storage_mailbox_create_event(struct event *parent,
struct mailbox_list *list, const char *vname);
int mailbox_verify_name(struct mailbox *box);
int mail_storage_list_index_rebuild_and_set_uncorrupted(struct mail_storage *storage);
int mail_storage_list_index_rebuild(struct mail_storage *storage,
enum mail_storage_list_index_rebuild_reason reason);
#endif