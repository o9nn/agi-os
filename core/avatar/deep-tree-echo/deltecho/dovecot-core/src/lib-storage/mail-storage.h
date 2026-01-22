#ifndef MAIL_STORAGE_H
#define MAIL_STORAGE_H
struct message_size;
#include "seq-range-array.h"
#include "file-lock.h"
#include "guid.h"
#include "mail-types.h"
#include "mail-error.h"
#include "mail-index.h"
#include "mail-namespace.h"
#include "mailbox-list.h"
#include "mailbox-attribute.h"
#define MAIL_STORAGE_NOTIFY_INTERVAL_SECS 10
#define MAIL_EXPUNGE_BATCH_SIZE 1000
#define MAIL_KEYWORD_HAS_ATTACHMENT "$HasAttachment"
#define MAIL_KEYWORD_HAS_NO_ATTACHMENT "$HasNoAttachment"
enum mail_storage_flags {
MAIL_STORAGE_FLAG_KEEP_HEADER_MD5 = 0x01,
MAIL_STORAGE_FLAG_NO_AUTODETECTION = 0x02,
MAIL_STORAGE_FLAG_NO_AUTOCREATE = 0x04,
MAIL_STORAGE_FLAG_NO_AUTOVERIFY = 0x08
};
enum mailbox_flags {
MAILBOX_FLAG_READONLY = 0x01,
MAILBOX_FLAG_SAVEONLY = 0x02,
MAILBOX_FLAG_DROP_RECENT = 0x04,
MAILBOX_FLAG_NO_INDEX_FILES = 0x10,
MAILBOX_FLAG_KEEP_LOCKED = 0x20,
MAILBOX_FLAG_POP3_SESSION = 0x40,
MAILBOX_FLAG_POST_SESSION = 0x80,
MAILBOX_FLAG_IGNORE_ACLS = 0x100,
MAILBOX_FLAG_OPEN_DELETED = 0x200,
MAILBOX_FLAG_DELETE_UNSAFE = 0x400,
MAILBOX_FLAG_AUTO_CREATE = 0x1000,
MAILBOX_FLAG_AUTO_SUBSCRIBE = 0x2000,
MAILBOX_FLAG_FSCK = 0x4000,
MAILBOX_FLAG_SPECIAL_USE = 0x8000,
MAILBOX_FLAG_ATTRIBUTE_SESSION = 0x10000,
};
enum mailbox_feature {
MAILBOX_FEATURE_CONDSTORE = 0x01,
};
enum mailbox_existence {
MAILBOX_EXISTENCE_NONE,
MAILBOX_EXISTENCE_NOSELECT,
MAILBOX_EXISTENCE_SELECT
};
enum mailbox_status_items {
STATUS_MESSAGES = 0x01,
STATUS_RECENT = 0x02,
STATUS_UIDNEXT = 0x04,
STATUS_UIDVALIDITY = 0x08,
STATUS_UNSEEN = 0x10,
STATUS_FIRST_UNSEEN_SEQ = 0x20,
STATUS_KEYWORDS = 0x40,
STATUS_HIGHESTMODSEQ = 0x80,
STATUS_PERMANENT_FLAGS = 0x200,
STATUS_FIRST_RECENT_UID = 0x400,
STATUS_FTS_LAST_INDEXED_UID = 0x800,
STATUS_CHECK_OVER_QUOTA = 0x1000,
STATUS_HIGHESTPVTMODSEQ = 0x2000,
#define MAILBOX_STATUS_FAILING_ITEMS \
(STATUS_FTS_LAST_INDEXED_UID | STATUS_CHECK_OVER_QUOTA)
};
enum mailbox_metadata_items {
MAILBOX_METADATA_GUID = 0x01,
MAILBOX_METADATA_VIRTUAL_SIZE = 0x02,
MAILBOX_METADATA_CACHE_FIELDS = 0x04,
MAILBOX_METADATA_PRECACHE_FIELDS = 0x08,
MAILBOX_METADATA_BACKEND_NAMESPACE = 0x10,
MAILBOX_METADATA_PHYSICAL_SIZE = 0x20,
MAILBOX_METADATA_FIRST_SAVE_DATE = 0x40
#define MAILBOX_METADATA_SYNC_ITEMS \
(MAILBOX_METADATA_VIRTUAL_SIZE | MAILBOX_METADATA_PHYSICAL_SIZE | \
MAILBOX_METADATA_FIRST_SAVE_DATE)
};
enum mailbox_search_result_flags {
MAILBOX_SEARCH_RESULT_FLAG_UPDATE = 0x01,
MAILBOX_SEARCH_RESULT_FLAG_QUEUE_SYNC = 0x02
};
enum mail_sort_type {
MAIL_SORT_ARRIVAL = 0x0001,
MAIL_SORT_CC = 0x0002,
MAIL_SORT_DATE = 0x0004,
MAIL_SORT_FROM = 0x0008,
MAIL_SORT_SIZE = 0x0010,
MAIL_SORT_SUBJECT = 0x0020,
MAIL_SORT_TO = 0x0040,
MAIL_SORT_RELEVANCY = 0x0080,
MAIL_SORT_DISPLAYFROM = 0x0100,
MAIL_SORT_DISPLAYTO = 0x0200,
MAIL_SORT_POP3_ORDER = 0x0400,
#define MAX_SORT_PROGRAM_SIZE (11 + 1)
MAIL_SORT_MASK = 0x0fff,
MAIL_SORT_FLAG_REVERSE = 0x1000,
MAIL_SORT_END = 0x0000
};
enum mail_fetch_field {
MAIL_FETCH_FLAGS = 0x00000001,
MAIL_FETCH_MESSAGE_PARTS = 0x00000002,
MAIL_FETCH_STREAM_HEADER = 0x00000004,
MAIL_FETCH_STREAM_BODY = 0x00000008,
MAIL_FETCH_DATE = 0x00000010,
MAIL_FETCH_RECEIVED_DATE = 0x00000020,
MAIL_FETCH_SAVE_DATE = 0x00000040,
MAIL_FETCH_PHYSICAL_SIZE = 0x00000080,
MAIL_FETCH_VIRTUAL_SIZE = 0x00000100,
MAIL_FETCH_NUL_STATE = 0x00000200,
MAIL_FETCH_STREAM_BINARY = 0x00000400,
MAIL_FETCH_IMAP_BODY = 0x00001000,
MAIL_FETCH_IMAP_BODYSTRUCTURE = 0x00002000,
MAIL_FETCH_IMAP_ENVELOPE = 0x00004000,
MAIL_FETCH_FROM_ENVELOPE = 0x00008000,
MAIL_FETCH_HEADER_MD5 = 0x00010000,
MAIL_FETCH_STORAGE_ID = 0x00020000,
MAIL_FETCH_UIDL_BACKEND = 0x00040000,
MAIL_FETCH_MAILBOX_NAME = 0x00080000,
MAIL_FETCH_SEARCH_RELEVANCY = 0x00100000,
MAIL_FETCH_GUID = 0x00200000,
MAIL_FETCH_POP3_ORDER = 0x00400000,
MAIL_FETCH_REFCOUNT = 0x00800000,
MAIL_FETCH_BODY_SNIPPET = 0x01000000,
MAIL_FETCH_REFCOUNT_ID = 0x02000000,
};
enum mailbox_transaction_flags {
MAILBOX_TRANSACTION_FLAG_HIDE = 0x01,
MAILBOX_TRANSACTION_FLAG_EXTERNAL = 0x02,
MAILBOX_TRANSACTION_FLAG_ASSIGN_UIDS = 0x04,
MAILBOX_TRANSACTION_FLAG_REFRESH = 0x08,
MAILBOX_TRANSACTION_FLAG_NO_CACHE_DEC = 0x10,
MAILBOX_TRANSACTION_FLAG_SYNC = 0x20,
MAILBOX_TRANSACTION_FLAG_NO_NOTIFY = 0x40,
};
enum mailbox_sync_flags {
MAILBOX_SYNC_FLAG_FULL_READ = 0x01,
MAILBOX_SYNC_FLAG_FULL_WRITE = 0x02,
MAILBOX_SYNC_FLAG_FAST = 0x04,
MAILBOX_SYNC_FLAG_NO_EXPUNGES = 0x08,
MAILBOX_SYNC_FLAG_FIX_INCONSISTENT = 0x40,
MAILBOX_SYNC_FLAG_EXPUNGE = 0x80,
MAILBOX_SYNC_FLAG_FORCE_RESYNC = 0x100,
MAILBOX_SYNC_FLAG_OPTIMIZE = 0x400
};
enum mailbox_sync_type {
MAILBOX_SYNC_TYPE_EXPUNGE = 0x01,
MAILBOX_SYNC_TYPE_FLAGS = 0x02,
MAILBOX_SYNC_TYPE_MODSEQ = 0x04
};
struct message_part;
struct mail_namespace;
struct mail_storage;
struct mail_search_args;
struct mail_search_result;
struct mail_keywords;
struct mail_save_context;
struct mailbox;
struct mailbox_transaction_context;
struct mailbox_status {
uint32_t messages;
uint32_t recent;
uint32_t unseen;
uint32_t uidvalidity;
uint32_t uidnext;
uint32_t first_unseen_seq;
uint32_t first_recent_uid;
uint32_t fts_last_indexed_uid;
uint64_t highest_modseq;
uint64_t highest_pvt_modseq;
const ARRAY_TYPE(keywords) *keywords;
enum mail_flags permanent_flags;
enum mail_flags flags;
bool permanent_keywords:1;
bool allow_new_keywords:1;
bool nonpermanent_modseqs:1;
bool no_modseq_tracking:1;
bool have_guids:1;
bool have_save_guids:1;
bool have_only_guid128:1;
};
struct mailbox_cache_field {
const char *name;
int decision;
time_t last_used;
};
ARRAY_DEFINE_TYPE(mailbox_cache_field, struct mailbox_cache_field);
struct mailbox_metadata {
guid_128_t guid;
uint64_t virtual_size;
uint64_t physical_size;
time_t first_save_date;
const ARRAY_TYPE(mailbox_cache_field) *cache_fields;
enum mail_fetch_field precache_fields;
const char *backend_ns_prefix;
enum mail_namespace_type backend_ns_type;
};
struct mailbox_update {
guid_128_t mailbox_guid;
uint32_t uid_validity;
uint32_t min_next_uid;
uint32_t min_first_recent_uid;
uint64_t min_highest_modseq;
uint64_t min_highest_pvt_modseq;
const struct mailbox_cache_field *cache_updates;
};
struct mail_transaction_commit_changes {
pool_t pool;
uint32_t uid_validity;
ARRAY_TYPE(seq_range) saved_uids;
unsigned int ignored_modseq_changes;
enum mail_index_transaction_change changes_mask;
bool no_read_perm;
};
struct mailbox_sync_rec {
uint32_t seq1, seq2;
enum mailbox_sync_type type;
};
struct mailbox_sync_status {
bool sync_delayed_expunges:1;
};
struct mailbox_expunge_rec {
uint32_t uid;
guid_128_t guid_128;
};
ARRAY_DEFINE_TYPE(mailbox_expunge_rec, struct mailbox_expunge_rec);
enum mail_lookup_abort {
MAIL_LOOKUP_ABORT_NEVER = 0,
MAIL_LOOKUP_ABORT_READ_MAIL,
MAIL_LOOKUP_ABORT_NOT_IN_CACHE,
MAIL_LOOKUP_ABORT_NOT_IN_CACHE_START_CACHING,
};
enum mail_access_type {
MAIL_ACCESS_TYPE_DEFAULT = 0,
MAIL_ACCESS_TYPE_SEARCH,
MAIL_ACCESS_TYPE_SORT,
};
struct mail {
struct mailbox *box;
struct mailbox_transaction_context *transaction;
uint32_t seq, uid;
bool expunged:1;
bool saving:1;
bool has_nuls:1;
bool has_no_nuls:1;
bool mail_stream_accessed:1;
bool mail_metadata_accessed:1;
enum mail_access_type access_type;
enum mail_lookup_abort lookup_abort;
};
struct mail_storage_progress_details {
const char *verb;
unsigned int total;
unsigned int processed;
struct timeval now;
struct timeval start_time;
};
struct mail_storage_callbacks {
void (*notify_ok)(struct mailbox *mailbox, const char *text,
void *context);
void (*notify_no)(struct mailbox *mailbox, const char *text,
void *context);
void (*notify_progress)(struct mailbox *mailbox,
const struct mail_storage_progress_details *dtl,
void *context);
};
struct mailbox_virtual_pattern {
struct mail_namespace *ns;
const char *pattern;
};
ARRAY_DEFINE_TYPE(mailbox_virtual_patterns, struct mailbox_virtual_pattern);
ARRAY_DEFINE_TYPE(mail_storage, struct mail_storage *);
ARRAY_DEFINE_TYPE(mailboxes, struct mailbox *);
extern ARRAY_TYPE(mail_storage) mail_storage_classes;
typedef void mailbox_notify_callback_t(struct mailbox *box, void *context);
void mail_storage_init(void);
void mail_storage_deinit(void);
void mail_storage_register_all(void);
void mail_storage_class_register(struct mail_storage *storage_class);
void mail_storage_class_unregister(struct mail_storage *storage_class);
struct mail_storage *mail_storage_find_class(const char *name);
int mail_storage_create(struct mail_namespace *ns, const char *driver,
enum mail_storage_flags flags, const char **error_r)
ATTR_NULL(2);
int mail_storage_create_full(struct mail_namespace *ns, const char *driver,
const char *data, enum mail_storage_flags flags,
struct mail_storage **storage_r,
const char **error_r) ATTR_NULL(2);
void mail_storage_unref(struct mail_storage **storage);
const struct mail_storage_settings *
mail_storage_get_settings(struct mail_storage *storage) ATTR_PURE;
struct mail_user *mail_storage_get_user(struct mail_storage *storage) ATTR_PURE;
void mail_storage_set_callbacks(struct mail_storage *storage,
struct mail_storage_callbacks *callbacks,
void *context) ATTR_NULL(3);
int mail_storage_purge(struct mail_storage *storage);
const char * ATTR_NOWARN_UNUSED_RESULT
mail_storage_get_last_error(struct mail_storage *storage,
enum mail_error *error_r) ATTR_NULL(2);
const char * ATTR_NOWARN_UNUSED_RESULT
mailbox_get_last_error(struct mailbox *box, enum mail_error *error_r)
ATTR_NULL(2);
enum mail_error mailbox_get_last_mail_error(struct mailbox *box);
const char * ATTR_NOWARN_UNUSED_RESULT
mail_storage_get_last_internal_error(struct mail_storage *storage,
enum mail_error *error_r) ATTR_NULL(2);
const char * ATTR_NOWARN_UNUSED_RESULT
mailbox_get_last_internal_error(struct mailbox *box,
enum mail_error *error_r) ATTR_NULL(2);
const char * ATTR_NOWARN_UNUSED_RESULT
mail_get_last_internal_error(struct mail *mail,
enum mail_error *error_r) ATTR_NULL(2);
void mail_storage_last_error_push(struct mail_storage *storage);
void mail_storage_last_error_pop(struct mail_storage *storage);
bool mail_storage_is_mailbox_file(struct mail_storage *storage) ATTR_PURE;
struct mailbox *mailbox_alloc(struct mailbox_list *list, const char *vname,
enum mailbox_flags flags);
struct mailbox *mailbox_alloc_guid(struct mailbox_list *list,
const guid_128_t guid,
enum mailbox_flags flags);
struct mailbox *
mailbox_alloc_for_user(struct mail_user *user, const char *mname,
enum mailbox_flags flags);
int mailbox_exists(struct mailbox *box, bool auto_boxes,
enum mailbox_existence *existence_r);
int mailbox_open(struct mailbox *box);
int mailbox_open_stream(struct mailbox *box, struct istream *input);
void mailbox_close(struct mailbox *box);
void mailbox_free(struct mailbox **box);
int mailbox_mkdir(struct mailbox *box, const char *path,
enum mailbox_list_path_type type);
bool mailbox_equals(const struct mailbox *box1,
const struct mail_namespace *ns2,
const char *vname2) ATTR_PURE;
bool mailbox_is_any_inbox(struct mailbox *box);
bool mailbox_has_special_use(struct mailbox *box, const char *special_use);
void mailbox_skip_create_name_restrictions(struct mailbox *box, bool set);
int mailbox_verify_create_name(struct mailbox *box);
int mailbox_create(struct mailbox *box, const struct mailbox_update *update,
bool directory) ATTR_NULL(2);
int mailbox_update(struct mailbox *box, const struct mailbox_update *update);
int mailbox_delete(struct mailbox *box);
int mailbox_delete_empty(struct mailbox *box);
int mailbox_rename(struct mailbox *src, struct mailbox *dest);
int mailbox_set_subscribed(struct mailbox *box, bool set);
bool mailbox_is_subscribed(struct mailbox *box);
int mailbox_enable(struct mailbox *box, enum mailbox_feature features);
enum mailbox_feature
mailbox_get_enabled_features(struct mailbox *box) ATTR_PURE;
struct mail_storage *mailbox_get_storage(const struct mailbox *box) ATTR_PURE;
struct mail_namespace *
mailbox_get_namespace(const struct mailbox *box) ATTR_PURE;
const struct mailbox_settings *
mailbox_get_settings(struct mailbox *box) ATTR_PURE;
const char *mailbox_get_vname(const struct mailbox *box) ATTR_PURE;
const char *mailbox_get_name(const struct mailbox *box) ATTR_PURE;
bool mailbox_is_readonly(struct mailbox *box);
bool mailbox_backends_equal(const struct mailbox *box1,
const struct mailbox *box2);
bool mailbox_is_inconsistent(struct mailbox *box);
int mailbox_get_status(struct mailbox *box, enum mailbox_status_items items,
struct mailbox_status *status_r);
void mailbox_get_open_status(struct mailbox *box,
enum mailbox_status_items items,
struct mailbox_status *status_r);
int mailbox_get_metadata(struct mailbox *box, enum mailbox_metadata_items items,
struct mailbox_metadata *metadata_r);
enum mail_flags mailbox_get_private_flags_mask(struct mailbox *box);
struct mailbox_sync_context *
mailbox_sync_init(struct mailbox *box, enum mailbox_sync_flags flags);
bool mailbox_sync_next(struct mailbox_sync_context *ctx,
struct mailbox_sync_rec *sync_rec_r);
int mailbox_sync_deinit(struct mailbox_sync_context **ctx,
struct mailbox_sync_status *status_r);
int mailbox_sync(struct mailbox *box, enum mailbox_sync_flags flags);
void mailbox_notify_changes(struct mailbox *box,
mailbox_notify_callback_t *callback, void *context)
ATTR_NULL(3);
#define mailbox_notify_changes(box, callback, context) \
mailbox_notify_changes(box, (mailbox_notify_callback_t *)callback, \
(void *)((char *)context - CALLBACK_TYPECHECK(callback, \
void (*)(struct mailbox *, typeof(context)))))
void mailbox_notify_changes_stop(struct mailbox *box);
struct mailbox_transaction_context *
mailbox_transaction_begin(struct mailbox *box,
enum mailbox_transaction_flags flags,
const char *reason);
int mailbox_transaction_commit(struct mailbox_transaction_context **t);
int mailbox_transaction_commit_get_changes(
struct mailbox_transaction_context **t,
struct mail_transaction_commit_changes *changes_r);
void mailbox_transaction_rollback(struct mailbox_transaction_context **t);
unsigned int mailbox_transaction_get_count(const struct mailbox *box) ATTR_PURE;
void mailbox_transaction_set_max_modseq(struct mailbox_transaction_context *t,
uint64_t max_modseq,
ARRAY_TYPE(seq_range) *seqs);
struct mailbox *
mailbox_transaction_get_mailbox(const struct mailbox_transaction_context *t)
ATTR_PURE;
void mailbox_get_seq_range(struct mailbox *box, uint32_t uid1, uint32_t uid2,
uint32_t *seq1_r, uint32_t *seq2_r);
void mailbox_get_uid_range(struct mailbox *box,
const ARRAY_TYPE(seq_range) *seqs,
ARRAY_TYPE(seq_range) *uids);
bool mailbox_get_expunges(struct mailbox *box, uint64_t prev_modseq,
const ARRAY_TYPE(seq_range) *uids_filter,
ARRAY_TYPE(mailbox_expunge_rec) *expunges);
bool mailbox_get_expunged_uids(struct mailbox *box, uint64_t prev_modseq,
const ARRAY_TYPE(seq_range) *uids_filter,
ARRAY_TYPE(seq_range) *expunged_uids);
struct mailbox_header_lookup_ctx *
mailbox_header_lookup_init(struct mailbox *box, const char *const headers[]);
void mailbox_header_lookup_ref(struct mailbox_header_lookup_ctx *ctx);
void mailbox_header_lookup_unref(struct mailbox_header_lookup_ctx **ctx);
struct mailbox_header_lookup_ctx *
mailbox_header_lookup_merge(const struct mailbox_header_lookup_ctx *hdr1,
const struct mailbox_header_lookup_ctx *hdr2);
struct mail_search_context * ATTR_NULL(3, 5)
mailbox_search_init(struct mailbox_transaction_context *t,
struct mail_search_args *args,
const enum mail_sort_type *sort_program,
enum mail_fetch_field wanted_fields,
struct mailbox_header_lookup_ctx *wanted_headers);
int mailbox_search_deinit(struct mail_search_context **ctx);
void mailbox_search_set_progress_hidden(struct mail_search_context *ctx,
bool hidden);
void mailbox_search_reset_progress_start(struct mail_search_context *ctx);
bool mailbox_search_next(struct mail_search_context *ctx, struct mail **mail_r);
bool mailbox_search_next_nonblock(struct mail_search_context *ctx,
struct mail **mail_r, bool *tryagain_r);
void mailbox_search_notify(struct mailbox *box, struct mail_search_context *ctx);
bool mailbox_search_seen_lost_data(struct mail_search_context *ctx);
void mailbox_search_mail_detach(struct mail_search_context *ctx,
struct mail *mail);
struct mail_search_result *
mailbox_search_result_save(struct mail_search_context *ctx,
enum mailbox_search_result_flags flags);
void mailbox_search_result_free(struct mail_search_result **result);
int mailbox_search_result_build(struct mailbox_transaction_context *t,
struct mail_search_args *args,
enum mailbox_search_result_flags flags,
struct mail_search_result **result_r);
const ARRAY_TYPE(seq_range) *
mailbox_search_result_get(struct mail_search_result *result);
void mailbox_search_result_sync(struct mail_search_result *result,
ARRAY_TYPE(seq_range) *removed_uids,
ARRAY_TYPE(seq_range) *added_uids);
int mailbox_keywords_create(struct mailbox *box, const char *const keywords[],
struct mail_keywords **keywords_r);
struct mail_keywords *
mailbox_keywords_create_valid(struct mailbox *box,
const char *const keywords[]);
struct mail_keywords *
mailbox_keywords_create_from_indexes(struct mailbox *box,
const ARRAY_TYPE(keyword_indexes) *idx);
struct mail_keywords *mailbox_keywords_merge(struct mail_keywords *keywords1,
struct mail_keywords *keywords2);
void mailbox_keywords_ref(struct mail_keywords *keywords);
void mailbox_keywords_unref(struct mail_keywords **keywords);
bool mailbox_keyword_is_valid(struct mailbox *box, const char *keyword,
const char **error_r);
struct mail_save_context *
mailbox_save_alloc(struct mailbox_transaction_context *t);
void mailbox_save_set_flags(struct mail_save_context *ctx,
enum mail_flags flags,
struct mail_keywords *keywords);
void mailbox_save_copy_flags(struct mail_save_context *ctx, struct mail *mail);
void mailbox_save_set_min_modseq(struct mail_save_context *ctx,
uint64_t min_modseq);
void mailbox_save_set_received_date(struct mail_save_context *ctx,
time_t received_date, int timezone_offset);
void mailbox_save_set_save_date(struct mail_save_context *ctx,
time_t save_date);
void mailbox_save_set_from_envelope(struct mail_save_context *ctx,
const char *envelope);
void mailbox_save_set_uid(struct mail_save_context *ctx, uint32_t uid);
void mailbox_save_set_guid(struct mail_save_context *ctx, const char *guid);
void mailbox_save_set_pop3_uidl(struct mail_save_context *ctx,
const char *uidl);
void mailbox_save_set_pop3_order(struct mail_save_context *ctx,
unsigned int order);
struct mail *mailbox_save_get_dest_mail(struct mail_save_context *ctx);
int mailbox_save_begin(struct mail_save_context **ctx, struct istream *input);
int mailbox_save_continue(struct mail_save_context *ctx);
int mailbox_save_finish(struct mail_save_context **ctx);
void mailbox_save_cancel(struct mail_save_context **ctx);
struct mailbox_transaction_context *
mailbox_save_get_transaction(struct mail_save_context *ctx);
int mailbox_copy(struct mail_save_context **ctx, struct mail *mail);
int mailbox_move(struct mail_save_context **ctx, struct mail *mail);
int mailbox_save_using_mail(struct mail_save_context **ctx, struct mail *mail);
struct mail *mail_alloc(struct mailbox_transaction_context *t,
enum mail_fetch_field wanted_fields,
struct mailbox_header_lookup_ctx *wanted_headers)
ATTR_NULL(3);
void mail_free(struct mail **mail);
void mail_set_seq(struct mail *mail, uint32_t seq);
bool mail_set_uid(struct mail *mail, uint32_t uid);
void mail_add_temp_wanted_fields(struct mail *mail,
enum mail_fetch_field fields,
struct mailbox_header_lookup_ctx *headers)
ATTR_NULL(3);
struct event *mail_event(struct mail *mail);
enum mail_flags mail_get_flags(struct mail *mail);
const char *const *mail_get_keywords(struct mail *mail);
const ARRAY_TYPE(keyword_indexes) *mail_get_keyword_indexes(struct mail *mail);
uint64_t mail_get_modseq(struct mail *mail);
uint64_t mail_get_pvt_modseq(struct mail *mail);
int mail_get_parts(struct mail *mail, struct message_part **parts_r);
int mail_get_date(struct mail *mail, time_t *date_r, int *timezone_r);
int mail_get_received_date(struct mail *mail, time_t *date_r);
int mail_get_save_date(struct mail *mail, time_t *date_r);
int mail_get_virtual_size(struct mail *mail, uoff_t *size_r);
int mail_get_physical_size(struct mail *mail, uoff_t *size_r);
int mail_get_first_header(struct mail *mail, const char *field,
const char **value_r);
int mail_get_first_header_utf8(struct mail *mail, const char *field,
const char **value_r);
int mail_get_headers(struct mail *mail, const char *field,
const char *const **value_r);
int mail_get_headers_utf8(struct mail *mail, const char *field,
const char *const **value_r);
int mail_get_header_stream(struct mail *mail,
struct mailbox_header_lookup_ctx *headers,
struct istream **stream_r);
int mail_get_stream(struct mail *mail, struct message_size *hdr_size,
struct message_size *body_size, struct istream **stream_r)
ATTR_NULL(2, 3);
int mail_get_stream_because(struct mail *mail, struct message_size *hdr_size,
struct message_size *body_size,
const char *reason, struct istream **stream_r)
ATTR_NULL(2, 3);
int mail_get_hdr_stream(struct mail *mail, struct message_size *hdr_size,
struct istream **stream_r) ATTR_NULL(2);
int mail_get_hdr_stream_because(struct mail *mail,
struct message_size *hdr_size,
const char *reason, struct istream **stream_r);
int mail_get_binary_stream(struct mail *mail, const struct message_part *part,
bool include_hdr, uoff_t *size_r,
bool *binary_r, struct istream **stream_r);
int mail_get_binary_size(struct mail *mail, const struct message_part *part,
bool include_hdr, uoff_t *size_r,
unsigned int *lines_r);
int mail_get_special(struct mail *mail, enum mail_fetch_field field,
const char **value_r);
int mail_get_backend_mail(struct mail *mail, struct mail **real_mail_r);
int mail_get_message_id(struct mail *mail, const char **value_r);
int mail_get_message_id_no_validation(struct mail *mail, const char **value_r);
void mail_update_flags(struct mail *mail, enum modify_type modify_type,
enum mail_flags flags);
void mail_update_keywords(struct mail *mail, enum modify_type modify_type,
struct mail_keywords *keywords);
void mail_update_modseq(struct mail *mail, uint64_t min_modseq);
void mail_update_pvt_modseq(struct mail *mail, uint64_t min_pvt_modseq);
void mail_update_pop3_uidl(struct mail *mail, const char *uidl);
void mail_expunge(struct mail *mail);
int mail_precache(struct mail *mail);
void mail_set_cache_corrupted(struct mail *mail,
enum mail_fetch_field field,
const char *reason);
void mail_generate_guid_128_hash(const char *guid, guid_128_t guid_128_r);
int mail_parse_human_timestamp(const char *str, time_t *timestamp_r,
bool *utc_r);
#endif