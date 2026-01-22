#ifndef VIRTUAL_STORAGE_H
#define VIRTUAL_STORAGE_H
#include "seq-range-array.h"
#include "index-storage.h"
#define VIRTUAL_STORAGE_NAME "virtual"
#define VIRTUAL_SUBSCRIPTION_FILE_NAME ".virtual-subscriptions"
#define VIRTUAL_CONFIG_FNAME "dovecot-virtual"
#define VIRTUAL_CONTEXT(obj) \
MODULE_CONTEXT(obj, virtual_storage_module)
#define VIRTUAL_CONTEXT_REQUIRE(obj) \
MODULE_CONTEXT_REQUIRE(obj, virtual_storage_module)
#define VIRTUAL_MAIL_INDEX_EXT2_HEADER_VERSION 1
struct virtual_save_context;
struct virtual_mail_index_header {
uint32_t change_counter;
uint32_t mailbox_count;
uint32_t highest_mailbox_id;
uint32_t search_args_crc32;
};
struct virtual_mail_index_mailbox_record {
uint32_t id;
uint32_t name_len;
uint32_t uid_validity;
uint32_t next_uid;
uint64_t highest_modseq;
};
struct virtual_mail_index_ext2_header {
uint8_t version;
uint8_t ext_record_size;
uint16_t hdr_size;
uint32_t change_counter;
};
struct virtual_mail_index_mailbox_ext2_record {
uint8_t guid[16];
};
struct virtual_mail_index_record {
uint32_t mailbox_id;
uint32_t real_uid;
};
struct virtual_storage {
struct mail_storage storage;
ARRAY_TYPE(const_string) open_stack;
unsigned int max_open_mailboxes;
};
struct virtual_backend_uidmap {
uint32_t real_uid;
uint32_t virtual_uid;
};
struct virtual_backend_box {
union mailbox_module_context module_ctx;
struct virtual_mailbox *virtual_mbox;
struct virtual_backend_box *prev_open, *next_open;
uint32_t mailbox_id;
const char *name;
unsigned int sync_mailbox_idx1;
uint32_t sync_uid_validity;
uint32_t sync_next_uid;
uint64_t sync_highest_modseq;
guid_128_t sync_guid;
uint64_t ondisk_highest_modseq;
struct mail_search_args *search_args;
struct mail_search_result *search_result;
struct mailbox *box;
ARRAY(struct virtual_backend_uidmap) uids;
struct mail *sync_mail;
ARRAY_TYPE(seq_range) sync_pending_removes;
ARRAY_TYPE(seq_range) sync_outside_expunges;
struct imap_match_glob *glob;
struct mail_namespace *ns;
const char *metadata_entry, *metadata_value;
struct mailbox_list_notify *notify;
bool open_tracked:1;
bool open_failed:1;
bool sync_seen:1;
bool wildcard:1;
bool clear_recent:1;
bool negative_match:1;
bool uids_nonsorted:1;
bool search_args_initialized:1;
bool deleted:1;
bool notify_changes_started:1;
bool first_sync:1;
};
ARRAY_DEFINE_TYPE(virtual_backend_box, struct virtual_backend_box *);
struct virtual_mailbox {
struct mailbox box;
struct virtual_storage *storage;
uint32_t virtual_ext_id;
uint32_t virtual_ext2_id;
uint32_t virtual_guid_ext_id;
uint32_t prev_uid_validity;
uint32_t prev_change_counter;
uint32_t highest_mailbox_id;
uint32_t search_args_crc32;
guid_128_t guid;
struct virtual_backend_box *lookup_prev_bbox;
uint32_t sync_virtual_next_uid;
ARRAY_TYPE(virtual_backend_box) backend_boxes;
struct virtual_backend_box *save_bbox;
struct virtual_backend_box *open_backend_boxes_head;
struct virtual_backend_box *open_backend_boxes_tail;
unsigned int backends_open_count;
ARRAY_TYPE(mailbox_virtual_patterns) list_include_patterns;
ARRAY_TYPE(mailbox_virtual_patterns) list_exclude_patterns;
bool uids_mapped:1;
bool sync_initialized:1;
bool inconsistent:1;
bool have_guid_flags_set:1;
bool have_guids:1;
bool have_save_guids:1;
bool ext_header_rewrite:1;
};
extern MODULE_CONTEXT_DEFINE(virtual_storage_module,
&mail_storage_module_register);
extern struct mail_storage virtual_storage;
extern struct mail_vfuncs virtual_mail_vfuncs;
int virtual_config_read(struct virtual_mailbox *mbox);
void virtual_config_free(struct virtual_mailbox *mbox);
int virtual_mailbox_ext_header_read(struct virtual_mailbox *mbox,
struct mail_index_view *view,
bool *broken_r);
struct virtual_backend_box *
virtual_backend_box_lookup_name(struct virtual_mailbox *mbox, const char *name);
bool virtual_backend_box_lookup(struct virtual_mailbox *mbox, uint32_t mailbox_id,
struct virtual_backend_box **vbox_r);
int virtual_backend_box_open(struct virtual_mailbox *mbox,
struct virtual_backend_box *bbox);
void virtual_backend_box_close(struct virtual_mailbox *mbox,
struct virtual_backend_box *bbox);
void virtual_backend_box_accessed(struct virtual_mailbox *mbox,
struct virtual_backend_box *bbox);
void virtual_backend_box_sync_mail_unset(struct virtual_backend_box *bbox);
struct mail_search_context *
virtual_search_init(struct mailbox_transaction_context *t,
struct mail_search_args *args,
const enum mail_sort_type *sort_program,
enum mail_fetch_field wanted_fields,
struct mailbox_header_lookup_ctx *wanted_headers);
int virtual_search_deinit(struct mail_search_context *ctx);
bool virtual_search_next_nonblock(struct mail_search_context *ctx,
struct mail **mail_r, bool *tryagain_r);
bool virtual_search_next_update_seq(struct mail_search_context *ctx);
struct mail *
virtual_mail_alloc(struct mailbox_transaction_context *t,
enum mail_fetch_field wanted_fields,
struct mailbox_header_lookup_ctx *wanted_headers);
struct mail *
virtual_mail_set_backend_mail(struct mail *mail,
struct virtual_backend_box *bbox);
void virtual_mail_set_unattached_backend_mail(struct mail *mail,
struct mail *backend_mail);
struct mailbox_sync_context *
virtual_storage_sync_init(struct mailbox *box, enum mailbox_sync_flags flags);
struct mail_save_context *
virtual_save_alloc(struct mailbox_transaction_context *t);
int virtual_save_begin(struct mail_save_context *ctx, struct istream *input);
int virtual_save_continue(struct mail_save_context *ctx);
int virtual_save_finish(struct mail_save_context *ctx);
void virtual_save_cancel(struct mail_save_context *ctx);
void virtual_save_free(struct mail_save_context *ctx);
void virtual_box_copy_error(struct mailbox *dest, struct mailbox *src);
void virtual_backend_mailbox_allocated(struct mailbox *box);
void virtual_backend_mailbox_opened(struct mailbox *box);
#endif