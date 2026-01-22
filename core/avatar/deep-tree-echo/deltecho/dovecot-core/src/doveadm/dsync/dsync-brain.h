#ifndef DSYNC_BRAIN_H
#define DSYNC_BRAIN_H
#include "module-context.h"
#include "guid.h"
#include "mail-error.h"
#include "mailbox-list-private.h"
struct mail_namespace;
struct mail_user;
struct dsync_ibc;
enum dsync_brain_flags {
DSYNC_BRAIN_FLAG_SEND_MAIL_REQUESTS	= 0x01,
DSYNC_BRAIN_FLAG_BACKUP_SEND		= 0x02,
DSYNC_BRAIN_FLAG_BACKUP_RECV		= 0x04,
DSYNC_BRAIN_FLAG_DEBUG			= 0x08,
DSYNC_BRAIN_FLAG_SYNC_VISIBLE_NAMESPACES= 0x10,
DSYNC_BRAIN_FLAG_NO_MAIL_SYNC		= 0x20,
DSYNC_BRAIN_FLAG_NO_BACKUP_OVERWRITE	= 0x40,
DSYNC_BRAIN_FLAG_PURGE_REMOTE		= 0x80,
DSYNC_BRAIN_FLAG_NO_MAIL_PREFETCH	= 0x100,
DSYNC_BRAIN_FLAG_NO_NOTIFY		= 0x400,
DSYNC_BRAIN_FLAG_EMPTY_HDR_WORKAROUND	= 0x800,
DSYNC_BRAIN_FLAG_NO_HEADER_HASHES	= 0x1000,
};
enum dsync_brain_sync_type {
DSYNC_BRAIN_SYNC_TYPE_UNKNOWN,
DSYNC_BRAIN_SYNC_TYPE_FULL,
DSYNC_BRAIN_SYNC_TYPE_CHANGED,
DSYNC_BRAIN_SYNC_TYPE_STATE
};
struct dsync_brain_settings {
const char *process_title_prefix;
ARRAY(struct mail_namespace *) sync_namespaces;
const char *sync_box;
const char *virtual_all_box;
guid_128_t sync_box_guid;
const char *const *exclude_mailboxes;
char mailbox_alt_char;
time_t sync_since_timestamp;
time_t sync_until_timestamp;
uoff_t sync_max_size;
const char *sync_flag;
const char *const *hashed_headers;
unsigned int lock_timeout_secs;
unsigned int import_commit_msgs_interval;
const char *state;
};
#define DSYNC_LIST_CONTEXT(obj) \
MODULE_CONTEXT(obj, dsync_mailbox_list_module)
struct dsync_mailbox_list {
union mailbox_list_module_context module_ctx;
bool have_orig_escape_char;
};
extern MODULE_CONTEXT_DEFINE(dsync_mailbox_list_module,
&mailbox_list_module_register);
struct dsync_brain *
dsync_brain_master_init(struct mail_user *user, struct dsync_ibc *ibc,
enum dsync_brain_sync_type sync_type,
enum dsync_brain_flags flags,
const struct dsync_brain_settings *set);
struct dsync_brain *
dsync_brain_slave_init(struct mail_user *user, struct dsync_ibc *ibc,
bool local, const char *process_title_prefix,
char default_alt_char);
int dsync_brain_deinit(struct dsync_brain **brain, enum mail_error *error_r);
bool dsync_brain_run(struct dsync_brain *brain, bool *changed_r);
bool dsync_brain_has_failed(struct dsync_brain *brain);
void dsync_brain_get_state(struct dsync_brain *brain, string_t *output);
enum dsync_brain_sync_type dsync_brain_get_sync_type(struct dsync_brain *brain);
const char *dsync_brain_get_unexpected_changes_reason(struct dsync_brain *brain,
bool *remote_only_r);
bool dsync_brain_want_namespace(struct dsync_brain *brain,
struct mail_namespace *ns);
#endif