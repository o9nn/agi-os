#ifndef DSYNC_IBC_H
#define DSYNC_IBC_H
#include "ioloop.h"
#include "guid.h"
#include "mail-error.h"
#include "dsync-brain.h"
struct dsync_mailbox;
struct dsync_mailbox_state;
struct dsync_mailbox_node;
struct dsync_mailbox_delete;
struct dsync_mailbox_attribute;
struct dsync_mail;
struct dsync_mail_change;
struct dsync_mail_request;
enum dsync_ibc_send_ret {
DSYNC_IBC_SEND_RET_OK	= 1,
DSYNC_IBC_SEND_RET_FULL	= 0
};
enum dsync_ibc_recv_ret {
DSYNC_IBC_RECV_RET_FINISHED	= -1,
DSYNC_IBC_RECV_RET_TRYAGAIN	= 0,
DSYNC_IBC_RECV_RET_OK		= 1
};
enum dsync_ibc_eol_type {
DSYNC_IBC_EOL_MAILBOX_STATE,
DSYNC_IBC_EOL_MAILBOX_TREE,
DSYNC_IBC_EOL_MAILBOX_ATTRIBUTE,
DSYNC_IBC_EOL_MAILBOX,
DSYNC_IBC_EOL_MAIL_CHANGES,
DSYNC_IBC_EOL_MAIL_REQUESTS,
DSYNC_IBC_EOL_MAILS
};
struct dsync_ibc_settings {
const char *hostname;
const char *sync_ns_prefixes;
const char *sync_box;
const char *virtual_all_box;
guid_128_t sync_box_guid;
const char *const *exclude_mailboxes;
time_t sync_since_timestamp;
time_t sync_until_timestamp;
uoff_t sync_max_size;
const char *sync_flags;
const char *const *hashed_headers;
char alt_char;
enum dsync_brain_sync_type sync_type;
enum dsync_brain_flags brain_flags;
bool hdr_hash_v2;
bool hdr_hash_v3;
unsigned int lock_timeout;
unsigned int import_commit_msgs_interval;
};
void dsync_ibc_init_pipe(struct dsync_ibc **ibc1_r,
struct dsync_ibc **ibc2_r);
struct dsync_ibc *
dsync_ibc_init_stream(struct istream *input, struct ostream *output,
const char *name, const char *temp_path_prefix,
unsigned int timeout_secs);
void dsync_ibc_deinit(struct dsync_ibc **ibc);
void dsync_ibc_set_io_callback(struct dsync_ibc *ibc,
io_callback_t *callback, void *context);
void dsync_ibc_send_handshake(struct dsync_ibc *ibc,
const struct dsync_ibc_settings *set);
enum dsync_ibc_recv_ret
dsync_ibc_recv_handshake(struct dsync_ibc *ibc,
const struct dsync_ibc_settings **set_r);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_end_of_list(struct dsync_ibc *ibc, enum dsync_ibc_eol_type type);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_mailbox_state(struct dsync_ibc *ibc,
const struct dsync_mailbox_state *state);
enum dsync_ibc_recv_ret
dsync_ibc_recv_mailbox_state(struct dsync_ibc *ibc,
struct dsync_mailbox_state *state_r);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_mailbox_tree_node(struct dsync_ibc *ibc,
const char *const *name,
const struct dsync_mailbox_node *node);
enum dsync_ibc_recv_ret
dsync_ibc_recv_mailbox_tree_node(struct dsync_ibc *ibc,
const char *const **name_r,
const struct dsync_mailbox_node **node_r);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_mailbox_deletes(struct dsync_ibc *ibc,
const struct dsync_mailbox_delete *deletes,
unsigned int count, char hierarchy_sep,
char escape_char);
enum dsync_ibc_recv_ret
dsync_ibc_recv_mailbox_deletes(struct dsync_ibc *ibc,
const struct dsync_mailbox_delete **deletes_r,
unsigned int *count_r, char *hierarchy_sep_r,
char *escape_char_r);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_mailbox(struct dsync_ibc *ibc,
const struct dsync_mailbox *dsync_box);
enum dsync_ibc_recv_ret
dsync_ibc_recv_mailbox(struct dsync_ibc *ibc,
const struct dsync_mailbox **dsync_box_r);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_mailbox_attribute(struct dsync_ibc *ibc,
const struct dsync_mailbox_attribute *attr);
enum dsync_ibc_recv_ret
dsync_ibc_recv_mailbox_attribute(struct dsync_ibc *ibc,
const struct dsync_mailbox_attribute **attr_r);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_change(struct dsync_ibc *ibc,
const struct dsync_mail_change *change);
enum dsync_ibc_recv_ret
dsync_ibc_recv_change(struct dsync_ibc *ibc,
const struct dsync_mail_change **change_r);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_mail_request(struct dsync_ibc *ibc,
const struct dsync_mail_request *request);
enum dsync_ibc_recv_ret
dsync_ibc_recv_mail_request(struct dsync_ibc *ibc,
const struct dsync_mail_request **request_r);
enum dsync_ibc_send_ret ATTR_NOWARN_UNUSED_RESULT
dsync_ibc_send_mail(struct dsync_ibc *ibc, const struct dsync_mail *mail);
enum dsync_ibc_recv_ret
dsync_ibc_recv_mail(struct dsync_ibc *ibc, struct dsync_mail **mail_r);
void dsync_ibc_send_finish(struct dsync_ibc *ibc, const char *error,
enum mail_error mail_error,
bool require_full_resync);
enum dsync_ibc_recv_ret
dsync_ibc_recv_finish(struct dsync_ibc *ibc, const char **error_r,
enum mail_error *mail_error_r,
bool *require_full_resync_r);
void dsync_ibc_close_mail_streams(struct dsync_ibc *ibc);
bool dsync_ibc_has_failed(struct dsync_ibc *ibc);
bool dsync_ibc_has_timed_out(struct dsync_ibc *ibc);
bool dsync_ibc_is_send_queue_full(struct dsync_ibc *ibc);
bool dsync_ibc_has_pending_data(struct dsync_ibc *ibc);
#endif