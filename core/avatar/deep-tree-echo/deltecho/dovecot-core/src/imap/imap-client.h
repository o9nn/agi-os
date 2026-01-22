#ifndef IMAP_CLIENT_H
#define IMAP_CLIENT_H
#include "imap-commands.h"
#include "message-size.h"
#define CLIENT_COMMAND_QUEUE_MAX_SIZE 4
#define CLIENT_MAX_SEARCH_UPDATES 10
struct client;
struct mail_storage;
struct mail_storage_service_ctx;
struct lda_settings;
struct imap_parser;
struct imap_arg;
struct imap_urlauth_context;
struct mailbox_keywords {
const ARRAY_TYPE(keywords) *names;
unsigned int announce_count;
};
struct imap_search_update {
char *tag;
struct mail_search_result *result;
bool return_uids;
pool_t fetch_pool;
struct imap_fetch_context *fetch_ctx;
};
enum client_command_state {
CLIENT_COMMAND_STATE_WAIT_INPUT,
CLIENT_COMMAND_STATE_WAIT_OUTPUT,
CLIENT_COMMAND_STATE_WAIT_EXTERNAL,
CLIENT_COMMAND_STATE_WAIT_UNAMBIGUITY,
CLIENT_COMMAND_STATE_WAIT_SYNC,
CLIENT_COMMAND_STATE_DONE
};
struct client_command_stats {
struct timeval start_time;
struct timeval last_run_timeval;
uint64_t start_ioloop_wait_usecs;
uint64_t running_usecs;
uint64_t lock_wait_usecs;
uint64_t bytes_in, bytes_out;
};
struct client_command_stats_start {
struct timeval timeval;
uint64_t lock_wait_usecs;
uint64_t bytes_in, bytes_out;
};
struct client_command_context {
struct client_command_context *prev, *next;
struct client *client;
struct event *event;
struct event *global_event;
pool_t pool;
const char *tag;
const char *name;
const char *args;
const char *human_args;
enum command_flags cmd_flags;
const char *tagline_reply;
command_func_t *func;
void *context;
ARRAY(union imap_module_context *) module_contexts;
struct imap_parser *parser;
enum client_command_state state;
struct client_command_stats stats;
struct client_command_stats_start stats_start;
struct imap_client_sync_context *sync;
bool uid:1;
bool cancel:1;
bool param_error:1;
bool search_save_result:1;
bool search_save_result_used:1;
bool temp_executed:1;
bool tagline_sent:1;
bool executing:1;
bool internal:1;
};
struct imap_client_vfuncs {
void (*init)(struct client *client);
void (*destroy)(struct client *client, const char *reason);
void (*send_tagline)(struct client_command_context *cmd,
const char *data);
int (*sync_notify_more)(struct imap_sync_context *ctx);
int (*state_export)(struct client *client, bool internal,
buffer_t *dest, const char **error_r);
ssize_t (*state_import)(struct client *client, bool internal,
const unsigned char *data, size_t size,
const char **error_r);
};
struct client {
struct client *prev, *next;
struct imap_client_vfuncs v;
struct event *event;
const char *const *userdb_fields;
int fd_in, fd_out;
struct io *io;
struct istream *input, *pre_rawlog_input, *post_rawlog_input;
struct ostream *output, *pre_rawlog_output, *post_rawlog_output;
struct timeout *to_idle, *to_idle_output, *to_delayed_input;
guid_128_t anvil_conn_guid;
pool_t pool;
const struct imap_settings *set;
const struct smtp_submit_settings *smtp_set;
string_t *capability_string;
const char *disconnect_reason;
struct mail_user *user;
struct mailbox *mailbox;
struct mailbox_keywords keywords;
unsigned int sync_counter;
uint32_t messages_count, recent_count, uidvalidity;
uoff_t prev_output_size;
ARRAY(bool) enabled_features;
time_t last_input, last_output;
unsigned int bad_counter;
struct imap_parser *free_parser;
pool_t command_pool;
struct client_command_context *command_queue;
unsigned int command_queue_size;
char *last_cmd_name;
struct client_command_stats last_cmd_stats;
uint64_t sync_last_full_modseq;
uint64_t highest_fetch_modseq;
ARRAY_TYPE(seq_range) fetch_failed_uids;
unsigned int fetch_hdr_count, fetch_body_count;
uint64_t fetch_hdr_bytes, fetch_body_bytes;
unsigned int deleted_count, expunged_count, trashed_count;
unsigned int autoexpunged_count, append_count;
ARRAY_TYPE(seq_range) search_saved_uidset;
ARRAY(struct imap_search_update) search_updates;
struct imap_notify_context *notify_ctx;
uint32_t notify_uidnext;
const struct compression_handler *compress_handler;
struct client_command_context *input_lock;
struct client_command_context *output_cmd_lock;
struct client_command_context *mailbox_change_lock;
struct imap_urlauth_context *urlauth_ctx;
ARRAY(union imap_module_context *) module_contexts;
bool sync_seen_deletes:1;
bool logged_out:1;
bool disconnected:1;
bool hibernated:1;
bool unhibernated:1;
bool destroyed:1;
bool handling_input:1;
bool syncing:1;
bool id_logged:1;
bool mailbox_examined:1;
bool anvil_sent:1;
bool tls_compression:1;
bool input_skip_line:1;
bool modseqs_sent_since_sync:1;
bool notify_immediate_expunges:1;
bool notify_count_changes:1;
bool notify_flag_changes:1;
bool nonpermanent_modseqs:1;
bool state_import_bad_idle_done:1;
bool state_import_idle_continue:1;
};
struct imap_module_register {
unsigned int id;
};
union imap_module_context {
struct imap_client_vfuncs super;
struct imap_module_register *reg;
};
extern struct imap_module_register imap_module_register;
extern struct client *imap_clients;
extern unsigned int imap_client_count;
extern unsigned int imap_feature_condstore;
extern unsigned int imap_feature_qresync;
struct client *client_create(int fd_in, int fd_out, bool unhibernated,
struct event *event, struct mail_user *user,
const struct imap_settings *set,
const struct smtp_submit_settings *smtp_set);
void client_create_finish_io(struct client *client);
int client_create_finish(struct client *client, const char **error_r);
void client_add_istream_prefix(struct client *client,
const unsigned char *data, size_t size);
void client_destroy(struct client *client, const char *reason) ATTR_NULL(2);
void client_disconnect(struct client *client, const char *reason);
void client_disconnect_with_error(struct client *client,
const char *client_error);
void client_kick(struct client *client);
void client_add_capability(struct client *client, const char *capability);
void client_send_line(struct client *client, const char *data);
int client_send_line_next(struct client *client, const char *data);
void client_send_tagline(struct client_command_context *cmd, const char *data);
void client_send_command_error(struct client_command_context *cmd,
const char *client_error);
void client_send_internal_error(struct client_command_context *cmd);
bool client_read_args(struct client_command_context *cmd, unsigned int count,
unsigned int flags, const struct imap_arg **args_r);
bool client_read_string_args(struct client_command_context *cmd,
unsigned int count, ...);
void client_args_finished(struct client_command_context *cmd,
const struct imap_arg *args);
bool client_handle_search_save_ambiguity(struct client_command_context *cmd);
void client_enable(struct client *client, unsigned int feature_idx);
bool client_has_enabled(struct client *client, unsigned int feature_idx);
enum mailbox_feature client_enabled_mailbox_features(struct client *client);
const char *const *client_enabled_features(struct client *client);
bool imap_client_hibernate(struct client **client, const char **reason_r);
struct imap_search_update *
client_search_update_lookup(struct client *client, const char *tag,
unsigned int *idx_r);
void client_search_updates_free(struct client *client);
struct client_command_context *client_command_alloc(struct client *client);
void client_command_init_finished(struct client_command_context *cmd);
void client_command_cancel(struct client_command_context **cmd);
void client_command_free(struct client_command_context **cmd);
bool client_handle_unfinished_cmd(struct client_command_context *cmd);
void client_continue_pending_input(struct client *client);
void client_add_missing_io(struct client *client);
const char *client_stats(struct client *client);
void client_input(struct client *client);
bool client_handle_input(struct client *client);
int client_output(struct client *client);
void clients_init(void);
void clients_destroy_all(void);
#endif