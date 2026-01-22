#ifndef IMAP_COMMANDS_H
#define IMAP_COMMANDS_H
struct client_command_context;
#include "mail-storage.h"
#include "mail-namespace.h"
#include "imap-parser.h"
#include "imap-sync.h"
#include "imap-commands-util.h"
typedef bool command_func_t(struct client_command_context *cmd);
typedef void command_hook_callback_t(struct client_command_context *ctx);
enum command_flags {
COMMAND_FLAG_USES_SEQS		= 0x01,
COMMAND_FLAG_BREAKS_SEQS	= 0x02,
COMMAND_FLAG_BREAKS_MAILBOX	= 0x04 | COMMAND_FLAG_BREAKS_SEQS,
COMMAND_FLAG_USES_MAILBOX	= COMMAND_FLAG_BREAKS_MAILBOX |
COMMAND_FLAG_USES_SEQS,
COMMAND_FLAG_REQUIRES_SYNC	= 0x08,
};
struct command {
const char *name;
command_func_t *func;
enum command_flags flags;
};
ARRAY_DEFINE_TYPE(command, struct command);
extern ARRAY_TYPE(command) imap_commands;
void command_register(const char *name, command_func_t *func,
enum command_flags flags);
void command_unregister(const char *name);
void command_register_array(const struct command *cmdarr, unsigned int count);
void command_unregister_array(const struct command *cmdarr, unsigned int count);
void command_hook_register(command_hook_callback_t *pre,
command_hook_callback_t *post);
void command_hook_unregister(command_hook_callback_t *pre,
command_hook_callback_t *post);
bool command_exec(struct client_command_context *cmd);
void command_stats_start(struct client_command_context *cmd);
void command_stats_flush(struct client_command_context *cmd);
struct command *command_find(const char *name);
void commands_init(void);
void commands_deinit(void);
bool cmd_logout(struct client_command_context *cmd);
bool cmd_capability(struct client_command_context *cmd);
bool cmd_noop(struct client_command_context *cmd);
bool cmd_select(struct client_command_context *cmd);
bool cmd_examine(struct client_command_context *cmd);
bool cmd_create(struct client_command_context *cmd);
bool cmd_delete(struct client_command_context *cmd);
bool cmd_rename(struct client_command_context *cmd);
bool cmd_subscribe(struct client_command_context *cmd);
bool cmd_unsubscribe(struct client_command_context *cmd);
bool cmd_list(struct client_command_context *cmd);
bool cmd_lsub(struct client_command_context *cmd);
bool cmd_status(struct client_command_context *cmd);
bool cmd_append(struct client_command_context *cmd);
bool cmd_check(struct client_command_context *cmd);
bool cmd_close(struct client_command_context *cmd);
bool cmd_expunge(struct client_command_context *cmd);
bool cmd_search(struct client_command_context *cmd);
bool cmd_fetch(struct client_command_context *cmd);
bool cmd_store(struct client_command_context *cmd);
bool cmd_copy(struct client_command_context *cmd);
bool cmd_uid(struct client_command_context *cmd);
bool cmd_cancelupdate(struct client_command_context *cmd);
bool cmd_enable(struct client_command_context *cmd);
bool cmd_id(struct client_command_context *cmd);
bool cmd_idle(struct client_command_context *cmd);
bool cmd_namespace(struct client_command_context *cmd);
bool cmd_getmetadata(struct client_command_context *cmd);
bool cmd_setmetadata(struct client_command_context *cmd);
bool cmd_notify(struct client_command_context *cmd);
bool cmd_sort(struct client_command_context *cmd);
bool cmd_thread(struct client_command_context *cmd);
bool cmd_uid_expunge(struct client_command_context *cmd);
bool cmd_move(struct client_command_context *cmd);
bool cmd_unselect(struct client_command_context *cmd);
bool cmd_compress(struct client_command_context *cmd);
bool cmd_genurlauth(struct client_command_context *cmd);
bool cmd_resetkey(struct client_command_context *cmd);
bool cmd_urlfetch(struct client_command_context *cmd);
bool cmd_list_full(struct client_command_context *cmd, bool lsub);
bool cmd_select_full(struct client_command_context *cmd, bool readonly);
bool cmd_subscribe_full(struct client_command_context *cmd, bool subscribe);
#endif