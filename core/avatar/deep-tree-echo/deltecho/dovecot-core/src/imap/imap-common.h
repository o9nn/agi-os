#ifndef IMAP_COMMON_H
#define IMAP_COMMON_H
#define CLIENT_IDLE_TIMEOUT_MSECS (60*30*1000)
#define CLIENT_OUTPUT_TIMEOUT_MSECS (5*60*1000)
#define CLIENT_OUTPUT_OPTIMAL_SIZE 2048
#define CLIENT_MAX_BAD_COMMANDS 20
#include "lib.h"
#include "imap-client.h"
#include "imap-settings.h"
struct mail_storage_service_input;
typedef void imap_client_created_func_t(struct client **client);
extern imap_client_created_func_t *hook_client_created;
extern bool imap_debug;
extern struct event_category event_category_imap;
imap_client_created_func_t * ATTR_NOWARN_UNUSED_RESULT
imap_client_created_hook_set(imap_client_created_func_t *new_hook);
void imap_refresh_proctitle(void);
void imap_refresh_proctitle_delayed(void);
int client_create_from_input(const struct mail_storage_service_input *input,
int fd_in, int fd_out, bool unhibernated,
struct client **client_r, const char **error_r);
#endif