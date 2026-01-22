#ifndef IMAP_SEARCH_ARGS_H
#define IMAP_SEARCH_ARGS_H
#include "mail-search.h"
struct imap_arg;
struct mailbox;
struct client_command_context;
int imap_search_args_build(struct client_command_context *cmd,
const struct imap_arg *args, const char *charset,
struct mail_search_args **search_args_r);
int imap_search_get_anyset(struct client_command_context *cmd,
const char *set, bool uid,
struct mail_search_args **search_args_r);
int imap_search_get_seqset(struct client_command_context *cmd,
const char *set, bool uid,
struct mail_search_args **search_args_r);
void imap_search_anyset_to_uidset(struct client_command_context *cmd,
struct mail_search_args *args);
void imap_search_add_changed_since(struct mail_search_args *search_args,
uint64_t modseq);
#endif