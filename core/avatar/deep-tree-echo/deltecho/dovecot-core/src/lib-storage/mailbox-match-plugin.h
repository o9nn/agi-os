#ifndef MAILBOX_MATCH_PLUGIN_H
#define MAILBOX_MATCH_PLUGIN_H
struct mailbox;
struct mailbox_match_plugin *
mailbox_match_plugin_init(struct mail_user *user, const char *set_prefix);
void mailbox_match_plugin_deinit(struct mailbox_match_plugin **match);
bool mailbox_match_plugin_exclude(struct mailbox_match_plugin *match,
struct mailbox *box);
#endif