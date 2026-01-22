#ifndef PUSH_NOTIFICATION_EVENTS_H
#define PUSH_NOTIFICATION_EVENTS_H
#include "mail-types.h"
struct mail;
struct mailbox;
struct push_notification_event_config;
struct push_notification_driver_txn;
struct push_notification_txn;
struct push_notification_txn_event;
struct push_notification_txn_mbox;
struct push_notification_txn_msg;
struct push_notification_event_vfuncs_init {
void *(*default_config)(void);
};
struct push_notification_event_vfuncs_mbox {
void (*debug_mbox)(struct push_notification_txn_event *event);
void (*free_mbox)(struct push_notification_txn_event *event);
};
struct push_notification_event_vfuncs_mbox_triggers {
void (*create)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_mbox *mbox);
void (*delete)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_mbox *mbox);
void (*rename)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_mbox *mbox,
struct mailbox *old);
void (*subscribe)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_mbox *mbox);
void (*unsubscribe)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_mbox *mbox);
};
struct push_notification_event_vfuncs_msg {
void (*debug_msg)(struct push_notification_txn_event *event);
void (*free_msg)(struct push_notification_txn_event *event);
};
struct push_notification_event_vfuncs_msg_triggers {
void (*save)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_msg *msg,
struct mail *mail);
void (*append)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_msg *msg,
struct mail *mail);
void (*expunge)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_msg *msg);
void (*flagchange)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_msg *msg,
struct mail *mail, enum mail_flags old_flags);
void (*keywordchange)(struct push_notification_txn *ptxn,
struct push_notification_event_config *ec,
struct push_notification_txn_msg *msg,
struct mail *mail,
const char *const *old_keywords);
};
struct push_notification_event_config {
const struct push_notification_event *event;
struct event *log_event;
void *config;
};
struct push_notification_event {
const char *name;
struct push_notification_event_vfuncs_init init;
struct push_notification_event_vfuncs_mbox mbox;
struct push_notification_event_vfuncs_mbox_triggers mbox_triggers;
struct push_notification_event_vfuncs_msg msg;
struct push_notification_event_vfuncs_msg_triggers msg_triggers;
};
struct push_notification_txn_event {
struct push_notification_event_config *event;
void *data;
};
ARRAY_DEFINE_TYPE(push_notification_event,
const struct push_notification_event *);
extern ARRAY_TYPE(push_notification_event) push_notification_events;
ARRAY_TYPE(push_notification_event) *push_notification_get_events(void);
void push_notification_event_init(struct push_notification_driver_txn *dtxn,
const char *event_name, void *config,
struct event *event);
void push_notification_event_register(
const struct push_notification_event *event);
void push_notification_event_unregister(
const struct push_notification_event *event);
#endif