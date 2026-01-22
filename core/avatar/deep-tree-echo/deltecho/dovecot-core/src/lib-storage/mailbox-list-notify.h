#ifndef MAILBOX_LIST_NOTIFY_H
#define MAILBOX_LIST_NOTIFY_H
#include "guid.h"
struct mailbox_list_notify;
enum mailbox_list_notify_event {
MAILBOX_LIST_NOTIFY_CREATE		= 0x01,
MAILBOX_LIST_NOTIFY_DELETE		= 0x02,
MAILBOX_LIST_NOTIFY_RENAME		= 0x04,
MAILBOX_LIST_NOTIFY_SUBSCRIBE		= 0x08,
MAILBOX_LIST_NOTIFY_UNSUBSCRIBE		= 0x10,
MAILBOX_LIST_NOTIFY_UIDVALIDITY		= 0x20,
MAILBOX_LIST_NOTIFY_APPENDS		= 0x40,
MAILBOX_LIST_NOTIFY_EXPUNGES		= 0x80,
MAILBOX_LIST_NOTIFY_SEEN_CHANGES	= 0x100,
MAILBOX_LIST_NOTIFY_MODSEQ_CHANGES	= 0x200
#define MAILBOX_LIST_NOTIFY_STATUS \
(MAILBOX_LIST_NOTIFY_APPENDS | \
MAILBOX_LIST_NOTIFY_EXPUNGES | \
MAILBOX_LIST_NOTIFY_SEEN_CHANGES | \
MAILBOX_LIST_NOTIFY_MODSEQ_CHANGES)
};
struct mailbox_list_notify {
struct mailbox_list *list;
enum mailbox_list_notify_event mask;
};
struct mailbox_list_notify_rec {
enum mailbox_list_notify_event events;
const char *storage_name, *vname;
guid_128_t guid;
const char *old_vname;
};
typedef void mailbox_list_notify_callback_t(void *);
int mailbox_list_notify_init(struct mailbox_list *list,
enum mailbox_list_notify_event mask,
struct mailbox_list_notify **notify_r);
void mailbox_list_notify_deinit(struct mailbox_list_notify **notify);
int mailbox_list_notify_next(struct mailbox_list_notify *notify,
const struct mailbox_list_notify_rec **rec_r);
void mailbox_list_notify_wait(struct mailbox_list_notify *notify,
mailbox_list_notify_callback_t *callback, void *context);
#define mailbox_list_notify_wait(notify, callback, context) \
mailbox_list_notify_wait(notify - CALLBACK_TYPECHECK(callback, void (*)(typeof(context))), \
(mailbox_list_notify_callback_t*)callback, context);
void mailbox_list_notify_flush(struct mailbox_list_notify *notify);
#endif