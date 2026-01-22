#ifndef PUSH_NOTIFICATION_DRIVERS_H
#define PUSH_NOTIFICATION_DRIVERS_H
#include "mail-user.h"
#include "push-notification-triggers.h"
struct mail_user;
struct push_notification_driver_config;
struct push_notification_driver_txn;
struct push_notification_driver_user;
struct push_notification_txn_mbox;
struct push_notification_txn_msg;
HASH_TABLE_DEFINE_TYPE(push_notification_config, const char *, const char *);
HASH_TABLE_DEFINE_TYPE(push_notification_msgs, void *,
struct push_notification_txn_msg *);
struct push_notification_driver_vfuncs {
int (*init)(struct push_notification_driver_config *config,
struct mail_user *user, pool_t pool, void **context,
const char **error_r);
bool (*begin_txn)(struct push_notification_driver_txn *dtxn);
void (*process_mbox)(struct push_notification_driver_txn *dtxn,
struct push_notification_txn_mbox *mbox);
void (*process_msg)(struct push_notification_driver_txn *dtxn,
struct push_notification_txn_msg *msg);
void (*end_txn)(struct push_notification_driver_txn *dtxn,
bool success);
void (*deinit)(struct push_notification_driver_user *duser);
void (*cleanup)(void);
};
struct push_notification_driver {
const char *name;
struct push_notification_driver_vfuncs v;
};
struct push_notification_driver_config {
HASH_TABLE_TYPE(push_notification_config) config;
const char *raw_config;
};
struct push_notification_driver_user {
const struct push_notification_driver *driver;
void *context;
};
struct push_notification_driver_txn {
const struct push_notification_driver_user *duser;
struct push_notification_txn *ptxn;
void *context;
};
struct push_notification_driver_list {
ARRAY(struct push_notification_driver_user *) drivers;
};
struct push_notification_user {
union mail_user_module_context module_ctx;
struct push_notification_driver_list *driverlist;
};
struct push_notification_trigger_ctx {
const char *name;
void *context;
};
struct push_notification_txn {
pool_t pool;
struct mailbox *mbox;
struct mail_user *muser;
struct push_notification_user *puser;
bool initialized;
enum push_notification_event_trigger trigger;
struct push_notification_trigger_ctx *trigger_ctx;
ARRAY(struct push_notification_driver_txn *) drivers;
ARRAY(struct push_notification_event_config *) events;
struct event *event;
struct push_notification_txn_mbox *mbox_txn;
HASH_TABLE_TYPE(push_notification_msgs) messages;
struct mailbox_transaction_context *t;
};
int push_notification_driver_init(
struct mail_user *user, const char *config_in, pool_t pool,
struct push_notification_driver_user **duser_r);
void push_notification_driver_cleanup_all(void);
void ATTR_FORMAT(3, 4)
push_notification_driver_debug(const char *label, struct mail_user *user,
const char *fmt, ...);
void push_notification_driver_register(
const struct push_notification_driver *driver);
void push_notification_driver_unregister(
const struct push_notification_driver *driver);
#endif