#ifndef PUSH_NOTIFICATION_EVENT_MESSAGENEW_H
#define PUSH_NOTIFICATION_EVENT_MESSAGENEW_H
#include "push-notification-event-message-common.h"
struct push_notification_event_messagenew_config {
enum push_notification_event_message_flags flags;
};
struct push_notification_event_messagenew_data {
const char *from;
const char *to;
const char *subject;
time_t date;
int date_tz;
const char *snippet;
bool flags_set;
enum mail_flags flags;
const char *const *keywords;
const char *message_id;
struct push_notification_message_ext ext;
};
#endif