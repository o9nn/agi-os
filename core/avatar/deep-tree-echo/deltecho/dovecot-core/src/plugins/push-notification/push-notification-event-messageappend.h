#ifndef PUSH_NOTIFICATION_EVENT_MESSAGEAPPEND_H
#define PUSH_NOTIFICATION_EVENT_MESSAGEAPPEND_H
struct push_notification_event_messageappend_config {
enum push_notification_event_message_flags flags;
};
struct push_notification_event_messageappend_data {
const char *from;
const char *to;
const char *subject;
const char *snippet;
time_t date;
int date_tz;
bool flags_set;
enum mail_flags flags;
const char *const *keywords;
const char *message_id;
struct push_notification_message_ext ext;
};
#endif