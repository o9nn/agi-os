#ifndef PUSH_NOTIFICATION_EVENT_FLAGSSET_H
#define PUSH_NOTIFICATION_EVENT_FLAGSSET_H
#include "mail-types.h"
struct push_notification_event_flagsset_config {
bool hide_deleted;
bool hide_seen;
};
struct push_notification_event_flagsset_data {
enum mail_flags flags_set;
ARRAY_TYPE(keywords) keywords_set;
};
#endif