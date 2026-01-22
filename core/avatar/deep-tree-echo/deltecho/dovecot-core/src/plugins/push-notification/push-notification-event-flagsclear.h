#ifndef PUSH_NOTIFICATION_EVENT_FLAGSCLEAR_H
#define PUSH_NOTIFICATION_EVENT_FLAGSCLEAR_H
#include "mail-types.h"
struct push_notification_event_flagsclear_config {
bool store_old;
};
struct push_notification_event_flagsclear_data {
enum mail_flags flags_clear;
ARRAY_TYPE(keywords) keywords_clear;
enum mail_flags flags_old;
ARRAY_TYPE(keywords) keywords_old;
};
#endif