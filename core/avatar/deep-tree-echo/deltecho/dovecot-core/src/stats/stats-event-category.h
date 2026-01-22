#ifndef STATS_EVENT_CATEGORY_H
#define STATS_EVENT_CATEGORY_H
void stats_event_category_register(const char *name,
struct event_category *parent);
void stats_event_categories_init(void);
void stats_event_categories_deinit(void);
#endif