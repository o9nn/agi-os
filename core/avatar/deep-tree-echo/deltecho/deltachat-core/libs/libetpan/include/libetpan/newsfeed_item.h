#ifndef NEWSFEED_ITEM_H
#define NEWSFEED_ITEM_H
#include <libetpan/newsfeed_types.h>
struct newsfeed_item * newsfeed_item_new(struct newsfeed * feed);
void newsfeed_item_free(struct newsfeed_item * item);
struct newsfeed * newsfeed_item_get_feed(struct newsfeed_item * item);
const char * newsfeed_item_get_url(struct newsfeed_item * item);
int newsfeed_item_set_url(struct newsfeed_item * item, const char * url);
const char * newsfeed_item_get_title(struct newsfeed_item * item);
int newsfeed_item_set_title(struct newsfeed_item * item, const char * title);
const char * newsfeed_item_get_summary(struct newsfeed_item * item);
int newsfeed_item_set_summary(struct newsfeed_item * item, const char * summary);
const char * newsfeed_item_get_text(struct newsfeed_item * item);
int newsfeed_item_set_text(struct newsfeed_item * item, const char * text);
const char * newsfeed_item_get_author(struct newsfeed_item * item);
int newsfeed_item_set_author(struct newsfeed_item * item, const char * author);
const char * newsfeed_item_get_id(struct newsfeed_item * item);
int newsfeed_item_set_id(struct newsfeed_item * item, const char * id);
time_t newsfeed_item_get_date_published(struct newsfeed_item * item);
void newsfeed_item_set_date_published(struct newsfeed_item * item, time_t date);
time_t newsfeed_item_get_date_modified(struct newsfeed_item * item);
void newsfeed_item_set_date_modified(struct newsfeed_item * item, time_t date);
struct newsfeed_item_enclosure * newsfeed_item_get_enclosure(struct newsfeed_item * item);
void newsfeed_item_set_enclosure(struct newsfeed_item * item,
struct newsfeed_item_enclosure * enclosure);
#endif