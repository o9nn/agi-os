#ifndef NEWSFEED_H
#define NEWSFEED_H
#include <libetpan/newsfeed_types.h>
#include <libetpan/newsfeed_item.h>
#include <libetpan/newsfeed_item_enclosure.h>
struct newsfeed * newsfeed_new(void);
void newsfeed_free(struct newsfeed * feed);
int newsfeed_get_response_code(struct newsfeed * feed);
int newsfeed_set_url(struct newsfeed * feed, const char * url);
const char * newsfeed_get_url(struct newsfeed * feed);
int newsfeed_set_title(struct newsfeed * feed, const char * title);
const char * newsfeed_get_title(struct newsfeed * feed);
int newsfeed_set_description(struct newsfeed * feed, const char * description);
const char * newsfeed_get_description(struct newsfeed * feed);
int newsfeed_set_language(struct newsfeed * feed, const char * language);
const char * newsfeed_get_language(struct newsfeed * feed);
int newsfeed_set_author(struct newsfeed * feed, const char * author);
const char * newsfeed_get_author(struct newsfeed * feed);
int newsfeed_set_generator(struct newsfeed * feed, const char * generator);
const char * newsfeed_get_generator(struct newsfeed * feed);
unsigned int newsfeed_item_list_get_count(struct newsfeed * feed);
struct newsfeed_item * newsfeed_get_item(struct newsfeed * feed, unsigned int n);
void newsfeed_set_date(struct newsfeed * feed, time_t date);
time_t newsfeed_get_date(struct newsfeed * feed);
void newsfeed_set_timeout(struct newsfeed * feed, unsigned int timeout);
unsigned int newsfeed_get_timeout(struct newsfeed * feed);
int newsfeed_add_item(struct newsfeed * feed, struct newsfeed_item * item);
int newsfeed_update(struct newsfeed * feed, time_t last_update);
#endif