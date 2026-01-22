#ifndef CONFIG_FILTER_H
#define CONFIG_FILTER_H
#include "net.h"
struct config_filter {
struct config_filter *parent;
const char *service;
const char *local_name;
const char *local_host, *remote_host;
struct ip_addr local_net, remote_net;
unsigned int local_bits, remote_bits;
const char *filter_name;
bool filter_name_array;
};
struct config_filter_parser {
struct config_filter_parser *children_head, *children_tail, *prev, *next;
struct config_filter filter;
const char *file_and_line;
struct config_module_parser *module_parsers;
bool filter_required_setting_seen;
};
bool config_filter_match(const struct config_filter *mask,
const struct config_filter *filter);
bool config_filters_equal(const struct config_filter *f1,
const struct config_filter *f2);
#endif