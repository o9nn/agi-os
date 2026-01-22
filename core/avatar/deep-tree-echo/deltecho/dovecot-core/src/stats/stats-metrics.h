#ifndef STATS_METRICS_H
#define STATS_METRICS_H
#include "stats-settings.h"
#include "sha1.h"
#define STATS_EVENT_FIELD_NAME_DURATION "duration"
struct metric;
struct stats_metrics;
struct exporter {
const char *name;
enum event_exporter_time_fmt time_format;
size_t format_max_field_len;
void (*format)(const struct metric *, struct event *, buffer_t *);
const char *format_mime_type;
const char *transport_args;
unsigned int transport_timeout;
void *transport_context;
void (*transport)(const struct exporter *, const buffer_t *);
};
struct metric_export_info {
const struct exporter *exporter;
enum event_exporter_includes {
EVENT_EXPORTER_INCL_NONE       = 0,
EVENT_EXPORTER_INCL_NAME       = 0x01,
EVENT_EXPORTER_INCL_HOSTNAME   = 0x02,
EVENT_EXPORTER_INCL_TIMESTAMPS = 0x04,
EVENT_EXPORTER_INCL_CATEGORIES = 0x08,
EVENT_EXPORTER_INCL_FIELDS     = 0x10,
} include;
};
struct metric_field {
const char *field_key;
struct stats_dist *stats;
};
enum metric_value_type {
METRIC_VALUE_TYPE_STR,
METRIC_VALUE_TYPE_INT,
METRIC_VALUE_TYPE_IP,
METRIC_VALUE_TYPE_BUCKET_INDEX,
};
struct metric_value {
enum metric_value_type type;
unsigned char hash[SHA1_RESULTLEN];
intmax_t intmax;
struct ip_addr ip;
};
struct metric {
const struct stats_metric_settings *set;
const char *name;
const char *sub_name;
size_t sub_name_used_size;
struct stats_dist *duration_stats;
unsigned int fields_count;
struct metric_field *fields;
unsigned int group_by_count;
const struct stats_metric_settings_group_by *group_by;
struct metric_value group_value;
ARRAY(struct metric *) sub_metrics;
struct metric_export_info export_info;
};
bool stats_metrics_add_dynamic(struct stats_metrics *metrics,
struct stats_metric_settings *set,
const char **error_r);
bool stats_metrics_remove_dynamic(struct stats_metrics *metrics,
const char *name);
int stats_metrics_init(struct event *event,
const struct stats_settings *set,
struct stats_metrics **metrics_r, const char **error_r);
void stats_metrics_deinit(struct stats_metrics **metrics);
void stats_metrics_reset(struct stats_metrics *metrics);
struct event_filter *
stats_metrics_get_event_filter(struct stats_metrics *metrics);
void stats_metrics_event(struct stats_metrics *metrics, struct event *event,
const struct failure_context *ctx);
struct stats_metrics_iter *
stats_metrics_iterate_init(struct stats_metrics *metrics);
const struct metric *stats_metrics_iterate(struct stats_metrics_iter *iter);
void stats_metrics_iterate_deinit(struct stats_metrics_iter **iter);
#endif