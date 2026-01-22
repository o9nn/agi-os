#ifndef STATS_SETTINGS_H
#define STATS_SETTINGS_H
#define STATS_METRIC_SETTINGS_DEFAULT_EXPORTER_INCLUDE \
"name hostname timestamps categories fields"
enum event_exporter_time_fmt {
EVENT_EXPORTER_TIME_FMT_NATIVE = 0,
EVENT_EXPORTER_TIME_FMT_UNIX,
EVENT_EXPORTER_TIME_FMT_RFC3339,
};
struct stats_exporter_settings {
pool_t pool;
const char *name;
const char *transport;
const char *transport_args;
unsigned int transport_timeout;
const char *format;
const char *format_args;
enum event_exporter_time_fmt parsed_time_format;
};
enum stats_metric_group_by_func {
STATS_METRIC_GROUPBY_DISCRETE = 0,
STATS_METRIC_GROUPBY_QUANTIZED,
};
enum stats_metric_group_by_modifier {
STATS_METRICS_GROUPBY_DOMAIN     = BIT(0),
STATS_METRICS_GROUPBY_UPPERCASE  = BIT(1),
STATS_METRICS_GROUPBY_LOWERCASE  = BIT(2),
};
struct stats_metric_settings_bucket_range {
intmax_t min;
intmax_t max;
};
struct stats_metric_settings_group_by {
const char *field;
enum stats_metric_group_by_func func;
enum stats_metric_group_by_modifier mod;
unsigned int num_ranges;
struct stats_metric_settings_bucket_range *ranges;
};
struct stats_metric_settings {
pool_t pool;
const char *name;
const char *description;
const char *fields;
const char *group_by;
const char *filter;
ARRAY(struct stats_metric_settings_group_by) parsed_group_by;
struct event_filter *parsed_filter;
const char *exporter;
const char *exporter_include;
};
struct stats_settings {
pool_t pool;
const char *stats_http_rawlog_dir;
ARRAY_TYPE(const_string) exporters;
ARRAY_TYPE(const_string) metrics;
};
extern const struct setting_parser_info stats_setting_parser_info;
extern const struct setting_parser_info stats_metric_setting_parser_info;
extern const struct setting_parser_info stats_exporter_setting_parser_info;
#endif