#ifndef EVENT_EXPORTER_H
#define EVENT_EXPORTER_H
#include "stats-metrics.h"
void event_export_fmt_json(const struct metric *metric, struct event *event, buffer_t *dest);
void event_export_fmt_none(const struct metric *metric, struct event *event, buffer_t *dest);
void event_export_fmt_tabescaped_text(const struct metric *metric, struct event *event, buffer_t *dest);
void event_export_transport_drop(const struct exporter *exporter, const buffer_t *buf);
void event_export_transport_http_post(const struct exporter *exporter, const buffer_t *buf);
void event_export_transport_http_post_deinit(void);
void event_export_transport_log(const struct exporter *exporter, const buffer_t *buf);
void event_export_transport_file(const struct exporter *exporter, const buffer_t *buf);
void event_export_transport_unix(const struct exporter *exporter, const buffer_t *buf);
void event_export_transport_file_reopen(void);
void event_export_transport_file_deinit(void);
void event_export_helper_fmt_rfc3339_time(string_t *dest, const struct timeval *time);
void event_export_helper_fmt_unix_time(string_t *dest, const struct timeval *time);
void event_export_helper_fmt_categories(string_t *dest,
const struct event *event,
void (*append)(string_t *, const char *),
const char *separator);
void event_export_transport_assign_context(const struct exporter *exporter,
void *context);
#endif