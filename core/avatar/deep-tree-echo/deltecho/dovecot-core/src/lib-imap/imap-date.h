#ifndef IMAP_DATE_H
#define IMAP_DATE_H
bool imap_parse_date(const char *str, time_t *timestamp_r);
bool imap_parse_datetime(const char *str, time_t *timestamp_r,
int *timezone_offset_r);
const char *imap_to_datetime(time_t timestamp);
const char *imap_to_datetime_tz(time_t timestamp, int timezone_offset);
bool imap_to_date(time_t timestamp, const char **str_r);
#endif