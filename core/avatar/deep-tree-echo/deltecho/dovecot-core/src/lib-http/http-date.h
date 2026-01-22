#ifndef HTTP_DATE_H
#define HTTP_DATE_H
bool http_date_parse(const unsigned char *data, size_t size,
time_t *timestamp_r);
bool http_date_parse_tm(const unsigned char *data, size_t size,
struct tm *tm_r);
const char *http_date_create_tm(struct tm *tm);
const char *http_date_create(time_t timestamp);
#endif