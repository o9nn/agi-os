#ifndef __TIMEFMT_H__
#define __TIMEFMT_H__
struct timeval;
size_t fmt_named_interval (struct timeval *tv, size_t width,
char *buf, size_t buf_len);
size_t fmt_seconds (struct timeval *tv, int leading_zeros, int frac_places,
size_t width, char *buf, size_t buf_len);
size_t fmt_minutes (struct timeval *tv, int leading_zeros,
size_t width, char *buf, size_t buf_len);
size_t fmt_past_time (struct timeval *tv, struct timeval *now,
size_t width, char *buf, size_t buf_len);
#endif