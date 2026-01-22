#ifndef HEADER_O_TIME_H
# define HEADER_O_TIME_H
# include <time.h>
struct tm *OPENSSL_gmtime(const time_t *timer, struct tm *result);
int OPENSSL_gmtime_adj(struct tm *tm, int offset_day, long offset_sec);
#endif