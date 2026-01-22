#ifndef _RTC_H
#define _RTC_H 1
#include <hurd/ioctl.h>
struct rtc_time
{
int tm_sec;
int tm_min;
int tm_hour;
int tm_mday;
int tm_mon;
int tm_year;
int tm_wday;
int tm_yday;
int tm_isdst;
};
typedef struct rtc_time rtc_time_t;
#define _IOT_rtc_time _IOT(_IOTS(int),9,0,0,0,0)
#define RTC_UIE_ON _IO('p', 0x03)
#define RTC_UIE_OFF _IO('p', 0x04)
#define RTC_RD_TIME _IOR('p', 0x09, struct rtc_time)
#define RTC_SET_TIME _IOW('p', 0x0a, struct rtc_time)
#endif