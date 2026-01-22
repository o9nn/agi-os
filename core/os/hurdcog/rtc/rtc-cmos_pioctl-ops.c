#include "rtc_pioctl_S.h"
#include <hurd/rtc.h>
#include <hurd/hurd_types.h>
#include <sys/io.h>
#include <stdbool.h>
#define BCD_TO_BIN(val) ((val)=((val)&15) + (((val)>>4)&15)*10 + \
((val)>>8)*100)
#define BIN_TO_BCD(val) ((val)=(((val)/100)<<8) + \
((((val)/10)%10)<<4) + (val)%10)
#define TM_EPOCH 1900
#define CLOCK_CTL_ADDR 0x70
#define CLOCK_DATA_ADDR 0x71
#define is_leap(year) \
((year) % 4 == 0 && ((year) % 100 != 0 || (year) % 400 == 0))
static const int mon_yday[2][13] =
{
{ 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365 },
{ 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335, 366 }
};
static inline unsigned char
cmos_read (unsigned char reg)
{
outb_p (reg, CLOCK_CTL_ADDR);
return inb_p (CLOCK_DATA_ADDR);
}
static inline void
cmos_write (unsigned char reg, unsigned char val)
{
outb_p (reg, CLOCK_CTL_ADDR);
outb_p (val, CLOCK_DATA_ADDR);
}
static inline int
cmos_clock_busy (void)
{
return (cmos_read (10) & 0x80);
}
static int
calculate_yday (const struct rtc_time *tm)
{
return mon_yday[is_leap (tm->tm_year)][tm->tm_mon] + tm->tm_mday - 1;
}
kern_return_t
rtc_S_pioctl_rtc_uie_on (struct trivfs_protid *cred)
{
return EOPNOTSUPP;
}
kern_return_t
rtc_S_pioctl_rtc_uie_off (struct trivfs_protid *cred)
{
return EOPNOTSUPP;
}
kern_return_t
rtc_S_pioctl_rtc_rd_time (struct trivfs_protid *cred, struct rtc_time *tm)
{
unsigned char status = 0;
unsigned char pmbit = 0;
int time_passed_in_milliseconds = 0;
bool read_rtc_successfully = false;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openmodes & O_READ))
return EBADF;
while (time_passed_in_milliseconds < 100)
{
if (!cmos_clock_busy ())
{
tm->tm_sec = cmos_read (0);
tm->tm_min = cmos_read (2);
tm->tm_hour = cmos_read (4);
tm->tm_wday = cmos_read (6);
tm->tm_mday = cmos_read (7);
tm->tm_mon = cmos_read (8);
tm->tm_year = cmos_read (9);
status = cmos_read (11);
if (tm->tm_sec == cmos_read (0))
{
read_rtc_successfully = true;
break;
}
}
usleep (1000);
time_passed_in_milliseconds++;
}
if (!read_rtc_successfully)
return EBUSY;
if (!(status & 0x04))
{
BCD_TO_BIN (tm->tm_sec);
BCD_TO_BIN (tm->tm_min);
pmbit = (tm->tm_hour & 0x80);
tm->tm_hour &= 0x7f;
BCD_TO_BIN (tm->tm_hour);
BCD_TO_BIN (tm->tm_wday);
BCD_TO_BIN (tm->tm_mday);
BCD_TO_BIN (tm->tm_mon);
BCD_TO_BIN (tm->tm_year);
}
tm->tm_wday -= 1;
tm->tm_mon -= 1;
if (tm->tm_year < 69)
tm->tm_year += 100;
tm->tm_yday = calculate_yday (tm);
if (pmbit)
{
tm->tm_hour += 12;
if (tm->tm_hour == 24)
tm->tm_hour = 0;
}
tm->tm_isdst = -1;
return KERN_SUCCESS;
}
kern_return_t
rtc_S_pioctl_rtc_set_time (struct trivfs_protid *cred, struct rtc_time tm)
{
unsigned char save_control, save_freq_select, pmbit = 0;
if (!cred)
return EOPNOTSUPP;
if (!(cred->po->openmodes & O_WRITE))
return EBADF;
save_control = cmos_read (11);
cmos_write (11, (save_control | 0x80));
save_freq_select = cmos_read (10);
cmos_write (10, (save_freq_select | 0x70));
tm.tm_year %= 100;
tm.tm_mon += 1;
tm.tm_wday += 1;
if (!(save_control & 0x02))
{
if (tm.tm_hour == 0)
tm.tm_hour = 24;
if (tm.tm_hour > 12)
{
tm.tm_hour -= 12;
pmbit = 0x80;
}
}
if (!(save_control & 0x04))
{
BIN_TO_BCD (tm.tm_sec);
BIN_TO_BCD (tm.tm_min);
BIN_TO_BCD (tm.tm_hour);
BIN_TO_BCD (tm.tm_wday);
BIN_TO_BCD (tm.tm_mday);
BIN_TO_BCD (tm.tm_mon);
BIN_TO_BCD (tm.tm_year);
}
cmos_write (0, tm.tm_sec);
cmos_write (2, tm.tm_min);
cmos_write (4, tm.tm_hour | pmbit);
cmos_write (6, tm.tm_wday);
cmos_write (7, tm.tm_mday);
cmos_write (8, tm.tm_mon);
cmos_write (9, tm.tm_year);
cmos_write (11, save_control);
cmos_write (10, save_freq_select);
return KERN_SUCCESS;
}