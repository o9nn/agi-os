#ifndef _MC146818RTC_H
#define _MC146818RTC_H
#include <asm/io.h>
#ifndef RTC_PORT
#define RTC_PORT(x)	(0x70 + (x))
#define RTC_ALWAYS_BCD	1
#endif
#define CMOS_READ(addr) ({ \
outb_p((addr),RTC_PORT(0)); \
inb_p(RTC_PORT(1)); \
})
#define CMOS_WRITE(val, addr) ({ \
outb_p((addr),RTC_PORT(0)); \
outb_p((val),RTC_PORT(1)); \
})
#define RTC_SECONDS		0
#define RTC_SECONDS_ALARM	1
#define RTC_MINUTES		2
#define RTC_MINUTES_ALARM	3
#define RTC_HOURS		4
#define RTC_HOURS_ALARM		5
# define RTC_ALARM_DONT_CARE 	0xC0
#define RTC_DAY_OF_WEEK		6
#define RTC_DAY_OF_MONTH	7
#define RTC_MONTH		8
#define RTC_YEAR		9
#define RTC_REG_A		10
#define RTC_REG_B		11
#define RTC_REG_C		12
#define RTC_REG_D		13
#define RTC_FREQ_SELECT	RTC_REG_A
# define RTC_UIP		0x80
# define RTC_DIV_CTL		0x70
#  define RTC_REF_CLCK_4MHZ	0x00
#  define RTC_REF_CLCK_1MHZ	0x10
#  define RTC_REF_CLCK_32KHZ	0x20
#  define RTC_DIV_RESET1	0x60
#  define RTC_DIV_RESET2	0x70
# define RTC_RATE_SELECT 	0x0F
#define RTC_CONTROL	RTC_REG_B
# define RTC_SET 0x80
# define RTC_PIE 0x40
# define RTC_AIE 0x20
# define RTC_UIE 0x10
# define RTC_SQWE 0x08
# define RTC_DM_BINARY 0x04
# define RTC_24H 0x02
# define RTC_DST_EN 0x01
#define RTC_INTR_FLAGS	RTC_REG_C
# define RTC_IRQF 0x80
# define RTC_PF 0x40
# define RTC_AF 0x20
# define RTC_UF 0x10
#define RTC_VALID	RTC_REG_D
# define RTC_VRT 0x80
#ifndef BCD_TO_BIN
#define BCD_TO_BIN(val) ((val)=((val)&15) + ((val)>>4)*10)
#endif
#ifndef BIN_TO_BCD
#define BIN_TO_BCD(val) ((val)=(((val)/10)<<4) + (val)%10)
#endif
struct rtc_time {
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
#define RTC_AIE_ON	_IO('p', 0x01)
#define RTC_AIE_OFF	_IO('p', 0x02)
#define RTC_UIE_ON	_IO('p', 0x03)
#define RTC_UIE_OFF	_IO('p', 0x04)
#define RTC_PIE_ON	_IO('p', 0x05)
#define RTC_PIE_OFF	_IO('p', 0x06)
#define RTC_ALM_SET	_IOW('p', 0x07, struct rtc_time)
#define RTC_ALM_READ	_IOR('p', 0x08, struct rtc_time)
#define RTC_RD_TIME	_IOR('p', 0x09, struct rtc_time)
#define RTC_SET_TIME	_IOW('p', 0x0a, struct rtc_time)
#define RTC_IRQP_READ	_IOR('p', 0x0b, unsigned long)
#define RTC_IRQP_SET	_IOW('p', 0x0c, unsigned long)
#define RTC_EPOCH_READ	_IOR('p', 0x0d, unsigned long)
#define RTC_EPOCH_SET	_IOW('p', 0x0e, unsigned long)
#endif