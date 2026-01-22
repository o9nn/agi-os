#ifndef _RTC_H_
#define _RTC_H_
#define RTC_ADDR	0x70
#define RTC_DATA	0x71
#define RTC_A		0x0a
#define RTC_UIP		0x80
#define RTC_DIV0	0x00
#define RTC_DIV1	0x10
#define RTC_DIV2	0x20
#define RTC_RATE6	0x06
#define RTC_B		0x0b
#define RTC_SET		0x80
#define RTC_PIE		0x40
#define RTC_AIE		0x20
#define RTC_UIE		0x10
#define RTC_SQWE	0x08
#define RTC_DM		0x04
#define RTC_HM		0x02
#define RTC_DSE		0x01
#define RTC_C		0x0c
#define RTC_IRQF	0x80
#define RTC_PF		0x40
#define RTC_AF		0x20
#define RTC_UF		0x10
#define RTC_D		0x0d
#define RTC_VRT		0x80
#define RTC_NREG	0x0e
#define RTC_NREGP	0x0a
#define RTCRTIME	_IOR('c', 0x01, struct rtc_st)
#define RTCSTIME	_IOW('c', 0x02, struct rtc_st)
struct rtc_st {
char	rtc_sec;
char	rtc_asec;
char	rtc_min;
char	rtc_amin;
char	rtc_hr;
char	rtc_ahr;
char	rtc_dow;
char	rtc_dom;
char	rtc_mon;
char	rtc_yr;
char	rtc_statusa;
char	rtc_statusb;
char	rtc_statusc;
char	rtc_statusd;
};
#define load_rtc(regs) \
MACRO_BEGIN \
int i; \
\
for (i = 0; i < RTC_NREG; i++) { \
outb(RTC_ADDR, i); \
regs[i] = inb(RTC_DATA); \
} \
MACRO_END
#define save_rtc(regs) \
MACRO_BEGIN \
int i; \
for (i = 0; i < RTC_NREGP; i++) { \
outb(RTC_ADDR, i); \
outb(RTC_DATA, regs[i]);\
} \
MACRO_END
extern int readtodc(uint64_t *tp);
extern int writetodc(void);
#endif