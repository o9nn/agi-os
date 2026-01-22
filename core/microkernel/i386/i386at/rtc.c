#include <sys/types.h>
#include <kern/mach_clock.h>
#include <kern/printf.h>
#include <i386/spl.h>
#include <i386/pio.h>
#include <i386at/rtc.h>
#define CENTURY_START	1970
static boolean_t first_rtcopen_ever = TRUE;
static void
rtcinit(void)
{
outb(RTC_ADDR, RTC_A);
outb(RTC_DATA, RTC_DIV2 | RTC_RATE6);
outb(RTC_ADDR, RTC_B);
outb(RTC_DATA, RTC_HM);
}
static int
rtcget(struct rtc_st *st)
{
unsigned char *regs = (unsigned char *)st;
if (first_rtcopen_ever) {
rtcinit();
first_rtcopen_ever = FALSE;
}
outb(RTC_ADDR, RTC_D);
if ((inb(RTC_DATA) & RTC_VRT) == 0) return(-1);
outb(RTC_ADDR, RTC_A);
while (inb(RTC_DATA) & RTC_UIP)
outb(RTC_ADDR, RTC_A);
load_rtc(regs);
return(0);
}
static void
rtcput(struct rtc_st *st)
{
unsigned char *regs = (unsigned char *)st;
unsigned char	x;
if (first_rtcopen_ever) {
rtcinit();
first_rtcopen_ever = FALSE;
}
outb(RTC_ADDR, RTC_B);
x = inb(RTC_DATA);
outb(RTC_ADDR, RTC_B);
outb(RTC_DATA, x | RTC_SET);
save_rtc(regs);
outb(RTC_ADDR, RTC_B);
outb(RTC_DATA, x & ~RTC_SET);
}
static int month[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
static int
yeartoday(int year)
{
if (year%4)
return 365;
if (year % 100)
return 366;
if (year % 400)
return 365;
return 366;
}
static int
hexdectodec(char n)
{
return(((n>>4)&0x0F)*10 + (n&0x0F));
}
static char
dectohexdec(int n)
{
return((char)(((n/10)<<4)&0xF0) | ((n%10)&0x0F));
}
int
readtodc(uint64_t *tp)
{
struct rtc_st rtclk;
time_t n;
int sec, min, hr, dom, mon, yr;
int i, days = 0;
spl_t	ospl;
ospl = splclock();
if (rtcget(&rtclk)) {
splx(ospl);
return(-1);
}
splx (ospl);
sec = hexdectodec(rtclk.rtc_sec);
min = hexdectodec(rtclk.rtc_min);
hr = hexdectodec(rtclk.rtc_hr);
dom = hexdectodec(rtclk.rtc_dom);
mon = hexdectodec(rtclk.rtc_mon);
yr = hexdectodec(rtclk.rtc_yr);
yr = (yr < CENTURY_START%100) ?
yr+CENTURY_START-CENTURY_START%100+100 :
yr+CENTURY_START-CENTURY_START%100;
if (yr >= CENTURY_START+90) {
printf("FIXME: we are approaching %u, update CENTURY_START\n", CENTURY_START);
}
printf("RTC time is %04u-%02u-%02u %02u:%02u:%02u\n", yr, mon, dom, hr, min, sec);
n = sec + 60 * min + 3600 * hr;
n += (dom - 1) * 3600 * 24;
if (yeartoday(yr) == 366)
month[1] = 29;
for (i = mon - 2; i >= 0; i--)
days += month[i];
month[1] = 28;
for (i = 1970; i < yr; i++)
days += yeartoday(i);
n += days * 3600 * 24;
*tp = n;
return(0);
}
int
writetodc(void)
{
struct rtc_st rtclk;
time_t n;
int diff, i, j;
spl_t	ospl;
ospl = splclock();
if (rtcget(&rtclk)) {
splx(ospl);
return(-1);
}
splx(ospl);
diff = 0;
n = (time.seconds - diff) % (3600 * 24);
rtclk.rtc_sec = dectohexdec(n%60);
n /= 60;
rtclk.rtc_min = dectohexdec(n%60);
rtclk.rtc_hr = dectohexdec(n/60);
n = (time.seconds - diff) / (3600 * 24);
rtclk.rtc_dow = (n + 4) % 7;
for (j = 1970, i = yeartoday(j); n >= i; j++, i = yeartoday(j))
n -= i;
rtclk.rtc_yr = dectohexdec(j % 100);
if (i == 366)
month[1] = 29;
for (i = 0; n >= month[i]; i++)
n -= month[i];
month[1] = 28;
rtclk.rtc_mon = dectohexdec(++i);
rtclk.rtc_dom = dectohexdec(++n);
ospl = splclock();
rtcput(&rtclk);
splx(ospl);
return(0);
}