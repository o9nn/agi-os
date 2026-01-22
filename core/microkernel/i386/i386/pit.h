#ifndef _I386_PIT_H_
#define _I386_PIT_H_
#if defined(AT386) || defined(ATX86_64)
#define PITCTR0_PORT 0x40
#define PITCTR1_PORT 0x41
#define PITCTR2_PORT 0x42
#define PITCTL_PORT 0x43
#define PITAUX_PORT 0x61
#define PITAUX_GATE2 0x01
#define PITAUX_OUT2 0x02
#define PITAUX_VAL 0x20
#endif
#define PIT_C0 0x00
#define PIT_LOADMODE 0x30
#define PIT_NDIVMODE 0x04
#define PIT_C1 0x40
#define PIT_READMODE 0x30
#define PIT_SQUAREMODE 0x06
#define PIT_RATEMODE 0x04
#define PIT_ONESHOTMODE 0x02
#define PIT_C2 0x80
#define POST_PORT 0x80
#if defined(AT386) || defined(ATX86_64)
#define CLKNUM 1193182
#endif
extern void clkstart(void);
extern void pit_prepare_sleep(int hz);
extern void pit_sleep(void);
extern void pit_udelay(int usec);
extern void pit_mdelay(int msec);
#endif