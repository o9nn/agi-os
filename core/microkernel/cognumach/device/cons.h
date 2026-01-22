#ifndef _DEVICE_CONS_H
#define _DEVICE_CONS_H
#include <sys/types.h>
struct consdev {
char *cn_name;
int (*cn_probe)(struct consdev *cp);
int (*cn_init)(struct consdev *cp);
int (*cn_getc)(dev_t dev, int wait);
int (*cn_putc)(dev_t dev, int c);
dev_t cn_dev;
short cn_pri;
};
#define CN_DEAD 0
#define CN_NORMAL 1
#define CN_INTERNAL 2
#define CN_REMOTE 3
#define CONSBUFSIZE 1024
#ifdef KERNEL
extern struct consdev constab[];
#endif
extern void cninit(void);
extern int cngetc(void);
extern int cnmaygetc(void);
extern void cnputc(char);
extern int (*romgetc)(char c);
extern void (*romputc)(char c);
#endif