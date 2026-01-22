#ifndef _LINUX_PORTIO_H
#define _LINUX_PORTIO_H
#define HAVE_PORTRESERVE
extern void reserve_setup(char *str, int *ints);
extern int check_region(unsigned long from, unsigned long extent);
extern void request_region(unsigned long from, unsigned long extent,const char *name);
extern void release_region(unsigned long from, unsigned long extent);
extern int get_ioport_list(char *);
#ifdef __sparc__
extern unsigned long occupy_region(unsigned long base, unsigned long end,
unsigned long num, unsigned int align,
const char *name);
#endif
#define HAVE_AUTOIRQ
extern void autoirq_setup(int waittime);
extern int autoirq_report(int waittime);
#endif