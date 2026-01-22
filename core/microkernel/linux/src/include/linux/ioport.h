#ifndef _LINUX_PORTIO_H
#define _LINUX_PORTIO_H
#define HAVE_PORTRESERVE
extern void reserve_setup(char *str, int *ints);
extern int check_region(unsigned int from, unsigned int extent);
extern void request_region(unsigned int from, unsigned int extent,const char *name);
extern void release_region(unsigned int from, unsigned int extent);
extern int get_ioport_list(char *);
#define HAVE_AUTOIRQ
extern void *irq2dev_map[];
extern int autoirq_setup(int waittime);
extern int autoirq_report(int waittime);
#endif