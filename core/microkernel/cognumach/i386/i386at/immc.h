#ifndef	_IMMC_H_
#define	_IMMC_H_
#include <sys/types.h>
int immc_cnprobe(struct consdev *cp);
int immc_cninit(struct consdev *cp);
int immc_cngetc(dev_t dev, int wait);
int immc_cnputc(dev_t dev, int c);
void immc_romputc(char c);
#endif