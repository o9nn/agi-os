#ifndef _LINUX_KDEV_T_H
#define _LINUX_KDEV_T_H
#ifdef __KERNEL__
#define MINORBITS 8
#define MINORMASK ((1U << MINORBITS) - 1)
typedef unsigned short kdev_t;
#define MAJOR(dev) ((unsigned int) ((dev) >> MINORBITS))
#define MINOR(dev) ((unsigned int) ((dev) & MINORMASK))
#define HASHDEV(dev) ((unsigned int) (dev))
#define NODEV 0
#define MKDEV(ma,mi) (((ma) << MINORBITS) | (mi))
#define B_FREE 0xffff
extern char * kdevname(kdev_t);
static inline unsigned int kdev_t_to_nr(kdev_t dev) {
return (MAJOR(dev)<<8) | MINOR(dev);
}
static inline kdev_t to_kdev_t(int dev)
{
int major, minor;
#if 0
major = (dev >> 16);
if (!major) {
major = (dev >> 8);
minor = (dev & 0xff);
} else
minor = (dev & 0xffff);
#else
major = (dev >> 8);
minor = (dev & 0xff);
#endif
return MKDEV(major, minor);
}
#else
#define MAJOR(dev) ((dev)>>8)
#define MINOR(dev) ((dev) & 0xff)
#define MKDEV(ma,mi) ((ma)<<8 | (mi))
#endif
#endif