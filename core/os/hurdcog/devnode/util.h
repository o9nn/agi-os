#ifndef UTIL_H
#define UTIL_H
#include <stdio.h>
#ifdef DEBUG
#define devnode_debug(format, ...) do \
{ \
fprintf (stderr , "devnode: " format, ## __VA_ARGS__);\
fflush (stderr); \
} while (0)
#else
#define devnode_debug(format, ...) do {} while (0)
#endif
#endif