#ifndef _MY_GETTIMEOFDAY_H_
#define _MY_GETTIMEOFDAY_H_
#ifdef _MSC_VER
#include <winsock2.h>
#include <time.h>
int gettimeofday(struct timeval * tp, struct timezone * tzp);
#endif
#endif