#ifndef _OPENCOG_PLATFORM_H
#define _OPENCOG_PLATFORM_H
#ifdef WIN32
#pragma warning(disable:4290)
#define strcasecmp _stricmp
#define snprintf _snprintf
#endif
#include <stdio.h>
#include <string.h>
#include <string>
#include <stdint.h>
#ifdef WIN32_NOT_UNIX
#define M_PI 3.14159265358979323846
struct timezone {};
int                round(float x);
int                gettimeofday(struct timeval* tp, void* tzp);
void               usleep(unsigned useconds);
unsigned long long atoll(const char *str);
unsigned int       sleep(unsigned seconds);
#endif
namespace opencog
{
size_t getMemUsage();
uint64_t getTotalRAM();
uint64_t getFreeRAM();
void set_thread_name(const char* name);
}
#endif