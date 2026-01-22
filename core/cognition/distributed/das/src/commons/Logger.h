#pragma once
#include <string.h>
#include <time.h>
#include <iostream>
static inline char* timenow();
#define NO_LOG 0x00
#define ERROR_LEVEL 0x01
#define INFO_LEVEL 0x02
#define DEBUG_LEVEL 0x03
#define LOCAL_DEBUG_LEVEL 0x04
#ifndef LOG_LEVEL
#define LOG_LEVEL INFO_LEVEL
#endif
#if LOG_LEVEL >= LOCAL_DEBUG_LEVEL
#define LOG_LOCAL_DEBUG(msg) \
(std::cout << timenow() << " | " \
<< "[LOCAL_DEBUG] | " << __FILE__ << " | " << __FUNCTION__ << " : " << __LINE__ << " | " \
<< msg << std::endl)
#else
#define LOG_LOCAL_DEBUG(msg)
#endif
#if LOG_LEVEL >= DEBUG_LEVEL
#define LOG_DEBUG(msg) \
(std::cout << timenow() << " | " \
<< "[DEBUG] | " << __FILE__ << " | " << __FUNCTION__ << " : " << __LINE__ << " | " \
<< msg << std::endl)
#else
#define LOG_DEBUG(msg)
#endif
#if LOG_LEVEL >= INFO_LEVEL
#define LOG_INFO(msg) \
(std::cout << timenow() << " | " \
<< "[INFO] | " << msg << std::endl)
#else
#define LOG_INFO(msg)
#endif
#if LOG_LEVEL >= ERROR_LEVEL
#define LOG_ERROR(msg) \
(std::cerr << timenow() << " | " \
<< "[ERROR] | " << __FILE__ << " | " << __FUNCTION__ << " : " << __LINE__ << " | " \
<< msg << std::endl)
#else
#define LOG_ERROR(msg)
#endif
static inline char* timenow() {
static char buffer[64];
time_t rawtime;
struct tm* timeinfo;
time(&rawtime);
timeinfo = localtime(&rawtime);
strftime(buffer, 64, "%Y-%m-%d %H:%M:%S", timeinfo);
return buffer;
}
#include "Profiler.h"