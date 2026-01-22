#ifndef _RESOURCES_H
#define _RESOURCES_H
#include "api-types.h"
#include "link-includes.h"
void      print_time(Parse_Options opts, const char * s, ...) GNUC_PRINTF(2,3);
void      print_total_space(Parse_Options opts);
void      resources_reset(Resources r);
void      resources_reset_space(Resources r);
bool      resources_timer_expired(Resources r);
bool      resources_memory_exhausted(Resources r);
bool      resources_exhausted(Resources r);
Resources resources_create(void);
void      resources_delete(Resources ti);
#endif