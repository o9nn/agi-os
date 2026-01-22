#ifndef _MACH_THREAD_STATUS_H_
#define _MACH_THREAD_STATUS_H_
#include <mach/machine/vm_types.h>
#include <mach/machine/thread_status.h>
typedef natural_t *thread_state_t;
#define THREAD_STATE_MAX (1024)
typedef natural_t thread_state_data_t[THREAD_STATE_MAX];
#define THREAD_STATE_FLAVOR_LIST 0
#endif