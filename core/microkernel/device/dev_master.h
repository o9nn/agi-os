#ifndef _DEVICE_DEV_MASTER_H_
#define _DEVICE_DEV_MASTER_H_
#include <cpus.h>
#if	NCPUS > 1
#include <kern/macros.h>
#include <kern/cpu_number.h>
#include <kern/sched_prim.h>
#include <kern/thread.h>
#include <kern/processor.h>
#define	io_grab_master() \
MACRO_BEGIN \
thread_bind(current_thread(), master_processor); \
if (current_processor() != master_processor) \
thread_block((void (*)()) 0); \
MACRO_END
#define	io_release_master() \
MACRO_BEGIN \
thread_bind(current_thread(), PROCESSOR_NULL); \
MACRO_END
#else	NCPUS > 1
#define	io_grab_master()
#define	io_release_master()
#endif	NCPUS > 1
#endif