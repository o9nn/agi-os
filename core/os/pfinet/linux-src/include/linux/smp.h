#ifndef __LINUX_SMP_H
#define __LINUX_SMP_H
#ifdef __SMP__
#include <asm/smp.h>
extern void smp_send_stop(void);
extern void FASTCALL(smp_send_reschedule(int cpu));
extern void smp_boot_cpus(void);
extern void smp_callin(void);
extern void smp_commence(void);
extern int smp_call_function (void (*func) (void *info), void *info,
int retry, int wait);
extern int smp_threads_ready;
extern int smp_num_cpus;
extern volatile unsigned long smp_msg_data;
extern volatile int smp_src_cpu;
extern volatile int smp_msg_id;
#define MSG_ALL_BUT_SELF 0x8000
#define MSG_ALL 0x8001
#define MSG_INVALIDATE_TLB 0x0001
#define MSG_STOP_CPU 0x0002
#define MSG_RESCHEDULE 0x0003
#define MSG_CALL_FUNCTION 0x0004
#else
#define smp_num_cpus 1
#define smp_processor_id() 0
#define hard_smp_processor_id() 0
#define smp_threads_ready 1
#define kernel_lock()
#define cpu_logical_map(cpu) 0
#define smp_call_function(func,info,retry,wait)
#endif
#endif