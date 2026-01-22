#ifndef __LINUX_SMP_H
#define __LINUX_SMP_H
#ifdef __SMP__
#include <asm/smp.h>
extern void smp_message_pass(int target, int msg, unsigned long data, int wait);
extern void smp_boot_cpus(void);
extern void smp_callin(void);
extern void smp_commence(void);
extern int smp_num_cpus;
extern int smp_threads_ready;
#ifdef __SMP_PROF__
extern volatile unsigned long smp_spins[NR_CPUS];
extern volatile unsigned long smp_spins_sys_idle[];
extern volatile unsigned long smp_spins_syscall[];
extern volatile unsigned long smp_spins_syscall_cur[];
extern volatile unsigned long smp_idle_count[1+NR_CPUS];
extern volatile unsigned long smp_idle_map;
#else
extern volatile unsigned long smp_spins;
#endif
extern volatile unsigned long smp_msg_data;
extern volatile int smp_src_cpu;
extern volatile int smp_msg_id;
#define MSG_ALL_BUT_SELF 0x8000
#define MSG_ALL 0x8001
#define MSG_INVALIDATE_TLB 0x0001
#define MSG_STOP_CPU 0x0002
#define MSG_RESCHEDULE 0x0003
#else
#define smp_num_cpus 1
#define smp_processor_id() 0
#define smp_message_pass(t,m,d,w)
#define smp_threads_ready 1
#define kernel_lock()
#endif
#endif