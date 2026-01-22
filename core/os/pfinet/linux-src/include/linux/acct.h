#ifndef _LINUX_ACCT_H
#define _LINUX_ACCT_H
#include <linux/types.h>
typedef __u16 comp_t;
#define ACCT_COMM 16
struct acct
{
char ac_flag;
__u16 ac_uid;
__u16 ac_gid;
__u16 ac_tty;
__u32 ac_btime;
comp_t ac_utime;
comp_t ac_stime;
comp_t ac_etime;
comp_t ac_mem;
comp_t ac_io;
comp_t ac_rw;
comp_t ac_minflt;
comp_t ac_majflt;
comp_t ac_swaps;
__u32 ac_exitcode;
char ac_comm[ACCT_COMM + 1];
char ac_pad[10];
};
#define AFORK 0x01
#define ASU 0x02
#define ACOMPAT 0x04
#define ACORE 0x08
#define AXSIG 0x10
#define AHZ 100
#ifdef __KERNEL__
#include <linux/config.h>
#ifdef CONFIG_BSD_PROCESS_ACCT
extern void acct_auto_close(kdev_t dev);
extern int acct_process(long exitcode);
#else
#define acct_auto_close(x) do { } while (0)
#define acct_process(x) do { } while (0)
#endif
#endif
#endif