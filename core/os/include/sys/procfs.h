#ifndef _SYS_PROCFS_H
#define _SYS_PROCFS_H 1
#include <features.h>
#include <inttypes.h>
#include <mach/std_types.h>
#include <sys/types.h>
#include <sys/time.h>
#include <ucontext.h>
__BEGIN_DECLS
#define ELF_PRARGSZ (80)
struct elf_psinfo
{
int pr_flag;
int pr_nlwp;
pid_t pr_pid;
pid_t pr_ppid;
pid_t pr_pgid;
pid_t pr_sid;
uid_t pr_uid, pr_euid;
gid_t pr_gid, pr_egid;
size_t pr_size;
size_t pr_rssize;
uint16_t pr_pctcpu;
uint16_t pr_pctmem;
struct timespec pr_start;
struct timespec pr_time;
struct timespec pr_ctime;
uint32_t pr_reserved1[2];
char pr_fname[16];
char pr_psargs[ELF_PRARGSZ];
int pr_wstat;
int pr_argc;
vm_address_t pr_argv;
vm_address_t pr_envp;
};
typedef struct elf_psinfo psinfo_t;
struct elf_pstatus
{
int pr_flags;
int pr_nlwp;
pid_t pr_pid;
pid_t pr_ppid;
pid_t pr_pgid;
pid_t pr_sid;
struct timespec pr_utime;
struct timespec pr_stime;
struct timespec pr_cutime;
struct timespec pr_cstime;
};
typedef struct elf_pstatus pstatus_t;
struct elf_siginfo
{
int si_signo;
int si_code;
int si_errno;
};
typedef gregset_t prgregset_t;
typedef fpregset_t prfpregset_t;
struct elf_lwpstatus
{
int pr_flags;
int pr_lwpid;
int pr_cursig;
struct elf_siginfo pr_info;
prgregset_t pr_reg;
prfpregset_t pr_fpreg;
};
typedef struct elf_lwpstatus lwpstatus_t;
__END_DECLS
#endif