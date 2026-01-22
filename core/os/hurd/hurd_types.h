#ifndef _HURD_TYPES_H
#define _HURD_TYPES_H
#include <time.h>
#include <mach/std_types.h>
#include <mach/message.h>
#include <sys/types.h>
#define HURD_RELEASE "0.0"
typedef mach_port_t file_t;
typedef mach_port_t fsys_t;
typedef mach_port_t io_t;
typedef mach_port_t process_t;
typedef mach_port_t auth_t;
typedef mach_port_t socket_t;
typedef mach_port_t pf_t;
typedef mach_port_t addr_port_t;
typedef mach_port_t startup_t;
typedef mach_port_t fs_notify_t;
typedef mach_port_t exec_startup_t;
typedef mach_port_t interrupt_t;
typedef mach_port_t proccoll_t;
typedef mach_port_t ctty_t;
typedef mach_port_t pci_t;
typedef mach_port_t shutdown_t;
typedef mach_port_t acpi_t;
#include <errno.h>
typedef char *data_t;
typedef const char *const_data_t;
typedef char string_t [1024];
typedef const char *const_string_t;
typedef int *intarray_t;
typedef const int *const_intarray_t;
typedef int *fd_mask_t;
typedef const int *const_fd_mask_t;
typedef mach_port_t *portarray_t;
typedef const mach_port_t *const_portarray_t;
typedef pid_t *pidarray_t;
typedef const pid_t *const_pidarray_t;
typedef uid_t *idarray_t;
typedef const uid_t *const_idarray_t;
typedef __loff_t *off_array_t;
typedef const __loff_t *const_off_array_t;
typedef struct rusage rusage_t;
typedef struct flock64 flock_t;
typedef struct utsname utsname_t;
#if _FILE_OFFSET_BITS == 64
typedef struct stat io_statbuf_t;
typedef struct statfs fsys_statfsbuf_t;
#else
typedef struct stat64 io_statbuf_t;
typedef struct statfs64 fsys_statfsbuf_t;
#endif
typedef struct timespec timespec_t;
#define EXEC_NEWTASK	0x00000001
#define EXEC_SECURE	0x00000002
#define EXEC_DEFAULTS	0x00000004
#define EXEC_SIGTRAP	0x00000008
#define	EXEC_STACK_ARGS	0x00000010
#define FS_TRANS_FORCE     0x00000001
#define FS_TRANS_EXCL      0x00000002
#define FS_TRANS_SET	   0x00000004
#define FS_TRANS_ORPHAN    0x00000008
enum retry_type
{
FS_RETRY_NORMAL = 1,
FS_RETRY_REAUTH = 2,
FS_RETRY_MAGICAL = 3,
};
typedef enum retry_type retry_type;
enum dir_changed_type
{
DIR_CHANGED_NULL,
DIR_CHANGED_NEW,
DIR_CHANGED_UNLINK,
DIR_CHANGED_RENUMBER,
};
typedef enum dir_changed_type dir_changed_type_t;
enum file_changed_type
{
FILE_CHANGED_NULL,
FILE_CHANGED_WRITE,
FILE_CHANGED_EXTEND,
FILE_CHANGED_TRUNCATE,
FILE_CHANGED_META,
};
typedef enum file_changed_type file_changed_type_t;
#define SELECT_READ  0x00000001
#define SELECT_WRITE 0x00000002
#define SELECT_URG   0x00000004
#define FSYS_GOAWAY_NOWAIT    0x00000001
#define FSYS_GOAWAY_NOSYNC    0x00000002
#define FSYS_GOAWAY_FORCE     0x00000004
#define FSYS_GOAWAY_UNLINK    0x00000008
#define FSYS_GOAWAY_RECURSE   0x00000010
enum term_bottom_type
{
TERM_ON_MACHDEV,
TERM_ON_HURDIO,
TERM_ON_MASTERPTY,
};
enum file_storage_class
{
STORAGE_OTHER,
STORAGE_DEVICE,
STORAGE_HURD_FILE,
STORAGE_NETWORK,
STORAGE_MEMORY,
STORAGE_TASK,
STORAGE_ZERO,
STORAGE_CONCAT,
STORAGE_INTERLEAVE,
STORAGE_LAYER,
STORAGE_REMAP,
STORAGE_COPY,
};
#define STORAGE_MUTATED   0x00000001
#include <mach/task_info.h>
#include <mach/thread_info.h>
#ifndef THREAD_SCHED_INFO
#include <mach/policy.h>
#endif
#define PI_FETCH_TASKINFO	0x0001
#define PI_FETCH_TASKEVENTS	0x0020
#define PI_FETCH_THREADS	0x0002
#define PI_FETCH_THREAD_BASIC	0x0004
#define PI_FETCH_THREAD_SCHED	0x0008
#define PI_FETCH_THREAD_WAITS	0x0010
struct procinfo
{
int state;
uid_t owner;
pid_t ppid;
pid_t pgrp;
pid_t session;
pid_t logincollection;
int exitstatus;
int sigcode;
int nthreads;
struct task_basic_info taskinfo;
struct task_events_info taskevents;
#ifdef TASK_SCHED_TIMESHARE_INFO
struct policy_timeshare_base timeshare_base_info;
#endif
struct
{
int died;
mach_msg_id_t rpc_block;
struct thread_basic_info pis_bi;
#ifdef THREAD_SCHED_INFO
struct thread_sched_info pis_si;
#else
struct policy_infos pis_pi;
#endif
} threadinfos[0];
};
typedef int *procinfo_t;
typedef const int *const_procinfo_t;
#define PI_STOPPED 0x00000001
#define PI_EXECED  0x00000002
#define PI_WAITING 0x00000004
#define PI_ORPHAN  0x00000008
#define PI_NOMSG   0x00000010
#define PI_SESSLD  0x00000020
#define PI_NOTOWNED 0x0000040
#define PI_NOPARENT 0x0000080
#define PI_ZOMBIE  0x00000100
#define PI_TRACED  0x00000200
#define PI_GETMSG  0x00000400
#define PI_LOGINLD 0x00000800
#define FSTYPE_UFS     0x00000000
#define FSTYPE_NFS     0x00000001
#define FSTYPE_GFS     0x00000002
#define FSTYPE_LFS     0x00000003
#define FSTYPE_SYSV    0x00000004
#define FSTYPE_FTP     0x00000005
#define FSTYPE_TAR     0x00000006
#define FSTYPE_AR      0x00000007
#define FSTYPE_CPIO    0x00000008
#define FSTYPE_MSLOSS  0x00000009
#define FSTYPE_CPM     0x0000000a
#define FSTYPE_HFS     0x0000000b
#define FSTYPE_DTFS    0x0000000c
#define FSTYPE_GRFS    0x0000000d
#define FSTYPE_TERM    0x0000000e
#define FSTYPE_DEV     0x0000000f
#define FSTYPE_PROC    0x00000010
#define FSTYPE_IFSOCK  0x00000011
#define FSTYPE_AFS     0x00000012
#define FSTYPE_DFS     0x00000013
#define FSTYPE_PROC9   0x00000014
#define FSTYPE_SOCKET  0x00000015
#define FSTYPE_MISC    0x00000016
#define FSTYPE_EXT2FS  0x00000017
#define FSTYPE_HTTP    0x00000018
#define FSTYPE_MEMFS   0x00000019
#define FSTYPE_ISO9660 0x0000001a
#define FSTYPE_PCI     0x0000001b
#define FSTYPE_ACPI    0x0000001c
enum
{
INIT_PORT_CWDIR,
INIT_PORT_CRDIR,
INIT_PORT_AUTH,
INIT_PORT_PROC,
INIT_PORT_CTTYID,
INIT_PORT_BOOTSTRAP,
INIT_PORT_MAX
};
enum
{
INIT_UMASK,
INIT_SIGMASK,
INIT_SIGIGN,
INIT_SIGPENDING,
INIT_TRACEMASK,
INIT_INT_MAX,
};
#include <stdint.h>
struct pci_bar
{
uint64_t base_addr;
uint64_t size;
unsigned is_IO:1;
unsigned is_prefetchable:1;
unsigned is_64:1;
};
struct pci_xrom_bar
{
uint64_t base_addr;
uint64_t size;
};
#endif