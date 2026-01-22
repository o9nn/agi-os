#ifndef DISKFS_PRIV_H
#define DISKFS_PRIV_H
#include <mach.h>
#include <hurd.h>
#include <sys/mman.h>
#include <hurd/ports.h>
#include <hurd/fshelp.h>
#include <hurd/iohelp.h>
#include <hurd/port.h>
#include <assert-backtrace.h>
#include <argp.h>
#include "diskfs.h"
extern int _diskfs_nosuid, _diskfs_noexec;
extern int _diskfs_noatime;
extern int _diskfs_relatime;
extern int _diskfs_no_inherit_dir_group;
extern char *_diskfs_chroot_directory;
extern char **_diskfs_boot_command;
extern struct hurd_port _diskfs_exec_portcell;
extern const struct argp_option diskfs_common_options[];
#define OPT_SUID_OK 600
#define OPT_EXEC_OK 601
#define OPT_ATIME 602
#define OPT_NO_INHERIT_DIR_GROUP 603
#define OPT_INHERIT_DIR_GROUP 604
#define DEFAULT_SYNC_INTERVAL 30
#define DEFAULT_SYNC_INTERVAL_STRING STRINGIFY(DEFAULT_SYNC_INTERVAL)
#define STRINGIFY(x) STRINGIFY_1(x)
#define STRINGIFY_1(x) #x
extern int _diskfs_diskdirty;
typedef struct protid *protid_t;
typedef struct diskfs_control *control_t;
typedef struct bootinfo *bootinfo_t;
error_t _diskfs_rdwr_internal (struct node *np, char *data, off_t offset,
mach_msg_type_number_t *amt,
int dir, int notime);
void _diskfs_init_completed (void);
void _diskfs_boot_privports (void);
void _diskfs_control_clean (void *);
void _diskfs_lastref (struct node *np);
int atime_should_update (struct node *np);
extern int _diskfs_ncontrol_ports;
extern pthread_spinlock_t _diskfs_control_lock;
extern fshelp_fetch_root_callback1_t _diskfs_translator_callback1;
extern fshelp_fetch_root_callback2_t _diskfs_translator_callback2;
#define CHANGE_NODE_FIELD(PROTID, OPERATION) \
({ \
error_t err = 0; \
struct node *np; \
\
if (!(PROTID)) \
return EOPNOTSUPP; \
\
if (diskfs_check_readonly ()) \
return EROFS; \
\
np = (PROTID)->po->np; \
\
pthread_mutex_lock (&np->lock); \
(OPERATION); \
if (diskfs_synchronous) \
diskfs_node_update (np, 1); \
pthread_mutex_unlock (&np->lock); \
return err; \
})
#define HONORED_STATE_MODES (O_APPEND|O_ASYNC|O_FSYNC|O_NONBLOCK|O_NOATIME)
#define OPENONLY_STATE_MODES \
(O_CREAT|O_EXCL|O_NOLINK|O_NOTRANS|O_NONBLOCK|O_EXLOCK|O_SHLOCK)
#endif