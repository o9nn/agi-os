#ifndef _ADVANSYS_H
#define _ADVANSYS_H
#define ASC_LINUX_VERSION(V, P, S)    (((V) * 65536) + ((P) * 256) + (S))
#ifndef LINUX_VERSION_CODE
#include <linux/version.h>
#endif
int advansys_detect(Scsi_Host_Template *);
int advansys_release(struct Scsi_Host *);
const char *advansys_info(struct Scsi_Host *);
int advansys_command(Scsi_Cmnd *);
int advansys_queuecommand(Scsi_Cmnd *, void (* done)(Scsi_Cmnd *));
int advansys_abort(Scsi_Cmnd *);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,89)
int advansys_reset(Scsi_Cmnd *);
#else
int advansys_reset(Scsi_Cmnd *, unsigned int);
#endif
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
int advansys_biosparam(Disk *, int, int[]);
#else
int advansys_biosparam(Disk *, kdev_t, int[]);
extern struct proc_dir_entry proc_scsi_advansys;
int advansys_proc_info(char *, char **, off_t, int, int, int);
#endif
void advansys_setup(char *, int *);
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,0)
#define ADVANSYS { \
NULL,                      \
NULL,                      \
"advansys",                \
advansys_detect,           \
advansys_release,          \
advansys_info,             \
advansys_command,          \
advansys_queuecommand, \
\
advansys_abort,            \
advansys_reset,            \
NULL,                      \
advansys_biosparam,        \
\
0,                         \
0,                         \
0,                         \
0,                         \
0,                            \
\
1,                         \
\
ENABLE_CLUSTERING,         \
}
#elif LINUX_VERSION_CODE < ASC_LINUX_VERSION(2,1,75)
#define ADVANSYS { \
NULL,                     \
NULL, \
\
\
&proc_scsi_advansys,      \
advansys_proc_info,    \
\
"advansys",               \
advansys_detect,          \
advansys_release,         \
advansys_info,            \
advansys_command,         \
advansys_queuecommand, \
\
advansys_abort,           \
advansys_reset, \
\
\
NULL,                     \
advansys_biosparam,       \
\
0,                         \
0,                         \
0,                         \
0,                         \
0,                            \
\
1,                         \
\
ENABLE_CLUSTERING,         \
}
#else
#define ADVANSYS { \
proc_dir:     &proc_scsi_advansys, \
proc_info:    advansys_proc_info, \
name:         "advansys", \
detect:       advansys_detect, \
release:      advansys_release, \
info:         advansys_info, \
command:      advansys_command, \
queuecommand: advansys_queuecommand, \
abort:        advansys_abort, \
reset:        advansys_reset, \
bios_param:    advansys_biosparam, \
\
unchecked_isa_dma: 1, \
\
use_clustering: ENABLE_CLUSTERING, \
}
#endif
#endif