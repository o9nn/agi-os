#include <linux/version.h>
#define ASC_LINUX_VERSION(V, P, S)	(((V) * 65536) + ((P) * 256) + (S))
#if LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,50)
#define VERSION_ELF_1_2_13
#elseif LINUX_VERSION_CODE < ASC_LINUX_VERSION(1,3,95)
#define VERSION_1_3_85
#else
#define VERSION_2_0_0
#endif
#ifndef DC390_H
#define DC390_H
#if defined(HOSTS_C) || defined(MODULE)
#ifdef	VERSION_2_0_0
#include <scsi/scsicam.h>
#else
#include <linux/scsicam.h>
#endif
extern int DC390_detect(Scsi_Host_Template *psht);
extern int DC390_queue_command(Scsi_Cmnd *cmd, void (*done)(Scsi_Cmnd *));
extern int DC390_abort(Scsi_Cmnd *cmd);
#ifdef	VERSION_2_0_0
extern int DC390_reset(Scsi_Cmnd *cmd, unsigned int resetFlags);
#else
extern int DC390_reset(Scsi_Cmnd *cmd);
#endif
#ifdef	VERSION_ELF_1_2_13
extern int DC390_bios_param(Disk *disk, int devno, int geom[]);
#else
extern int DC390_bios_param(Disk *disk, kdev_t devno, int geom[]);
#endif
#ifdef MODULE
static int DC390_release(struct Scsi_Host *);
#else
#define DC390_release NULL
#endif
#ifndef VERSION_ELF_1_2_13
extern struct proc_dir_entry proc_scsi_tmscsim;
extern int tmscsim_proc_info(char *buffer, char **start, off_t offset, int length, int hostno, int inout);
#endif
#ifdef	VERSION_2_0_0
#define DC390_T    {			\
NULL,			\
NULL,		\
&proc_scsi_tmscsim,	 	\
tmscsim_proc_info,		\
"Tekram DC390(T) V1.11 Feb-05-1997",   \
DC390_detect,			\
DC390_release,		\
NULL,		\
NULL,		\
DC390_queue_command,	\
DC390_abort,		\
DC390_reset,		\
NULL, \
DC390_bios_param,	\
10,	\
7,  	\
SG_ALL, 		\
2,  \
0, 	\
0,  \
DISABLE_CLUSTERING	\
}
#endif
#ifdef	VERSION_1_3_85
#define DC390_T    {			\
NULL,			\
NULL,		\
&proc_scsi_tmscsim,	 	\
tmscsim_proc_info,		\
"Tekram DC390(T) V1.11 Feb-05-1997",   \
DC390_detect,			\
DC390_release,		\
NULL,		\
NULL,		\
DC390_queue_command,	\
DC390_abort,		\
DC390_reset,		\
NULL, \
DC390_bios_param,	\
10,	\
7,  	\
SG_ALL, 		\
2,  \
0, 	\
0,  \
DISABLE_CLUSTERING	\
}
#endif
#ifdef	VERSION_ELF_1_2_13
#define DC390_T     {		\
NULL,			\
NULL,			\
"Tekram DC390(T) V1.11 Feb-05-1997",\
DC390_detect,		\
DC390_release,			\
NULL, 	\
NULL,  \
DC390_queue_command,	\
DC390_abort,		\
DC390_reset,		\
NULL, \
DC390_bios_param,	\
10,	\
7,  	\
16,	\
2,  \
0, 	\
0,  \
DISABLE_CLUSTERING	\
}
#endif
#endif
#endif