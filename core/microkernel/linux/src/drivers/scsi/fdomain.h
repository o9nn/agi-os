#ifndef _FDOMAIN_H
#define _FDOMAIN_H
int fdomain_16x0_detect( Scsi_Host_Template * );
int fdomain_16x0_command( Scsi_Cmnd * );
int fdomain_16x0_abort( Scsi_Cmnd * );
const char *fdomain_16x0_info( struct Scsi_Host * );
int fdomain_16x0_reset( Scsi_Cmnd *, unsigned int );
int fdomain_16x0_queue( Scsi_Cmnd *, void (*done)(Scsi_Cmnd *) );
int fdomain_16x0_biosparam( Disk *, kdev_t, int * );
int fdomain_16x0_proc_info( char *buffer, char **start, off_t offset,
int length, int hostno, int inout );
extern struct proc_dir_entry proc_scsi_fdomain;
#define FDOMAIN_16X0 { NULL, \
NULL, \
NULL, \
fdomain_16x0_proc_info, \
NULL, \
fdomain_16x0_detect, \
NULL, \
fdomain_16x0_info, \
fdomain_16x0_command, \
fdomain_16x0_queue, \
fdomain_16x0_abort, \
fdomain_16x0_reset, \
NULL, \
fdomain_16x0_biosparam, \
1, \
6, \
64, \
1, \
0, \
0, \
DISABLE_CLUSTERING }
#endif