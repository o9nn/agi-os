#ifndef _NCR53C406A_H
#define _NCR53C406A_H
#ifndef NULL
#define NULL 0
#endif
#define NCR53c406a { \
NULL , \
NULL , \
&proc_scsi_NCR53c406a , \
NULL , \
"NCR53c406a" , \
NCR53c406a_detect , \
NULL , \
NCR53c406a_info , \
NCR53c406a_command , \
NCR53c406a_queue , \
NCR53c406a_abort , \
NCR53c406a_reset , \
NULL , \
NCR53c406a_biosparm , \
1 , \
7 , \
32 , \
1 , \
0 , \
1 , \
ENABLE_CLUSTERING \
}
extern struct proc_dir_entry proc_scsi_NCR53c406a;
int NCR53c406a_detect(Scsi_Host_Template *);
const char* NCR53c406a_info(struct Scsi_Host *);
int NCR53c406a_command(Scsi_Cmnd *);
int NCR53c406a_queue(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
int NCR53c406a_abort(Scsi_Cmnd *);
int NCR53c406a_reset(Scsi_Cmnd *, unsigned int);
int NCR53c406a_biosparm(Disk *, kdev_t, int []);
#endif