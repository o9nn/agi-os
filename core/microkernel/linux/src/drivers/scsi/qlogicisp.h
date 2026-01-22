#ifndef _QLOGICISP_H
#define _QLOGICISP_H
#define QLOGICISP_REQ_QUEUE_LEN	63
#define QLOGICISP_MAX_SG(ql)	(4 + ((ql) > 0) ? 7*((ql) - 1) : 0)
int isp1020_detect(Scsi_Host_Template *);
int isp1020_release(struct Scsi_Host *);
const char * isp1020_info(struct Scsi_Host *);
int isp1020_queuecommand(Scsi_Cmnd *, void (* done)(Scsi_Cmnd *));
int isp1020_abort(Scsi_Cmnd *);
int isp1020_reset(Scsi_Cmnd *, unsigned int);
int isp1020_biosparam(Disk *, kdev_t, int[]);
#ifndef NULL
#define NULL (0)
#endif
static struct proc_dir_entry proc_scsi_isp1020;
#define QLOGICISP {							   \
NULL,					   \
NULL,					   \
NULL,					   \
NULL,					   \
NULL,					   \
isp1020_detect,				   \
isp1020_release,			   \
isp1020_info,				   \
NULL,					   \
isp1020_queuecommand,			   \
isp1020_abort,				   \
isp1020_reset,				   \
NULL,					   \
isp1020_biosparam,			   \
QLOGICISP_REQ_QUEUE_LEN,		   \
-1,					   \
QLOGICISP_MAX_SG(QLOGICISP_REQ_QUEUE_LEN), \
1,					   \
0,					   \
0,					   \
DISABLE_CLUSTERING			   \
}
#endif