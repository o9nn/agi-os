#ifndef SYM53C8XX_H
#define SYM53C8XX_H
#include "sym53c8xx_defs.h"
#if defined(HOSTS_C) || defined(MODULE)
#include <scsi/scsicam.h>
int sym53c8xx_abort(Scsi_Cmnd *);
int sym53c8xx_detect(Scsi_Host_Template *tpnt);
const char *sym53c8xx_info(struct Scsi_Host *host);
int sym53c8xx_queue_command(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
int sym53c8xx_reset(Scsi_Cmnd *, unsigned int);
#ifdef MODULE
int sym53c8xx_release(struct Scsi_Host *);
#else
#define sym53c8xx_release NULL
#endif
#if LINUX_VERSION_CODE >= LinuxVersionCode(2,1,75)
#define SYM53C8XX { name: "", \
detect: sym53c8xx_detect, \
release: sym53c8xx_release, \
info: sym53c8xx_info, \
queuecommand: sym53c8xx_queue_command,\
abort: sym53c8xx_abort, \
reset: sym53c8xx_reset, \
bios_param: scsicam_bios_param, \
can_queue: SCSI_NCR_CAN_QUEUE, \
this_id: 7, \
sg_tablesize: SCSI_NCR_SG_TABLESIZE, \
cmd_per_lun: SCSI_NCR_CMD_PER_LUN, \
use_clustering: DISABLE_CLUSTERING}
#else
#define SYM53C8XX { NULL, NULL, NULL, NULL, \
NULL, sym53c8xx_detect, \
sym53c8xx_release, sym53c8xx_info, NULL, \
sym53c8xx_queue_command,sym53c8xx_abort, \
sym53c8xx_reset, NULL, scsicam_bios_param, \
SCSI_NCR_CAN_QUEUE, 7, \
SCSI_NCR_SG_TABLESIZE, SCSI_NCR_CMD_PER_LUN, \
0, 0, DISABLE_CLUSTERING}
#endif
#endif
#endif