#ifndef _aic7xxx_h
#define _aic7xxx_h
#define AIC7XXX_H_VERSION "3.2.4"
#ifndef LINUX_VERSION_CODE
#include <linux/version.h>
#endif
#ifndef KERNEL_VERSION
#define KERNEL_VERSION(x,y,z) (((x)<<16)+((y)<<8)+(z))
#endif
#if defined(__i386__)
# define AIC7XXX_BIOSPARAM aic7xxx_biosparam
#else
# define AIC7XXX_BIOSPARAM NULL
#endif
#if LINUX_VERSION_CODE > KERNEL_VERSION(2,1,65)
#define AIC7XXX { \
next: NULL, \
module: NULL, \
proc_dir: NULL, \
proc_info: aic7xxx_proc_info, \
name: NULL, \
detect: aic7xxx_detect, \
release: aic7xxx_release, \
info: aic7xxx_info, \
command: NULL, \
queuecommand: aic7xxx_queue, \
eh_strategy_handler: NULL, \
eh_abort_handler: NULL, \
eh_device_reset_handler: NULL, \
eh_bus_reset_handler: NULL, \
eh_host_reset_handler: NULL, \
abort: aic7xxx_abort, \
reset: aic7xxx_reset, \
slave_attach: NULL, \
bios_param: AIC7XXX_BIOSPARAM, \
can_queue: 255, \
this_id: -1, \
sg_tablesize: 0, \
cmd_per_lun: 3, \
present: 0, \
unchecked_isa_dma: 0, \
use_clustering: ENABLE_CLUSTERING, \
use_new_eh_code: 0 \
}
#else
#define AIC7XXX { \
next: NULL, \
usage_count: NULL, \
proc_dir: NULL, \
proc_info: aic7xxx_proc_info, \
name: NULL, \
detect: aic7xxx_detect, \
release: aic7xxx_release, \
info: aic7xxx_info, \
command: NULL, \
queuecommand: aic7xxx_queue, \
abort: aic7xxx_abort, \
reset: aic7xxx_reset, \
slave_attach: NULL, \
bios_param: AIC7XXX_BIOSPARAM, \
can_queue: 255, \
this_id: -1, \
sg_tablesize: 0, \
cmd_per_lun: 3, \
present: 0, \
unchecked_isa_dma: 0, \
use_clustering: ENABLE_CLUSTERING \
}
#endif
extern int aic7xxx_queue(Scsi_Cmnd *, void (*)(Scsi_Cmnd *));
extern int aic7xxx_biosparam(Disk *, kdev_t, int[]);
extern int aic7xxx_detect(Scsi_Host_Template *);
extern int aic7xxx_command(Scsi_Cmnd *);
extern int aic7xxx_reset(Scsi_Cmnd *, unsigned int);
extern int aic7xxx_abort(Scsi_Cmnd *);
extern int aic7xxx_release(struct Scsi_Host *);
extern const char *aic7xxx_info(struct Scsi_Host *);
extern int aic7xxx_proc_info(char *, char **, off_t, int, int, int);
#endif