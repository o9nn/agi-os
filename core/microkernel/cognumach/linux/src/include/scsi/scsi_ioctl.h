#ifndef _SCSI_IOCTL_H
#define _SCSI_IOCTL_H
#define SCSI_IOCTL_SEND_COMMAND 1
#define SCSI_IOCTL_TEST_UNIT_READY 2
#define SCSI_IOCTL_BENCHMARK_COMMAND 3
#define SCSI_IOCTL_SYNC 4
#define SCSI_IOCTL_START_UNIT 5
#define SCSI_IOCTL_STOP_UNIT 6
#define SCSI_IOCTL_DOORLOCK 0x5380
#define SCSI_IOCTL_DOORUNLOCK 0x5381
#define	SCSI_REMOVAL_PREVENT	1
#define	SCSI_REMOVAL_ALLOW	0
#ifdef __KERNEL__
extern int scsi_ioctl (Scsi_Device *dev, int cmd, void *arg);
extern int kernel_scsi_ioctl (Scsi_Device *dev, int cmd, void *arg);
extern int scsi_ioctl_send_command(Scsi_Device *dev, void *buffer);
#endif
#endif