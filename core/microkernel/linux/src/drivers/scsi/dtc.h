#ifndef DTC3280_H
#define DTC3280_H
#define DTC_PUBLIC_RELEASE 1
#define DTCDEBUG_INIT	0x1
#define DTCDEBUG_TRANSFER 0x2
#define DTC_CONTROL_REG		0x100
#define D_CR_ACCESS		0x80
#define CSR_DIR_READ		0x40
#define CSR_RESET              0x80
#define CSR_5380_REG           0x80
#define CSR_TRANS_DIR          0x40
#define CSR_SCSI_BUFF_INTR     0x20
#define CSR_5380_INTR          0x10
#define CSR_SHARED_INTR        0x08
#define CSR_HOST_BUF_NOT_RDY   0x04
#define CSR_SCSI_BUF_RDY       0x02
#define CSR_GATED_5380_IRQ     0x01
#define CSR_INT_BASE (CSR_SCSI_BUFF_INTR | CSR_5380_INTR)
#define DTC_BLK_CNT		0x101
#define D_CR_ACCESS             0x80
#define DTC_SWITCH_REG		0x3982
#define DTC_RESUME_XFER		0x3982
#define DTC_5380_OFFSET		0x3880
#define DTC_DATA_BUF		0x3900
#ifndef ASM
int dtc_abort(Scsi_Cmnd *);
int dtc_biosparam(Disk *, kdev_t, int*);
int dtc_detect(Scsi_Host_Template *);
int dtc_queue_command(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
int dtc_reset(Scsi_Cmnd *, unsigned int reset_flags);
int dtc_proc_info (char *buffer, char **start, off_t offset,
int length, int hostno, int inout);
#ifndef NULL
#define NULL 0
#endif
#ifndef CMD_PER_LUN
#define CMD_PER_LUN 2
#endif
#ifndef CAN_QUEUE
#define CAN_QUEUE 32
#endif
#if defined(HOSTS_C) || defined(MODULE)
#define DTC3x80 {NULL, NULL, NULL, NULL, \
"DTC 3180/3280 ", dtc_detect, NULL,  \
NULL,							\
NULL, dtc_queue_command, dtc_abort, dtc_reset, NULL, 	\
dtc_biosparam, 						\
CAN_QUEUE,  7, SG_ALL,			\
CMD_PER_LUN , 0, 0, DISABLE_CLUSTERING}
#endif
#ifndef HOSTS_C
#define NCR5380_implementation_fields \
volatile unsigned char *base
#define NCR5380_local_declare() \
volatile unsigned char *base
#define NCR5380_setup(instance) \
base = (volatile unsigned char *) (instance)->base
#define DTC_address(reg) (base + DTC_5380_OFFSET + reg)
#define dbNCR5380_read(reg)                                              \
(rval=*(DTC_address(reg)), \
(((unsigned char) printk("DTC : read register %d at addr %08x is: %02x\n"\
, (reg), (int)DTC_address(reg), rval)), rval ) )
#define dbNCR5380_write(reg, value) do {                                  \
printk("DTC : write %02x to register %d at address %08x\n",         \
(value), (reg), (int)DTC_address(reg));     \
*(DTC_address(reg)) = (value);} while(0)
#if !(DTCDEBUG & DTCDEBUG_TRANSFER)
#define NCR5380_read(reg) (*(DTC_address(reg)))
#define NCR5380_write(reg, value) (*(DTC_address(reg)) = (value))
#else
#define NCR5380_read(reg) (*(DTC_address(reg)))
#define xNCR5380_read(reg)						\
(((unsigned char) printk("DTC : read register %d at address %08x\n"\
, (reg), DTC_address(reg))), *(DTC_address(reg)))
#define NCR5380_write(reg, value) do {					\
printk("DTC : write %02x to register %d at address %08x\n", 	\
(value), (reg), (int)DTC_address(reg));	\
*(DTC_address(reg)) = (value);		} while(0)
#endif
#define NCR5380_intr dtc_intr
#define NCR5380_queue_command dtc_queue_command
#define NCR5380_abort dtc_abort
#define NCR5380_reset dtc_reset
#define NCR5380_proc_info dtc_proc_info
#define DTC_IRQS 0x9c00
#endif
#endif
#endif