#ifndef T128_H
#define T128_H
#define T128_PUBLIC_RELEASE 3
#define TDEBUG_INIT	0x1
#define TDEBUG_TRANSFER 0x2
#define T_ROM_OFFSET		0
#define T_RAM_OFFSET		0x1800
#define T_CONTROL_REG_OFFSET	0x1c00
#define T_CR_INT		0x10
#define T_CR_CT			0x02
#define T_STATUS_REG_OFFSET	0x1c20
#define T_ST_BOOT		0x80
#define T_ST_S3			0x40
#define T_ST_S2			0x20
#define T_ST_S1			0x10
#define T_ST_PS2		0x08
#define T_ST_RDY		0x04
#define T_ST_TIM		0x02
#define T_ST_ZERO		0x01
#define T_5380_OFFSET		0x1d00
#define T_DATA_REG_OFFSET	0x1e00
#ifndef ASM
int t128_abort(Scsi_Cmnd *);
int t128_biosparam(Disk *, kdev_t, int*);
int t128_detect(Scsi_Host_Template *);
int t128_queue_command(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
int t128_reset(Scsi_Cmnd *, unsigned int reset_flags);
int t128_proc_info (char *buffer, char **start, off_t offset,
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
#define TRANTOR_T128 {NULL, NULL, NULL, NULL, \
"Trantor T128/T128F/T228", t128_detect, NULL,  \
NULL,							\
NULL, t128_queue_command, t128_abort, t128_reset, NULL, 	\
t128_biosparam, 						\
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
#define T128_address(reg) (base + T_5380_OFFSET + ((reg) * 0x20))
#if !(TDEBUG & TDEBUG_TRANSFER)
#define NCR5380_read(reg) (*(T128_address(reg)))
#define NCR5380_write(reg, value) (*(T128_address(reg)) = (value))
#else
#define NCR5380_read(reg)						\
(((unsigned char) printk("scsi%d : read register %d at address %08x\n"\
, instance->hostno, (reg), T128_address(reg))), *(T128_address(reg)))
#define NCR5380_write(reg, value) {					\
printk("scsi%d : write %02x to register %d at address %08x\n", 	\
instance->hostno, (value), (reg), T128_address(reg));	\
*(T128_address(reg)) = (value);					\
}
#endif
#define NCR5380_intr t128_intr
#define NCR5380_queue_command t128_queue_command
#define NCR5380_abort t128_abort
#define NCR5380_reset t128_reset
#define NCR5380_proc_info t128_proc_info
#define T128_IRQS 0xc4a8
#endif
#endif
#endif