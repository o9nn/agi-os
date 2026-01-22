#ifndef _EATA_PIO_H
#define _EATA_PIO_H
#include <linux/blk.h>
#include "scsi.h"
#include "hosts.h"
#include <scsi/scsicam.h>
#ifndef HOSTS_C
#include "eata_generic.h"
#define VER_MAJOR 0
#define VER_MINOR 0
#define VER_SUB "1b"
#define VERBOSE_SETUP
#define ALLOW_DMA_BOARDS 1
#define DEBUG_EATA 1
#define DPT_DEBUG 0
#define DBG_DELAY 0
#define DBG_PROBE 0
#define DBG_ISA 0
#define DBG_EISA 0
#define DBG_PCI 0
#define DBG_PIO 0
#define DBG_COM 0
#define DBG_QUEUE 0
#define DBG_INTR 0
#define DBG_INTR2 0
#define DBG_PROC 0
#define DBG_PROC_WRITE 0
#define DBG_REGISTER 0
#define DBG_ABNORM 1
#if DEBUG_EATA
#define DBG(x, y) if ((x)) {y;}
#else
#define DBG(x, y)
#endif
#endif
int eata_pio_detect(Scsi_Host_Template *);
const char *eata_pio_info(struct Scsi_Host *);
int eata_pio_command(Scsi_Cmnd *);
int eata_pio_queue(Scsi_Cmnd *, void (*done)(Scsi_Cmnd *));
int eata_pio_abort(Scsi_Cmnd *);
int eata_pio_reset(Scsi_Cmnd *, unsigned int);
int eata_pio_proc_info(char *, char **, off_t, int, int, int);
#ifdef MODULE
int eata_pio_release(struct Scsi_Host *);
#else
#define eata_pio_release NULL
#endif
#define EATA_PIO { \
NULL, NULL, \
NULL, \
eata_pio_proc_info, \
"EATA (Extended Attachment) PIO driver", \
eata_pio_detect, \
eata_pio_release, \
NULL, NULL, \
eata_pio_queue, \
eata_pio_abort, \
eata_pio_reset, \
NULL, \
scsicam_bios_param, \
0, \
0, \
0, \
0, \
0, \
1, \
ENABLE_CLUSTERING }
#endif