#ifndef _EATA_DMA_H
#define _EATA_DMA_H
#ifndef HOSTS_C
#include "eata_generic.h"
#define VER_MAJOR 2
#define VER_MINOR 5
#define VER_SUB   "9b"
#define CHECKPAL        0
#define CHECK_BLINK     1
#define CRIPPLE_QUEUE   0
#define DEBUG_EATA      1
#define DPT_DEBUG       0
#define DBG_DELAY       0
#define DBG_PROBE       0
#define DBG_PCI         0
#define DBG_EISA        0
#define DBG_ISA         0
#define DBG_BLINK       0
#define DBG_PIO         0
#define DBG_COM         0
#define DBG_QUEUE       0
#define DBG_QUEUE2      0
#define DBG_INTR        0
#define DBG_INTR2       0
#define DBG_INTR3       0
#define DBG_REQSENSE    0
#define DBG_RESET       0
#define DBG_STATUS      0
#define DBG_PROC        0
#define DBG_PROC_WRITE  0
#define DBG_REGISTER    0
#define DBG_ABNORM      1
#if DEBUG_EATA
#define DBG(x, y)   if ((x)) {y;}
#else
#define DBG(x, y)
#endif
#endif
int eata_detect(Scsi_Host_Template *);
const char *eata_info(struct Scsi_Host *);
int eata_command(Scsi_Cmnd *);
int eata_queue(Scsi_Cmnd *, void (* done)(Scsi_Cmnd *));
int eata_abort(Scsi_Cmnd *);
int eata_reset(Scsi_Cmnd *, unsigned int);
int eata_proc_info(char *, char **, off_t, int, int, int);
#ifdef MODULE
int eata_release(struct Scsi_Host *);
#else
#define eata_release NULL
#endif
#include <scsi/scsicam.h>
#define EATA_DMA {                   \
NULL, NULL,                  \
NULL,                \
eata_proc_info,      \
"EATA (Extended Attachment) HBA driver", \
eata_detect,                 \
eata_release,                \
NULL, NULL,                  \
eata_queue,                  \
eata_abort,                  \
eata_reset,                  \
NULL,      \
scsicam_bios_param,          \
0,         \
0,         \
0,         \
0,         \
0,         \
1,         \
ENABLE_CLUSTERING }
#endif