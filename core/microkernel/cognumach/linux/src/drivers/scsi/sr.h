#ifndef _SR_H
#define _SR_H
#include "scsi.h"
typedef struct
{
unsigned 	capacity;
unsigned 	sector_size;
Scsi_Device  	*device;
unsigned long   mpcd_sector;
char            xa_flags;
unsigned char	sector_bit_size;
unsigned char	sector_bit_shift;
unsigned 	needs_sector_size:1;
unsigned 	ten:1;
unsigned 	remap:1;
unsigned 	use:1;
unsigned	auto_eject:1;
} Scsi_CD;
extern Scsi_CD * scsi_CDs;
#endif