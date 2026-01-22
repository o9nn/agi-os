#ifndef	_DISK_STATUS_H_
#define	_DISK_STATUS_H_
#define LABELSECTOR	0
#define LABELOFFSET	64
#define DISKMAGIC	((unsigned int) 0x82564557U)
#ifndef MAXPARTITIONS
#define	MAXPARTITIONS	8
#endif
#ifndef LOCORE
struct disklabel {
unsigned int	d_magic;
short	d_type;
short	d_subtype;
char	d_typename[16];
#if defined(MACH_KERNEL) || defined(STANDALONE)
char	d_packname[16];
#else
union {
char	un_d_packname[16];
struct {
char *un_d_boot0;
char *un_d_boot1;
} un_b;
} d_un;
#define d_packname	d_un.un_d_packname
#define d_boot0		d_un.un_b.un_d_boot0
#define d_boot1		d_un.un_b.un_d_boot1
#endif
unsigned int	d_secsize;
unsigned int	d_nsectors;
unsigned int	d_ntracks;
unsigned int	d_ncylinders;
unsigned int	d_secpercyl;
unsigned int	d_secperunit;
unsigned short	d_sparespertrack;
unsigned short	d_sparespercyl;
unsigned int	d_acylinders;
unsigned short	d_rpm;
unsigned short	d_interleave;
unsigned short	d_trackskew;
unsigned short	d_cylskew;
unsigned int	d_headswitch;
unsigned int	d_trkseek;
unsigned int	d_flags;
#define NDDATA 5
unsigned int	d_drivedata[NDDATA];
#define NSPARE 5
unsigned int	d_spare[NSPARE];
unsigned int	d_magic2;
unsigned short	d_checksum;
unsigned short	d_npartitions;
unsigned int	d_bbsize;
unsigned int	d_sbsize;
struct	partition {
unsigned int	p_size;
unsigned int	p_offset;
unsigned int	p_fsize;
unsigned char	p_fstype;
unsigned char	p_frag;
unsigned short	p_cpg;
} d_partitions[MAXPARTITIONS+1];
#if	defined(alpha) && defined(MACH_KERNEL)
int	bugfix;
#endif
};
#else
.set	d_secsize,40
.set	d_nsectors,44
.set	d_ntracks,48
.set	d_ncylinders,52
.set	d_secpercyl,56
.set	d_secperunit,60
.set	d_end_,276
#endif
#define	DTYPE_SMD		1
#define	DTYPE_MSCP		2
#define	DTYPE_DEC		3
#define	DTYPE_SCSI		4
#define	DTYPE_ESDI		5
#define	DTYPE_ST506		6
#define	DTYPE_FLOPPY		10
#ifdef DKTYPENAMES
static char *dktypenames[] = {
"unknown",
"SMD",
"MSCP",
"old DEC",
"SCSI",
"ESDI",
"type 6",
"type 7",
"type 8",
"type 9",
"floppy",
0
};
#define DKMAXTYPES	(sizeof(dktypenames) / sizeof(dktypenames[0]) - 1)
#endif
#define	FS_UNUSED	0
#define	FS_SWAP		1
#define	FS_V6		2
#define	FS_V7		3
#define	FS_SYSV		4
#define	FS_V71K		5
#define	FS_V8		6
#define	FS_BSDFFS	7
#define FS_LINUXFS	8
#ifdef	DKTYPENAMES
static char *fstypenames[] = {
"unused",
"swap",
"Version 6",
"Version 7",
"System V",
"4.1BSD",
"Eighth Edition",
"4.2BSD",
"Linux",
0
};
#define FSMAXTYPES	(sizeof(fstypenames) / sizeof(fstypenames[0]) - 1)
#endif
#define		D_REMOVABLE	0x01
#define		D_ECC		0x02
#define		D_BADSECT	0x04
#define		D_RAMDISK	0x08
#define		D_CHAIN		0x10
#define	d_smdflags	d_drivedata[0]
#define		D_SSE		0x1
#define	d_mindist	d_drivedata[1]
#define	d_maxdist	d_drivedata[2]
#define	d_sdist		d_drivedata[3]
#define d_precompcyl	d_drivedata[0]
#define d_gap3		d_drivedata[1]
#define	d_step		d_drivedata[2]
#ifndef LOCORE
struct format_op {
char	*df_buf;
int	df_count;
recnum_t	df_startblk;
int	df_reg[8];
};
#define DIOCGDINFO	_IOR('d', 101, struct disklabel)
#define DIOCSDINFO	_IOW('d', 102, struct disklabel)
#define DIOCWDINFO	_IOW('d', 103, struct disklabel)
#define DIOCRFORMAT	_IOWR('d', 105, struct format_op)
#define DIOCWFORMAT	_IOWR('d', 106, struct format_op)
#define DIOCSSTEP	_IOW('d', 107, int)
#define DIOCSRETRIES	_IOW('d', 108, int)
#define DIOCWLABEL	_IOW('d', 109, int)
#define DIOCSBAD	_IOW('d', 110, struct dkbad)
#endif
#endif