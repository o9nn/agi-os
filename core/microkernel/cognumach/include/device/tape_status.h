#ifndef _TAPE_STATUS_H_
#define _TAPE_STATUS_H_
struct tape_status {
unsigned int mt_type;
unsigned int speed;
unsigned int density;
unsigned int flags;
# define TAPE_FLG_REWIND 0x1
# define TAPE_FLG_WP 0x2
};
#define TAPE_STATUS_COUNT (sizeof(struct tape_status)/sizeof(int))
#define TAPE_STATUS (('m'<<16) + 1)
#define MT_ISTS 0x01
#define MT_ISHT 0x02
#define MT_ISTM 0x03
#define MT_ISMT 0x04
#define MT_ISUT 0x05
#define MT_ISCPC 0x06
#define MT_ISAR 0x07
#define MT_ISTMSCP 0x08
#define MT_ISCY 0x09
#define MT_ISSCSI 0x0a
struct tape_params {
unsigned int mt_operation;
unsigned int mt_repeat_count;
};
#define MTWEOF 0
#define MTFSF 1
#define MTBSF 2
#define MTFSR 3
#define MTBSR 4
#define MTREW 5
#define MTOFFL 6
#define MTNOP 7
#define MTCACHE 8
#define MTNOCACHE 9
struct mtget {
short mt_type;
short mt_dsreg;
short mt_erreg;
short mt_resid;
unsigned long mt_fileno;
unsigned long mt_blkno;
};
#define MTIOCTOP _IOW('m', 1, struct tape_params)
#define MTIOCGET _IOR('m', 2, struct mtget)
#define MTIOCIEOT _IO('m', 3)
#define MTIOCEEOT _IO('m', 4)
#endif