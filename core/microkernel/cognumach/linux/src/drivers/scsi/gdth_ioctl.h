#ifndef _GDTH_IOCTL_H
#define _GDTH_IOCTL_H
#define GDTIOCTL_MASK ('J'<<8)
#define GDTIOCTL_GENERAL (GDTIOCTL_MASK | 0)
#define GDTIOCTL_DRVERS (GDTIOCTL_MASK | 1)
#define GDTIOCTL_CTRTYPE (GDTIOCTL_MASK | 2)
#define GDTIOCTL_OSVERS (GDTIOCTL_MASK | 3)
#define GDTIOCTL_CTRCNT (GDTIOCTL_MASK | 5)
#define GDTIOCTL_LOCKDRV (GDTIOCTL_MASK | 6)
#define GDTIOCTL_LOCKCHN (GDTIOCTL_MASK | 7)
#define GDTIOCTL_EVENT (GDTIOCTL_MASK | 8)
#define GDTIOCTL_MAGIC 0x06030f07UL
typedef struct {
ulong magic;
ushort ioctl;
ushort ionode;
ushort service;
ushort timeout;
union {
struct {
unchar command[512];
unchar data[1];
} general;
struct {
unchar lock;
unchar drive_cnt;
ushort drives[35];
} lockdrv;
struct {
unchar lock;
unchar channel;
} lockchn;
struct {
int erase;
int handle;
} event;
} iu;
} gdth_iowr_str;
typedef struct {
ulong size;
ulong status;
union {
struct {
unchar data[1];
} general;
struct {
ushort version;
} drvers;
struct {
unchar type;
ushort info;
ushort oem_id;
ushort bios_ver;
ushort access;
ushort ext_type;
} ctrtype;
struct {
unchar version;
unchar subversion;
ushort revision;
} osvers;
struct {
ushort count;
} ctrcnt;
struct {
int handle;
unchar evt[32];
} event;
} iu;
} gdth_iord_str;
#endif