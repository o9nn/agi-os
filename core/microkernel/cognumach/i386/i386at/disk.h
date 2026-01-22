#ifndef _DISK_H_
#define _DISK_H_
#define V_NUMPAR 16
#define VTOC_SANE 0x600DDEEE
#define PDLOCATION 29
#define LBLLOC 1
struct localpartition {
u_int p_flag;
long p_start;
long p_size;
};
typedef struct localpartition localpartition_t;
struct evtoc {
u_int fill0[6];
u_int cyls;
u_int tracks;
u_int sectors;
u_int fill1[13];
u_int version;
u_int alt_ptr;
u_short alt_len;
u_int sanity;
u_int xcyls;
u_int xtracks;
u_int xsectors;
u_short nparts;
u_short fill2;
char label[40];
struct localpartition part[V_NUMPAR];
char fill[512-352];
};
#endif