#ifndef _VTX_H
#define _VTX_H
#define VTXIOCGETINFO  0x7101
#define VTXIOCCLRPAGE  0x7102
#define VTXIOCCLRFOUND 0x7103
#define VTXIOCPAGEREQ  0x7104
#define VTXIOCGETSTAT  0x7105
#define VTXIOCGETPAGE  0x7106
#define VTXIOCSTOPDAU  0x7107
#define VTXIOCPUTPAGE  0x7108
#define VTXIOCSETDISP  0x7109
#define VTXIOCPUTSTAT  0x710a
#define VTXIOCCLRCACHE 0x710b
#define VTXIOCSETVIRT  0x710c
#define SAA5243 0
#define SAA5246 1
#define SAA5249 2
#define SAA5248 3
#define XSTV5346 4
typedef struct {
int version_major, version_minor;
int numpages;
int cct_type;
}
vtx_info_t;
#define MIN_UNIT   (1<<0)
#define MIN_TEN    (1<<1)
#define HR_UNIT    (1<<2)
#define HR_TEN     (1<<3)
#define PG_UNIT    (1<<4)
#define PG_TEN     (1<<5)
#define PG_HUND    (1<<6)
#define PGMASK_MAX (1<<7)
#define PGMASK_PAGE (PG_HUND | PG_TEN | PG_UNIT)
#define PGMASK_HOUR (HR_TEN | HR_UNIT)
#define PGMASK_MINUTE (MIN_TEN | MIN_UNIT)
typedef struct
{
int page;
int hour;
int minute;
int pagemask;
int pgbuf;
int start;
int end;
void *buffer;
}
vtx_pagereq_t;
#define VTX_PAGESIZE (40 * 24)
#define VTX_VIRTUALSIZE (40 * 49)
typedef struct
{
int pagenum;
int hour;
int minute;
int charset;
unsigned delete : 1;
unsigned headline : 1;
unsigned subtitle : 1;
unsigned supp_header : 1;
unsigned update : 1;
unsigned inter_seq : 1;
unsigned dis_disp : 1;
unsigned serial : 1;
unsigned notfound : 1;
unsigned pblf : 1;
unsigned hamming : 1;
}
vtx_pageinfo_t;
typedef enum {
DISPOFF, DISPNORM, DISPTRANS, DISPINS, INTERLACE_OFFSET
} vtxdisp_t;
#define TUNIOCGETINFO  0x7201
#define TUNIOCRESET    0x7202
#define TUNIOCSETFREQ  0x7203
#define TUNIOCGETFREQ  0x7204
#define TUNIOCSETCHAN  0x7205
#define TUNIOCGETCHAN  0x7206
typedef struct
{
int version_major, version_minor;
unsigned freq : 1;
unsigned chan : 1;
unsigned scan : 1;
unsigned autoscan : 1;
unsigned afc : 1;
unsigned dummy1, dummy2, dummy3, dummy4, dummy5, dummy6, dummy7, dummy8, dummy9, dummy10,
dummy11 : 1;
int dummy12, dummy13, dummy14, dummy15, dummy16, dummy17, dummy18, dummy19;
} tuner_info_t;
#endif