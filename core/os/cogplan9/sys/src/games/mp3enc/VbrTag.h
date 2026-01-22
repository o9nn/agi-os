#ifndef LAME_VRBTAG_H
#define LAME_VRBTAG_H
#include "lame.h"
#define FRAMES_FLAG 0x0001
#define BYTES_FLAG 0x0002
#define TOC_FLAG 0x0004
#define VBR_SCALE_FLAG 0x0008
#define NUMTOCENTRIES 100
#define FRAMES_AND_BYTES (FRAMES_FLAG | BYTES_FLAG)
typedef struct
{
int h_id;
int samprate;
int flags;
int frames;
int bytes;
int vbr_scale;
unsigned char toc[NUMTOCENTRIES];
int headersize;
} VBRTAGDATA;
int CheckVbrTag(unsigned char *buf);
int GetVbrTag(VBRTAGDATA *pTagData, unsigned char *buf);
int SeekPoint(unsigned char TOC[NUMTOCENTRIES], int file_bytes, float percent);
int InitVbrTag(lame_global_flags *gfp);
int PutVbrTag(lame_global_flags *gfp,FILE *fid,int nVbrScale);
void AddVbrFrame(lame_global_flags *gfp);
#endif