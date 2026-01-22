#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include "machine.h"
#if defined(__riscos__) && defined(FPA10)
#include	"ymath.h"
#else
#include	<math.h>
#endif
#include "VbrTag.h"
#include "version.h"
#include "bitstream.h"
#include "VbrTag.h"
#include	<assert.h>
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
#ifdef _DEBUG
#endif
#define VBRHEADERSIZE (NUMTOCENTRIES+4+4+4+4+4)
#define XING_BITRATE1 128
#define XING_BITRATE2  64
#define XING_BITRATE25 32
const static char	VBRTag[]={"Xing"};
const int SizeOfEmptyFrame[2][2]=
{
{17,9},
{32,17},
};
void addVbr(VBR_seek_info_t * v, int bitrate)
{
int i;
v->sum += bitrate;
v->seen ++;
if (v->seen < v->want) {
return;
}
if (v->pos < v->size) {
v->bag[v->pos] = v->sum;
v->pos ++;
v->seen = 0;
}
if (v->pos == v->size) {
for (i = 1; i < v->size; i += 2) {
v->bag[i/2] = v->bag[i];
}
v->want *= 2;
v->pos  /= 2;
}
}
void Xing_seek_table(VBR_seek_info_t * v, unsigned char *t)
{
int i, index;
int seek_point;
if (v->pos <= 0)
return;
for (i = 1; i < NUMTOCENTRIES; ++i) {
float j = i/(float)NUMTOCENTRIES, act, sum;
index = (int)(floor(j * v->pos));
if (index > v->pos-1)
index = v->pos-1;
act = v->bag[index];
sum = v->sum;
seek_point = (int)(256. * act / sum);
if (seek_point > 255)
seek_point = 255;
t[i] = seek_point;
}
}
#if 0
void print_seeking(unsigned char *t)
{
int i;
printf("seeking table ");
for (i = 0; i < NUMTOCENTRIES; ++i) {
printf(" %d ", t[i]);
}
printf("\n");
}
#endif
void AddVbrFrame(lame_global_flags *gfp)
{
lame_internal_flags *gfc = gfp->internal_flags;
int kbps = bitrate_table[gfp->version][gfc->bitrate_index];
if (gfc->VBR_seek_table.bag == NULL) {
gfc->VBR_seek_table.sum  = 0;
gfc->VBR_seek_table.seen = 0;
gfc->VBR_seek_table.want = 1;
gfc->VBR_seek_table.pos  = 0;
gfc->VBR_seek_table.bag  = malloc (400*sizeof(int));
if (gfc->VBR_seek_table.bag != NULL) {
gfc->VBR_seek_table.size = 400;
}
else {
gfc->VBR_seek_table.size = 0;
ERRORF (gfc,"Error: can't allocate VbrFrames buffer\n");
return;
}
}
addVbr(&gfc->VBR_seek_table, kbps);
gfp->nVbrNumFrames++;
}
static int ExtractI4(unsigned char *buf)
{
int x;
x = buf[0];
x <<= 8;
x |= buf[1];
x <<= 8;
x |= buf[2];
x <<= 8;
x |= buf[3];
return x;
}
void CreateI4(unsigned char *buf, int nValue)
{
buf[0]=(nValue>>24)&0xff;
buf[1]=(nValue>>16)&0xff;
buf[2]=(nValue>> 8)&0xff;
buf[3]=(nValue    )&0xff;
}
int CheckVbrTag(unsigned char *buf)
{
int			h_id, h_mode, h_sr_index;
h_id       = (buf[1] >> 3) & 1;
h_sr_index = (buf[2] >> 2) & 3;
h_mode     = (buf[3] >> 6) & 3;
if( h_id )
{
if( h_mode != 3 )	buf+=(32+4);
else				buf+=(17+4);
}
else
{
if( h_mode != 3 ) buf+=(17+4);
else              buf+=(9+4);
}
if( buf[0] != VBRTag[0] ) return 0;
if( buf[1] != VBRTag[1] ) return 0;
if( buf[2] != VBRTag[2] ) return 0;
if( buf[3] != VBRTag[3] ) return 0;
return 1;
}
int GetVbrTag(VBRTAGDATA *pTagData,  unsigned char *buf)
{
int			i, head_flags;
int			h_bitrate,h_id, h_mode, h_sr_index;
pTagData->flags = 0;
h_id       = (buf[1] >> 3) & 1;
h_sr_index = (buf[2] >> 2) & 3;
h_mode     = (buf[3] >> 6) & 3;
h_bitrate  = ((buf[2]>>4)&0xf);
h_bitrate = bitrate_table[h_id][h_bitrate];
if( h_id )
{
if( h_mode != 3 )	buf+=(32+4);
else				buf+=(17+4);
}
else
{
if( h_mode != 3 ) buf+=(17+4);
else              buf+=(9+4);
}
if( buf[0] != VBRTag[0] ) return 0;
if( buf[1] != VBRTag[1] ) return 0;
if( buf[2] != VBRTag[2] ) return 0;
if( buf[3] != VBRTag[3] ) return 0;
buf+=4;
pTagData->h_id = h_id;
pTagData->samprate = samplerate_table[h_id][h_sr_index];
if( h_id == 0 )
pTagData->samprate >>= 1;
head_flags = pTagData->flags = ExtractI4(buf); buf+=4;
if( head_flags & FRAMES_FLAG )
{
pTagData->frames   = ExtractI4(buf); buf+=4;
}
if( head_flags & BYTES_FLAG )
{
pTagData->bytes = ExtractI4(buf); buf+=4;
}
if( head_flags & TOC_FLAG )
{
if( pTagData->toc != NULL )
{
for(i=0;i<NUMTOCENTRIES;i++)
pTagData->toc[i] = buf[i];
}
buf+=NUMTOCENTRIES;
}
pTagData->vbr_scale = -1;
if( head_flags & VBR_SCALE_FLAG )
{
pTagData->vbr_scale = ExtractI4(buf); buf+=4;
}
pTagData->headersize =
((h_id+1)*72000*h_bitrate) / pTagData->samprate;
#ifdef DEBUG_VBRTAG
DEBUGF("\n\n********************* VBR TAG INFO *****************\n");
DEBUGF("tag         :%s\n",VBRTag);
DEBUGF("head_flags  :%d\n",head_flags);
DEBUGF("bytes       :%d\n",pTagData->bytes);
DEBUGF("frames      :%d\n",pTagData->frames);
DEBUGF("VBR Scale   :%d\n",pTagData->vbr_scale);
DEBUGF("toc:\n");
if( pTagData->toc != NULL )
{
for(i=0;i<NUMTOCENTRIES;i++)
{
if( (i%10) == 0 ) DEBUGF("\n");
DEBUGF(" %3d", (int)(pTagData->toc[i]));
}
}
DEBUGF("\n***************** END OF VBR TAG INFO ***************\n");
#endif
return 1;
}
int InitVbrTag(lame_global_flags *gfp)
{
int nMode,SampIndex;
lame_internal_flags *gfc = gfp->internal_flags;
#define MAXFRAMESIZE 576
nMode = gfp->mode;
SampIndex = gfc->samplerate_index;
gfp->pVbrFrames=NULL;
gfp->nVbrNumFrames=0;
gfp->nVbrFrameBufferSize=0;
if (nMode==3)
{
gfp->nZeroStreamSize=SizeOfEmptyFrame[gfp->version][1]+4;
}
else
{
gfp->nZeroStreamSize=SizeOfEmptyFrame[gfp->version][0]+4;
}
{
int i,bitrate,tot;
if (1==gfp->version) {
bitrate = XING_BITRATE1;
} else {
if (gfp->out_samplerate < 16000 )
bitrate = XING_BITRATE25;
else
bitrate = XING_BITRATE2;
}
gfp->TotalFrameSize=
((gfp->version+1)*72000*bitrate) / gfp->out_samplerate;
tot = (gfp->nZeroStreamSize+VBRHEADERSIZE);
tot += 20;
assert(gfp->TotalFrameSize >= tot );
assert(gfp->TotalFrameSize <= MAXFRAMESIZE );
for (i=0; i<gfp->TotalFrameSize; ++i)
add_dummy_byte(gfp,0);
}
return 0;
}
int PutVbrTag(lame_global_flags *gfp,FILE *fpStream,int nVbrScale)
{
lame_internal_flags * gfc = gfp->internal_flags;
long lFileSize;
int nStreamIndex;
char abyte,bbyte;
u_char		btToc[NUMTOCENTRIES];
u_char pbtStreamBuffer[MAXFRAMESIZE];
char str1[80];
unsigned char id3v2Header[10];
size_t id3v2TagSize;
if (gfc->VBR_seek_table.pos <= 0)
return -1;
memset(pbtStreamBuffer,0x00,sizeof(pbtStreamBuffer));
fseek(fpStream,0,SEEK_END);
lFileSize=ftell(fpStream);
if (lFileSize==0)
return -1;
fseek(fpStream,0,SEEK_SET);
fread(id3v2Header,1,sizeof id3v2Header,fpStream);
if (!strncmp((char *)id3v2Header,"ID3",3)) {
id3v2TagSize=(((id3v2Header[6] & 0x7f)<<21)
| ((id3v2Header[7] & 0x7f)<<14)
| ((id3v2Header[8] & 0x7f)<<7)
| (id3v2Header[9] & 0x7f))
+ sizeof id3v2Header;
} else {
id3v2TagSize=0;
}
fseek(fpStream,id3v2TagSize+gfp->TotalFrameSize,SEEK_SET);
fread(pbtStreamBuffer,4,1,fpStream);
pbtStreamBuffer[0]=(u_char) 0xff;
abyte = (pbtStreamBuffer[1] & (char) 0xf1);
{ int bitrate;
if (1==gfp->version) {
bitrate = XING_BITRATE1;
} else {
if (gfp->out_samplerate < 16000 )
bitrate = XING_BITRATE25;
else
bitrate = XING_BITRATE2;
}
bbyte = 16*BitrateIndex(bitrate,gfp->version,gfp->out_samplerate);
}
if (gfp->version==1) {
pbtStreamBuffer[1]=abyte | (char) 0x0a;
abyte = pbtStreamBuffer[2] & (char) 0x0d;
pbtStreamBuffer[2]=(char) bbyte | abyte;
}else{
pbtStreamBuffer[1]=abyte | (char) 0x02;
abyte = pbtStreamBuffer[2] & (char) 0x0d;
pbtStreamBuffer[2]=(char) bbyte | abyte;
}
fseek(fpStream,id3v2TagSize,SEEK_SET);
memset(btToc,0,sizeof(btToc));
Xing_seek_table (&gfc->VBR_seek_table, btToc);
nStreamIndex=gfp->nZeroStreamSize;
pbtStreamBuffer[nStreamIndex++]=VBRTag[0];
pbtStreamBuffer[nStreamIndex++]=VBRTag[1];
pbtStreamBuffer[nStreamIndex++]=VBRTag[2];
pbtStreamBuffer[nStreamIndex++]=VBRTag[3];
CreateI4(&pbtStreamBuffer[nStreamIndex],FRAMES_FLAG+BYTES_FLAG+TOC_FLAG+VBR_SCALE_FLAG);
nStreamIndex+=4;
CreateI4(&pbtStreamBuffer[nStreamIndex],gfp->nVbrNumFrames);
nStreamIndex+=4;
CreateI4(&pbtStreamBuffer[nStreamIndex],(int)lFileSize);
nStreamIndex+=4;
memcpy(&pbtStreamBuffer[nStreamIndex],btToc,sizeof(btToc));
nStreamIndex+=sizeof(btToc);
CreateI4(&pbtStreamBuffer[nStreamIndex],nVbrScale);
nStreamIndex+=4;
sprintf ( str1, "LAME%s", get_lame_short_version () );
strncpy ( (char*)pbtStreamBuffer + nStreamIndex, str1, 20 );
nStreamIndex += 20;
#ifdef DEBUG_VBRTAG
{
VBRTAGDATA TestHeader;
GetVbrTag(&TestHeader,pbtStreamBuffer);
}
#endif
if (fwrite(pbtStreamBuffer,(unsigned int)gfp->TotalFrameSize,1,fpStream)!=1)
{
return -1;
}
free(gfp->pVbrFrames);
gfp->pVbrFrames=NULL;
return 0;
}