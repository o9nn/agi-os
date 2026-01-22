#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#ifdef HAVE_VORBIS
#include <stdlib.h>
#include <limits.h>
#include <time.h>
#include "vorbis/codec.h"
#include "modes/modes.h"
#include "lame.h"
#include "util.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
short int  convbuffer [4096];
int        convsize;
ogg_sync_state    oy;
ogg_stream_state  os;
ogg_page          og;
ogg_packet        op;
vorbis_info       vi;
vorbis_comment    vc;
vorbis_dsp_state  vd;
vorbis_block      vb;
int lame_decode_ogg_initfile( lame_global_flags*  gfp,
FILE*               fd,
mp3data_struct*     mp3data )
{
lame_internal_flags *gfc = gfp->internal_flags;
char *buffer;
int  bytes;
int i;
ogg_sync_init(&oy);
buffer=ogg_sync_buffer(&oy,4096);
bytes=fread(buffer,1,4096,fd);
ogg_sync_wrote(&oy,bytes);
if(ogg_sync_pageout(&oy,&og)!=1){
ERRORF( gfc, "Error initializing Ogg bitstream data.\n" );
return -1;
}
ogg_stream_init(&os,ogg_page_serialno(&og));
vorbis_info_init(&vi);
vorbis_comment_init(&vc);
if(ogg_stream_pagein(&os,&og)<0){
ERRORF( gfc, "Error reading first page of Ogg bitstream data.\n" );
return -1;
}
if(ogg_stream_packetout(&os,&op)!=1){
ERRORF( gfc, "Error reading initial header packet.\n" );
return -1;
}
if(vorbis_synthesis_headerin(&vi,&vc,&op)<0){
ERRORF( gfc, "This Ogg bitstream does not contain Vorbis "
"audio data.\n");
return -1;
}
i=0;
while(i<2){
while(i<2){
int result=ogg_sync_pageout(&oy,&og);
if(result==0)break;
if(result==1){
ogg_stream_pagein(&os,&og);
while(i<2){
result=ogg_stream_packetout(&os,&op);
if(result==0)break;
if(result==-1){
ERRORF( gfc, "Corrupt secondary header.  Exiting.\n" );
return -1;
}
vorbis_synthesis_headerin(&vi,&vc,&op);
i++;
}
}
}
buffer=ogg_sync_buffer(&oy,4096);
bytes=fread(buffer,1,4096,fd);
if(bytes==0 && i<2){
ERRORF( gfc, "End of file before finding all Vorbis headers!\n" );
return -1;
}
ogg_sync_wrote(&oy,bytes);
}
{
}
vorbis_synthesis_init(&vd,&vi);
vorbis_block_init(&vd,&vb);
mp3data->stereo = vi.channels;
mp3data->samplerate = vi.rate;
mp3data->bitrate = 0;
mp3data->nsamp=MAX_U_32_NUM;
return 0;
}
int lame_decode_ogg_fromfile( lame_global_flags*  gfp,
FILE*               fd,
short int           pcm_l[],
short int           pcm_r[],
mp3data_struct*     mp3data )
{
lame_internal_flags *gfc = gfp->internal_flags;
int samples,result,i,j,eof=0,eos=0,bout=0;
double **pcm;
while(1){
convsize=1024;
samples=vorbis_synthesis_pcmout(&vd,&pcm);
if (samples >= convsize || eos || eof) {
int clipflag=0;
bout=(samples<convsize?samples:convsize);
for(i=0;i<vi.channels;i++){
double  *mono=pcm[i];
for(j=0;j<bout;j++){
int val=mono[j]*32767.;
if(val>32767){
val=32767;
clipflag=1;
}
if(val<-32768){
val=-32768;
clipflag=1;
}
if (i==0) pcm_l[j]=val;
if (i==1) pcm_r[j]=val;
}
}
vorbis_synthesis_read(&vd,bout);
break;
}
result=ogg_sync_pageout(&oy,&og);
if(result==0) {
}else if (result==-1){
ERRORF( gfc, "Corrupt or missing data in bitstream; "
"continuing...\n");
}else{
ogg_stream_pagein(&os,&og);
do {
result=ogg_stream_packetout(&os,&op);
if(result==0) {
} else if(result==-1){
}else{
vorbis_synthesis(&vb,&op);
vorbis_synthesis_blockin(&vd,&vb);
}
} while (result!=0);
}
if(ogg_page_eos(&og))eos=1;
if(!eos){
char *buffer;
int bytes;
buffer=ogg_sync_buffer(&oy,4096);
bytes=fread(buffer,1,4096,fd);
ogg_sync_wrote(&oy,bytes);
if(bytes==0)eof=1;
}
}
mp3data->stereo = vi.channels;
mp3data->samplerate = vi.rate;
mp3data->bitrate = 0;
if (bout==0) {
ogg_stream_clear(&os);
vorbis_block_clear(&vb);
vorbis_dsp_clear(&vd);
vorbis_info_clear(&vi);
ogg_sync_clear(&oy);
return -1;
}
return bout;
}
ogg_stream_state  os2;
ogg_page          og2;
ogg_packet        op2;
vorbis_info       vi2;
vorbis_comment    vc2;
vorbis_dsp_state  vd2;
vorbis_block      vb2;
#define MAX_COMMENT_LENGTH 255
int lame_encode_ogg_init(lame_global_flags *gfp)
{
lame_internal_flags *gfc=gfp->internal_flags;
char comment[MAX_COMMENT_LENGTH+1];
if (gfp->compression_ratio < 5.01) {
memcpy(&vi2,&info_E,sizeof(vi2));
MSGF( gfc, "Encoding with Vorbis mode info_E \n" );
} else if (gfp->compression_ratio < 6) {
memcpy(&vi2,&info_D,sizeof(vi2));
MSGF( gfc, "Encoding with Vorbis mode info_D \n" );
} else if (gfp->compression_ratio < 8) {
memcpy(&vi2,&info_C,sizeof(vi2));
MSGF( gfc, "Encoding with Vorbis mode info_C \n" );
} else if (gfp->compression_ratio < 10) {
memcpy(&vi2,&info_B,sizeof(vi2));
MSGF( gfc, "Encoding with Vorbis mode info_B \n" );
} else if (gfp->compression_ratio < 12) {
memcpy(&vi2,&info_A,sizeof(vi2));
MSGF( gfc, "Encoding with Vorbis mode info_A \n" );
} else {
memcpy(&vi2,&info_A,sizeof(vi2));
MSGF( gfc, "Encoding with Vorbis mode info_A \n" );
}
vi2.channels = gfc->channels_out;
vi2.rate = gfp->out_samplerate;
vorbis_comment_init(&vc2);
vorbis_comment_add(&vc2,"Track encoded using L.A.M.E. libvorbis interface.");
#ifdef THIS_CODE_IS_NOT_BROKEN_ANYMORE
if(gfp->tag_spec.title) {
strcpy(comment,"TITLE=");
strncat(comment,gfp->tag_spec.title,MAX_COMMENT_LENGTH-strlen(comment));
vorbis_comment_add(&vc2,comment);
}
if(gfp->tag_spec.artist) {
strcpy(comment,"ARTIST=");
strncat(comment,gfp->tag_spec.artist,MAX_COMMENT_LENGTH-strlen(comment));
vorbis_comment_add(&vc2,comment);
}
if(gfp->tag_spec.album) {
strcpy(comment,"ALBUM=");
strncat(comment,gfp->tag_spec.album,MAX_COMMENT_LENGTH-strlen(comment));
vorbis_comment_add(&vc2,comment);
}
if(gfp->tag_spec.year) {
sprintf(comment, "DATE=%d", gfp->tag_spec.year);
vorbis_comment_add(&vc2,comment);
}
if(gfp->tag_spec.comment) {
strcpy(comment,"DESCRIPTION=");
strncat(comment,gfp->tag_spec.comment,MAX_COMMENT_LENGTH-strlen(comment));
vorbis_comment_add(&vc2,comment);
}
#endif
vorbis_analysis_init(&vd2,&vi2);
vorbis_block_init(&vd2,&vb2);
srand(time(NULL));
ogg_stream_init(&os2,rand());
{
ogg_packet header;
ogg_packet header_comm;
ogg_packet header_code;
vorbis_analysis_headerout(&vd2,&vc2,&header,&header_comm,&header_code);
ogg_stream_packetin(&os2,&header);
ogg_stream_packetin(&os2,&header_comm);
ogg_stream_packetin(&os2,&header_code);
}
return 0;
}
int lame_encode_ogg_finish(lame_global_flags *gfp,
char *mp3buf, int mp3buf_size)
{
int eos=0,bytes=0;
vorbis_analysis_wrote(&vd2,0);
while(vorbis_analysis_blockout(&vd2,&vb2)==1){
vorbis_analysis(&vb2,&op2);
ogg_stream_packetin(&os2,&op2);
while(!eos){
int result=ogg_stream_pageout(&os2,&og2);
if(result==0)break;
bytes += og2.header_len + og2.body_len;
if (bytes > mp3buf_size && mp3buf_size>0)
return -5;
memcpy(mp3buf,og2.header,og2.header_len);
memcpy(mp3buf+og2.header_len,og2.body,og2.body_len);
if(ogg_page_eos(&og2))eos=1;
}
}
ogg_stream_clear(&os2);
vorbis_block_clear(&vb2);
vorbis_dsp_clear(&vd2);
return bytes;
}
int  lame_encode_ogg_frame (
lame_global_flags*  gfp,
const sample_t*     inbuf_l,
const sample_t*     inbuf_r,
unsigned char*      mp3buf,
size_t              mp3buf_size )
{
lame_internal_flags *gfc = gfp->internal_flags;
int  i;
int  eos   = 0;
int  bytes = 0;
double **buffer = vorbis_analysis_buffer(&vd2,gfp->framesize);
for ( i = 0; i < gfp->framesize; i++ )
buffer [0] [i] = (1/32768.) * inbuf_l [i];
if ( gfc->channels_out == 2 )
for ( i = 0; i < gfp->framesize; i++ )
buffer [1] [i] = (1/32768.) * inbuf_r [i];
vorbis_analysis_wrote(&vd2,i);
while(vorbis_analysis_blockout(&vd2,&vb2)==1){
int result;
vorbis_analysis(&vb2,&op2);
ogg_stream_packetin(&os2,&op2);
do {
result=ogg_stream_pageout(&os2,&og2);
if (result==0) break;
bytes += og2.header_len + og2.body_len;
if (bytes > mp3buf_size && mp3buf_size>0)
return -6;
memcpy(mp3buf,og2.header,og2.header_len);
memcpy(mp3buf+og2.header_len,og2.body,og2.body_len);
mp3buf += og2.header_len + og2.body_len;
if(ogg_page_eos(&og2))eos=1;
} while (1);
}
(gfp -> frameNum)++;
return bytes;
}
#endif