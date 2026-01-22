#ifdef HAVE_CONFIG_H
#include <config.h>
#endif
#include <assert.h>
#include "lame.h"
#include "util.h"
#include "newmdct.h"
#include "psymodel.h"
#include "quantize.h"
#include "quantize_pvt.h"
#include "bitstream.h"
#include "VbrTag.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
void
adjust_ATH( lame_global_flags* const  gfp,
FLOAT8              tot_ener[2][4] )
{
lame_internal_flags* const  gfc = gfp->internal_flags;
int gr, channel;
if (gfc->ATH->use_adjust) {
FLOAT8 max_val = 0;
for ( gr = 0; gr < gfc->mode_gr; ++gr )
for ( channel = 0; channel < gfc->channels_out; ++channel )
max_val = Max( max_val, tot_ener[gr][channel] );
max_val *= 32767/1e13;
if (vbr_mtrh == gfp->VBR) {
FLOAT8
x = Max (640, 320*(int)(max_val/320));
x = x/32768;
gfc->ATH->adjust *= gfc->ATH->decay;
if (gfc->ATH->adjust < x)
gfc->ATH->adjust = x;
}
else {
#ifdef OLD_ATH_AUTO_ADJUST
if      (0.5 < max_val / 32768) {
gfc->ATH->adjust = 1.0;
}
else if (0.3 < max_val / 32768) {
gfc->ATH->adjust *= 0.955;
if (gfc->ATH->adjust < 0.3)
gfc->ATH->adjust = 0.3;
}
else {
gfc->ATH->adjust *= 0.93;
if (gfc->ATH->adjust < 0.01)
gfc->ATH->adjust = 0.01;
}
#else
FLOAT8 max_val_n = max_val / 32768;
FLOAT8 adj_lim_new;
if( max_val_n > 0.25) {
if( gfc->ATH->adjust >= 1.0) {
gfc->ATH->adjust = 1.0;
} else {
if( gfc->ATH->adjust < gfc->ATH->adjust_limit) {
gfc->ATH->adjust = gfc->ATH->adjust_limit;
}
}
gfc->ATH->adjust_limit = 1.0;
} else {
adj_lim_new = 15.84 * (max_val_n * max_val_n) + 0.01;
if( gfc->ATH->adjust >= adj_lim_new) {
gfc->ATH->adjust *= adj_lim_new * 0.075 + 0.925;
if( gfc->ATH->adjust < adj_lim_new) {
gfc->ATH->adjust = adj_lim_new;
}
} else {
if( gfc->ATH->adjust_limit >= adj_lim_new) {
gfc->ATH->adjust = adj_lim_new;
} else {
if( gfc->ATH->adjust < gfc->ATH->adjust_limit) {
gfc->ATH->adjust = gfc->ATH->adjust_limit;
}
}
}
gfc->ATH->adjust_limit = adj_lim_new;
}
#endif
}
}
}
typedef FLOAT8 chgrdata[2][2];
int  lame_encode_mp3_frame (
lame_global_flags* const  gfp,
sample_t*                 inbuf_l,
sample_t*                 inbuf_r,
unsigned char*            mp3buf,
int                    mp3buf_size )
{
#ifdef macintosh
static FLOAT8 xr[2][2][576];
static int l3_enc[2][2][576];
#else
FLOAT8 xr[2][2][576];
int l3_enc[2][2][576];
#endif
int mp3count;
III_psy_ratio masking_LR[2][2];
III_psy_ratio masking_MS[2][2];
III_psy_ratio (*masking)[2][2];
III_scalefac_t scalefac[2][2];
const sample_t *inbuf[2];
lame_internal_flags *gfc=gfp->internal_flags;
FLOAT8 tot_ener[2][4];
FLOAT8 ms_ener_ratio[2]={.5,.5};
chgrdata pe,pe_MS;
chgrdata *pe_use;
int ch,gr,mean_bits;
int bitsPerFrame;
int check_ms_stereo;
FLOAT8 ms_ratio_next = 0.;
FLOAT8 ms_ratio_prev = 0.;
memset((char *) masking_LR, 0, sizeof(masking_LR));
memset((char *) masking_MS, 0, sizeof(masking_MS));
memset((char *) scalefac, 0, sizeof(scalefac));
inbuf[0]=inbuf_l;
inbuf[1]=inbuf_r;
check_ms_stereo =  (gfp->mode == JOINT_STEREO);
gfc->mode_ext = MPG_MD_LR_LR;
if (gfc->lame_encode_frame_init==0 )  {
gfc->lame_encode_frame_init=1;
gfc->frac_SpF = ((gfp->version+1)*72000L*gfp->brate) % gfp->out_samplerate;
gfc->slot_lag  = gfc->frac_SpF;
#if 576 < FFTOFFSET
# error FFTOFFSET greater than 576: FFT uses a negative offset
#endif
assert(gfc->mf_size>=(BLKSIZE+gfp->framesize-FFTOFFSET));
assert(gfc->mf_size>=(286+576*(1+gfc->mode_gr)));
{
int i,j;
sample_t primebuff0[286+1152+576];
sample_t primebuff1[286+1152+576];
for (i=0, j=0; i<286+576*(1+gfc->mode_gr); ++i) {
if (i<576*gfc->mode_gr) {
primebuff0[i]=0;
if (gfc->channels_out==2)
primebuff1[i]=0;
}else{
primebuff0[i]=inbuf[0][j];
if (gfc->channels_out==2)
primebuff1[i]=inbuf[1][j];
++j;
}
}
for ( gr = 0; gr < gfc->mode_gr; gr++ ) {
for ( ch = 0; ch < gfc->channels_out; ch++ ) {
gfc->l3_side.gr[gr].ch[ch].tt.block_type=SHORT_TYPE;
}
}
mdct_sub48(gfc, primebuff0, primebuff1, xr);
}
iteration_init(gfp);
{
FLOAT8 frame_duration = 576. * gfc->mode_gr / gfp->out_samplerate;
gfc->ATH->decay = pow(10., -12./10. * frame_duration);
gfc->ATH->adjust = 1.0;
gfc->ATH->adjust_limit = 0.01;
}
}
switch (gfp->padding_type) {
case 0:
gfc->padding=0;
break;
case 1:
gfc->padding=1;
break;
case 2:
default:
if (gfp->VBR!=vbr_off) {
gfc->padding=0;
} else {
if (gfp->disable_reservoir) {
gfc->padding = 0;
}else{
gfc->slot_lag -= gfc->frac_SpF;
if (gfc->slot_lag < 0) {
gfc->slot_lag += gfp->out_samplerate;
gfc->padding = 1;
} else {
gfc->padding = 0;
}
}
}
}
if (gfc->psymodel) {
int ret;
const sample_t *bufp[2];
int blocktype[2];
ms_ratio_prev=gfc->ms_ratio[gfc->mode_gr-1];
for (gr=0; gr < gfc->mode_gr ; gr++) {
for ( ch = 0; ch < gfc->channels_out; ch++ )
bufp[ch] = &inbuf[ch][576 + gr*576-FFTOFFSET];
if (gfc->nsPsy.use) {
ret=L3psycho_anal_ns( gfp, bufp, gr,
&gfc->ms_ratio[gr],&ms_ratio_next,
masking_LR, masking_MS,
pe[gr],pe_MS[gr],tot_ener[gr],blocktype);
} else {
ret=L3psycho_anal( gfp, bufp, gr,
&gfc->ms_ratio[gr],&ms_ratio_next,
masking_LR, masking_MS,
pe[gr],pe_MS[gr],tot_ener[gr],blocktype);
}
if (ret!=0) return -4;
for ( ch = 0; ch < gfc->channels_out; ch++ )
gfc->l3_side.gr[gr].ch[ch].tt.block_type=blocktype[ch];
if (check_ms_stereo) {
ms_ener_ratio[gr] = tot_ener[gr][2]+tot_ener[gr][3];
if (ms_ener_ratio[gr]>0)
ms_ener_ratio[gr] = tot_ener[gr][3]/ms_ener_ratio[gr];
}
}
}else{
for (gr=0; gr < gfc->mode_gr ; gr++)
for ( ch = 0; ch < gfc->channels_out; ch++ ) {
gfc->l3_side.gr[gr].ch[ch].tt.block_type=NORM_TYPE;
pe_MS[gr][ch]=pe[gr][ch]=700;
}
}
adjust_ATH( gfp, tot_ener );
for( gr = 0; gr < gfc->mode_gr; gr++ ) {
for ( ch = 0; ch < gfc->channels_out; ch++ ) {
gr_info *cod_info = &gfc->l3_side.gr[gr].ch[ch].tt;
cod_info->mixed_block_flag = 0;
if (cod_info->block_type == NORM_TYPE )
cod_info->window_switching_flag = 0;
else
cod_info->window_switching_flag = 1;
}
}
mdct_sub48(gfc, inbuf[0], inbuf[1], xr);
for (gr = 0; gr < gfc->mode_gr; gr++) {
for (ch = 0; ch < gfc->channels_out; ch++) {
gr_info *cod_info = &gfc->l3_side.gr[gr].ch[ch].tt;
if (cod_info->block_type==SHORT_TYPE) {
freorder(gfc->scalefac_band.s,xr[gr][ch]);
}
}
}
if (check_ms_stereo) {
int gr0 = 0, gr1 = gfc->mode_gr-1;
check_ms_stereo =
(gfc->l3_side.gr[gr0].ch[0].tt.block_type==gfc->l3_side.gr[gr0].ch[1].tt.block_type) &&
(gfc->l3_side.gr[gr1].ch[0].tt.block_type==gfc->l3_side.gr[gr1].ch[1].tt.block_type);
}
assert (  gfc->mode_ext == MPG_MD_LR_LR );
gfc->mode_ext = MPG_MD_LR_LR;
if (gfp->force_ms) {
gfc->mode_ext = MPG_MD_MS_LR;
} else if (check_ms_stereo) {
FLOAT8  ms_ratio_ave1;
FLOAT8  ms_ratio_ave2;
FLOAT8  threshold1    = 0.35;
FLOAT8  threshold2    = 0.45;
if (gfc->mode_gr==1) {
ms_ratio_ave1 = 0.33 * ( gfc->ms_ratio[0] + ms_ratio_prev + ms_ratio_next );
ms_ratio_ave2 = gfc->ms_ratio[0];
}else{
ms_ratio_ave1 = 0.25 * ( gfc->ms_ratio[0] + gfc->ms_ratio[1] + ms_ratio_prev + ms_ratio_next );
ms_ratio_ave2 = 0.50 * ( gfc->ms_ratio[0] + gfc->ms_ratio[1] );
}
if (gfp->mode_automs) {
if ( gfp->compression_ratio < 11.025 ) {
double thr = (gfp->compression_ratio - 6.3) / (11.025 - 6.3);
if (thr<0) thr=0;
threshold1   *= thr;
threshold2   *= thr;
}
}
if ((ms_ratio_ave1 < threshold1  &&  ms_ratio_ave2 < threshold2) || gfc->nsPsy.use) {
int  sum_pe_MS = pe_MS[0][0] + pe_MS[0][1] + pe_MS[1][0] + pe_MS[1][1];
int  sum_pe_LR = pe   [0][0] + pe   [0][1] + pe   [1][0] + pe   [1][1];
if (sum_pe_MS <= 1.07 * sum_pe_LR && !gfc->nsPsy.use) gfc->mode_ext = MPG_MD_MS_LR;
if (sum_pe_MS <= 1.00 * sum_pe_LR &&  gfc->nsPsy.use) gfc->mode_ext = MPG_MD_MS_LR;
}
}
if (gfp->analysis && gfc->pinfo != NULL) {
for ( gr = 0; gr < gfc->mode_gr; gr++ ) {
for ( ch = 0; ch < gfc->channels_out; ch++ ) {
gfc->pinfo->ms_ratio[gr]=gfc->ms_ratio[gr];
gfc->pinfo->ms_ener_ratio[gr]=ms_ener_ratio[gr];
gfc->pinfo->blocktype[gr][ch]=
gfc->l3_side.gr[gr].ch[ch].tt.block_type;
memcpy(gfc->pinfo->xr[gr][ch],xr[gr][ch],sizeof(xr[gr][ch]));
if (gfc->mode_ext==MPG_MD_MS_LR) {
gfc->pinfo->pe[gr][ch]=gfc->pinfo->pe[gr][ch+2];
gfc->pinfo->ers[gr][ch]=gfc->pinfo->ers[gr][ch+2];
memcpy(gfc->pinfo->energy[gr][ch],gfc->pinfo->energy[gr][ch+2],
sizeof(gfc->pinfo->energy[gr][ch]));
}
}
}
}
if (MPG_MD_MS_LR == gfc->mode_ext) {
masking = &masking_MS;
pe_use = &pe_MS;
} else {
masking = &masking_LR;
pe_use = &pe;
}
if (gfc->nsPsy.use && (gfp->VBR == vbr_off || gfp->VBR == vbr_abr)) {
static FLOAT fircoef[19] = {
-0.0207887,-0.0378413,-0.0432472,-0.031183,
7.79609e-18,0.0467745,0.10091,0.151365,
0.187098,0.2,0.187098,0.151365,
0.10091,0.0467745,7.79609e-18,-0.031183,
-0.0432472,-0.0378413,-0.0207887,
};
int i;
FLOAT8 f;
for(i=0;i<18;i++) gfc->nsPsy.pefirbuf[i] = gfc->nsPsy.pefirbuf[i+1];
i=0;
gfc->nsPsy.pefirbuf[18] = 0;
for ( gr = 0; gr < gfc->mode_gr; gr++ ) {
for ( ch = 0; ch < gfc->channels_out; ch++ ) {
gfc->nsPsy.pefirbuf[18] += (*pe_use)[gr][ch];
i++;
}
}
gfc->nsPsy.pefirbuf[18] = gfc->nsPsy.pefirbuf[18] / i;
f = 0;
for(i=0;i<19;i++) f += gfc->nsPsy.pefirbuf[i] * fircoef[i];
for ( gr = 0; gr < gfc->mode_gr; gr++ ) {
for ( ch = 0; ch < gfc->channels_out; ch++ ) {
(*pe_use)[gr][ch] *= 670 / f;
}
}
}
switch (gfp->VBR){
default:
case vbr_off:
iteration_loop( gfp,*pe_use,ms_ener_ratio, xr, *masking, l3_enc, scalefac);
break;
case vbr_mt:
VBR_quantize( gfp,*pe_use,ms_ener_ratio, xr, *masking, l3_enc, scalefac);
break;
case vbr_rh:
case vbr_mtrh:
VBR_iteration_loop( gfp,*pe_use,ms_ener_ratio, xr, *masking, l3_enc, scalefac);
break;
case vbr_abr:
ABR_iteration_loop( gfp,*pe_use,ms_ener_ratio, xr, *masking, l3_enc, scalefac);
break;
}
getframebits(gfp, &bitsPerFrame, &mean_bits);
format_bitstream( gfp, bitsPerFrame, l3_enc, scalefac);
mp3count = copy_buffer(mp3buf,mp3buf_size,&gfc->bs);
if (gfp->bWriteVbrTag) AddVbrFrame(gfp);
if (gfp->analysis && gfc->pinfo != NULL) {
int j;
for ( ch = 0; ch < gfc->channels_out; ch++ ) {
for ( j = 0; j < FFTOFFSET; j++ )
gfc->pinfo->pcmdata[ch][j] = gfc->pinfo->pcmdata[ch][j+gfp->framesize];
for ( j = FFTOFFSET; j < 1600; j++ ) {
gfc->pinfo->pcmdata[ch][j] = inbuf[ch][j-FFTOFFSET];
}
}
set_frame_pinfo (gfp, xr, *masking, l3_enc, scalefac);
}
updateStats( gfc );
return mp3count;
}