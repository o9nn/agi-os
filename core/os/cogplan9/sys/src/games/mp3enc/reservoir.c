#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <assert.h>
#include "util.h"
#include "reservoir.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
int
ResvFrameBegin(lame_global_flags *gfp,III_side_info_t *l3_side, int mean_bits, int frameLength )
{
lame_internal_flags *gfc=gfp->internal_flags;
int fullFrameBits;
int resvLimit;
int maxmp3buf;
resvLimit = (gfp->version==1) ? 8*511 : 8*255 ;
maxmp3buf = (gfp->strict_ISO) ? 8*960 : 8*2047;
if ( frameLength > maxmp3buf ||  gfp->disable_reservoir ) {
gfc->ResvMax = 0;
} else {
gfc->ResvMax = maxmp3buf - frameLength;
if ( gfc->ResvMax > resvLimit )
gfc->ResvMax = resvLimit;
}
fullFrameBits = mean_bits * gfc->mode_gr + Min ( gfc->ResvSize, gfc->ResvMax );
if ( gfp->strict_ISO  &&  fullFrameBits > maxmp3buf )
fullFrameBits = maxmp3buf;
assert ( 0 == gfc->ResvMax % 8 );
assert ( gfc->ResvMax >= 0 );
l3_side->resvDrain_pre = 0;
if ( gfc->pinfo != NULL ) {
gfc->pinfo->mean_bits = mean_bits / 2;
gfc->pinfo->resvsize  = gfc->ResvSize;
}
return fullFrameBits;
}
void ResvMaxBits(lame_global_flags *gfp, int mean_bits, int *targ_bits, int *extra_bits)
{
lame_internal_flags *gfc=gfp->internal_flags;
int add_bits;
int full_fac;
*targ_bits = mean_bits ;
full_fac=9;
if (gfc->ResvSize > ((gfc->ResvMax * full_fac) / 10)) {
add_bits= gfc->ResvSize-((gfc->ResvMax * full_fac) / 10);
*targ_bits += add_bits;
}else {
add_bits =0 ;
if (!gfp->disable_reservoir)
*targ_bits -= .1*mean_bits;
}
*extra_bits =
(gfc->ResvSize  < (gfc->ResvMax*6)/10  ? gfc->ResvSize : (gfc->ResvMax*6)/10);
*extra_bits -= add_bits;
if (*extra_bits < 0) *extra_bits=0;
}
void
ResvAdjust(lame_internal_flags *gfc,gr_info *gi, III_side_info_t *l3_side, int mean_bits )
{
gfc->ResvSize += (mean_bits / gfc->channels_out) - gi->part2_3_length;
#if 0
printf("part2_3_length:  %i  avg=%i  incres: %i  resvsize=%i\n",gi->part2_3_length,
mean_bits/gfc->channels_out,
mean_bits/gfc->channels_out-gi->part2_3_length,gfc->ResvSize);
#endif
}
void
ResvFrameEnd(lame_internal_flags *gfc, III_side_info_t *l3_side, int mean_bits)
{
int stuffingBits;
int over_bits;
if ( gfc->channels_out == 2  &&  (mean_bits & 1) )
gfc->ResvSize += 1;
stuffingBits=0;
l3_side->resvDrain_post = 0;
l3_side->resvDrain_pre = 0;
if ( (over_bits = gfc->ResvSize % 8) != 0 )
stuffingBits += over_bits;
over_bits = (gfc->ResvSize - stuffingBits) - gfc->ResvMax;
if (over_bits > 0) {
assert ( 0 == over_bits % 8 );
assert ( over_bits >= 0 );
stuffingBits += over_bits;
}
#define NEW_DRAINXX
#ifdef NEW_DRAIN
{
int mdb_bytes = Min(l3_side->main_data_begin*8,stuffingBits)/8;
l3_side->resvDrain_pre += 8*mdb_bytes;
stuffingBits -= 8*mdb_bytes;
gfc->ResvSize -= 8*mdb_bytes;
l3_side->main_data_begin -= mdb_bytes;
assert ( stuffingBits >= 0 );
l3_side->resvDrain_post += (stuffingBits % 8);
gfc->ResvSize -= stuffingBits % 8;
}
#else
l3_side->resvDrain_post += stuffingBits;
gfc->ResvSize -= stuffingBits;
#endif
return;
}