#ifndef LAME_RESERVOIR_H
#define LAME_RESERVOIR_H
int ResvFrameBegin(lame_global_flags *gfp,III_side_info_t *l3_side, int mean_bits, int frameLength );
void ResvMaxBits(lame_global_flags *gfp, int mean_bits, int *targ_bits, int *max_bits);
void ResvAdjust(lame_internal_flags *gfc,gr_info *gi, III_side_info_t *l3_side, int mean_bits );
void ResvFrameEnd(lame_internal_flags *gfc,III_side_info_t *l3_side, int mean_bits );
#endif