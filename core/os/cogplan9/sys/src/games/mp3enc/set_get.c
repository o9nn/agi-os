#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <assert.h>
#include "lame.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
int
lame_set_num_samples( lame_global_flags*  gfp,
unsigned long       num_samples)
{
gfp->num_samples = num_samples;
return 0;
}
unsigned long
lame_get_num_samples( const lame_global_flags* gfp )
{
return gfp->num_samples;
}
int
lame_set_in_samplerate( lame_global_flags*  gfp,
int                 in_samplerate )
{
gfp->in_samplerate = in_samplerate;
return 0;
}
int
lame_get_in_samplerate( const lame_global_flags*  gfp )
{
return gfp->in_samplerate;
}
int
lame_set_num_channels( lame_global_flags*  gfp,
int                 num_channels )
{
if ( 2 < num_channels || 0 == num_channels )
return -1;
gfp->num_channels = num_channels;
return 0;
}
int
lame_get_num_channels( const lame_global_flags*  gfp )
{
return gfp->num_channels;
}
int
lame_set_scale( lame_global_flags*  gfp,
float               scale )
{
gfp->scale = scale;
return 0;
}
float
lame_get_scale( const lame_global_flags*  gfp )
{
return gfp->scale;
}
int
lame_set_out_samplerate( lame_global_flags*  gfp,
int                 out_samplerate )
{
gfp->out_samplerate = out_samplerate;
return 0;
}
int
lame_get_out_samplerate( const lame_global_flags*  gfp )
{
return gfp->out_samplerate;
}
int
lame_set_analysis( lame_global_flags*  gfp,
int                 analysis )
{
if ( 0 > analysis || 1 < analysis )
return -1;
gfp->analysis = analysis;
return 0;
}
int
lame_get_analysis( const lame_global_flags*  gfp )
{
assert( 0 <= gfp->analysis && 1 >= gfp->analysis );
return gfp->analysis;
}
int
lame_set_bWriteVbrTag( lame_global_flags*  gfp,
int bWriteVbrTag )
{
if ( 0 > bWriteVbrTag || 1 < bWriteVbrTag )
return -1;
gfp->bWriteVbrTag = bWriteVbrTag;
return 0;
}
int
lame_get_bWriteVbrTag( const lame_global_flags*  gfp )
{
assert( 0 <= gfp->bWriteVbrTag && 1 >= gfp->bWriteVbrTag );
return gfp->bWriteVbrTag;
}
int
lame_set_disable_waveheader( lame_global_flags*  gfp,
int                 disable_waveheader )
{
if ( 0 > disable_waveheader || 1 < disable_waveheader )
return -1;
gfp->disable_waveheader = disable_waveheader;
return 0;
}
int
lame_get_disable_waveheader( const lame_global_flags*  gfp )
{
assert( 0 <= gfp->disable_waveheader && 1 >= gfp->disable_waveheader );
return gfp->disable_waveheader;
}
int
lame_set_decode_only( lame_global_flags*  gfp,
int                 decode_only )
{
if ( 0 > decode_only || 1 < decode_only )
return -1;
gfp->decode_only = decode_only;
return 0;
}
int
lame_get_decode_only( const lame_global_flags*  gfp )
{
assert( 0 <= gfp->decode_only && 1 >= gfp->decode_only );
return gfp->decode_only;
}
int
lame_set_ogg( lame_global_flags*  gfp,
int                 ogg )
{
if ( 0 > ogg || 1 < ogg )
return -1;
gfp->ogg = ogg;
return 0;
}
int
lame_get_ogg( const lame_global_flags*  gfp )
{
assert( 0 <= gfp->ogg && 1 >= gfp->ogg );
return gfp->ogg;
}
int
lame_set_quality( lame_global_flags*  gfp,
int                 quality )
{
gfp->quality = quality;
return 0;
}
int
lame_get_quality( const lame_global_flags*  gfp )
{
return gfp->quality;
}
int
lame_set_mode( lame_global_flags*  gfp,
MPEG_mode           mode )
{
if( 0 > mode || MAX_INDICATOR <= mode )
return -1;
gfp->mode = mode;
return 0;
}
MPEG_mode
lame_get_mode( const lame_global_flags*  gfp )
{
assert( 0 <= gfp->mode && MAX_INDICATOR > gfp->mode );
return gfp->mode;
}
int
lame_set_mode_automs( lame_global_flags*  gfp,
int                 mode_automs )
{
if ( 0 > mode_automs || 1 < mode_automs )
return -1;
gfp->mode_automs = mode_automs;
return 0;
}
int
lame_get_mode_automs( const lame_global_flags*  gfp )
{
assert( 0 <= gfp->mode_automs && 1 >= gfp->mode_automs );
return gfp->mode_automs;
}
int
lame_set_force_ms( lame_global_flags*  gfp,
int                 force_ms );
int
lame_get_force_ms( const lame_global_flags*  gfp );
int
lame_set_free_format( lame_global_flags*  gfp,
int                 free_format );
int
lame_get_free_format( const lame_global_flags*  gfp );
int
lame_set_errorf( lame_global_flags*  gfp,
void                (*func)( const char*, va_list ) )
{
gfp->report.errorf = func;
return 0;
}
int
lame_set_debugf( lame_global_flags*  gfp,
void                (*func)( const char*, va_list ) )
{
gfp->report.debugf = func;
return 0;
}
int
lame_set_msgf( lame_global_flags*  gfp,
void                (*func)( const char *, va_list ) )
{
gfp->report.msgf = func;
return 0;
}
int
lame_set_brate( lame_global_flags*  gfp,
int                 brate );
int
lame_get_brate( const lame_global_flags*  gfp );
int
lame_set_compression_ratio( lame_global_flags*  gfp,
float               compression_ratio );
float
lame_get_compression_ratio( const lame_global_flags*  gfp );
int
lame_set_copyright( lame_global_flags*  gfp,
int                 copyright );
int
lame_get_copyright( const lame_global_flags*  gfp );
int
lame_set_original( lame_global_flags*  gfp,
int                 original );
int
lame_get_original( const lame_global_flags*  gfp );
int
lame_set_error_protection( lame_global_flags*  gfp,
int                 error_protection );
int
lame_get_error_protection( const lame_global_flags*  gfp );
int
lame_set_padding_type( lame_global_flags*  gfp,
int                 padding_type );
int
lame_get_padding_type( const lame_global_flags*  gfp );
int
lame_set_extension( lame_global_flags*  gfp,
int                 extension );
int
lame_get_extension( const lame_global_flags*  gfp );
int
lame_set_strict_ISO( lame_global_flags*  gfp,
int                 strict_ISO );
int
lame_get_strict_ISO( const lame_global_flags*  gfp );
int
lame_set_disable_reservoir( lame_global_flags*  gfp,
int                 disable_reservoir );
int
lame_get_disable_reservoir( const lame_global_flags*  gfp );
int
lame_set_experimentalX( lame_global_flags*  gfp,
int                 experimentalX );
int
lame_get_experimentalX( const lame_global_flags*  gfp );
int
lame_set_experimentalY( lame_global_flags*  gfp,
int                 experimentalY );
int
lame_get_experimentalY( const lame_global_flags*  gfp );
int
lame_set_experimentalZ( lame_global_flags*  gfp,
int                 experimentalZ );
int
lame_get_experimentalZ( const lame_global_flags*  gfp );
int
lame_set_exp_nspsytune( lame_global_flags*  gfp,
int                 exp_nspsytune );
int
lame_get_exp_nspsytune( const lame_global_flags*  gfp );
int
lame_set_VBR( lame_global_flags*  gfp,
vbr_mode            VBR );
vbr_mode
lame_get_exp_VBR( const lame_global_flags*  gfp );
int
lame_set_VBR_q( lame_global_flags*  gfp,
int                 VBR_q );
int
lame_get_VBR_q( const lame_global_flags*  gfp );
int
lame_set_VBR_mean_bitrate_kbps( lame_global_flags*  gfp,
int                 VBR_mean_bitrate_kbps );
int
lame_get_VBR_mean_bitrate_kbps( const lame_global_flags*  gfp );
int
lame_set_VBR_min_bitrate_kbps( lame_global_flags*  gfp,
int                 VBR_min_bitrate_kbps );
int
lame_get_VBR_min_bitrate_kbps( const lame_global_flags*  gfp );
int
lame_set_VBR_max_bitrate_kbps( lame_global_flags*  gfp,
int                 VBR_max_bitrate_kbps );
int
lame_get_VBR_max_bitrate_kbps( const lame_global_flags*  gfp );
int
lame_set_VBR_hard_min( lame_global_flags*  gfp,
int                 VBR_hard_min );
int
lame_get_VBR_hard_min( const lame_global_flags*  gfp );
int
lame_set_lowpassfreq( lame_global_flags*  gfp,
int                 lowpassfreq );
int
lame_get_lowpassfreq( const lame_global_flags*  gfp );
int
lame_set_lowpasswidth( lame_global_flags*  gfp,
int                 lowpasswidth );
int
lame_get_lowpasswidth( const lame_global_flags*  gfp );
int
lame_set_highpassfreq( lame_global_flags*  gfp,
int                 highpassfreq );
int
lame_get_highpassfreq( const lame_global_flags*  gfp );
int
lame_set_highpasswidth( lame_global_flags*  gfp,
int                 highpasswidth );
int
lame_get_highpasswidth( const lame_global_flags*  gfp );
int
lame_set_ATHonly( lame_global_flags*  gfp,
int                 ATHonly );
int
lame_get_ATHonly( const lame_global_flags*  gfp );
int
lame_set_ATHshort( lame_global_flags*  gfp,
int                 ATHshort );
int
lame_get_ATHshort( const lame_global_flags*  gfp );
int
lame_set_noATH( lame_global_flags*  gfp,
int                 noATH );
int
lame_get_noATH( const lame_global_flags*  gfp );
int
lame_set_ATHtype( lame_global_flags*  gfp,
int                 ATHtype );
int
lame_get_ATHtype( const lame_global_flags*  gfp );
int
lame_set_ATHlower( lame_global_flags*  gfp,
float               ATHlower );
float
lame_get_ATHlower( const lame_global_flags*  gfp );
int
lame_set_cwlimit( lame_global_flags*  gfp,
int                 cwlimit );
int
lame_get_cwlimit( const lame_global_flags*  gfp );
int
lame_set_allow_diff_short( lame_global_flags*  gfp,
int                 allow_diff_short );
int
lame_get_allow_diff_short( const lame_global_flags*  gfp );
int
lame_set_useTemporal( lame_global_flags*  gfp,
int                 useTemporal );
int
lame_get_useTemporal( const lame_global_flags*  gfp );
int
lame_set_no_short_blocks( lame_global_flags*  gfp,
int                 no_short_blocks );
int
lame_get_no_short_blocks( const lame_global_flags*  gfp );
int
lame_set_emphasis( lame_global_flags*  gfp,
int                 emphasis );
int
lame_get_emphasis( const lame_global_flags*  gfp );
int
lame_get_version( const lame_global_flags* gfp );
int
lame_get_encoder_delay( const lame_global_flags*  gfp );
int
lame_get_framesize( const lame_global_flags*  gfp );
int
lame_get_frameNum( const lame_global_flags*  gfp );
int
lame_get_totalframes( const lame_global_flags*  gfp );