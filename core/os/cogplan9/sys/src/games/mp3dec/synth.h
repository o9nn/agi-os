# ifndef LIBMAD_SYNTH_H
# define LIBMAD_SYNTH_H
# include "fixed.h"
# include "frame.h"
struct mad_pcm {
unsigned int samplerate;
unsigned short channels;
unsigned short length;
mad_fixed_t samples[2][1152];
};
struct mad_synth {
mad_fixed_t filter[2][2][2][16][8];
unsigned int phase;
struct mad_pcm pcm;
};
enum {
MAD_PCM_CHANNEL_SINGLE = 0
};
enum {
MAD_PCM_CHANNEL_DUAL_1 = 0,
MAD_PCM_CHANNEL_DUAL_2 = 1
};
enum {
MAD_PCM_CHANNEL_STEREO_LEFT  = 0,
MAD_PCM_CHANNEL_STEREO_RIGHT = 1
};
void mad_synth_init(struct mad_synth *);
# define mad_synth_finish(synth)
void mad_synth_mute(struct mad_synth *);
void mad_synth_frame(struct mad_synth *, struct mad_frame const *);
# endif