# ifndef LIBMAD_FRAME_H
# define LIBMAD_FRAME_H
# include "fixed.h"
# include "stream.h"
enum mad_layer {
MAD_LAYER_I = 1,
MAD_LAYER_II = 2,
MAD_LAYER_III = 3
};
enum mad_mode {
MAD_MODE_SINGLE_CHANNEL = 0,
MAD_MODE_DUAL_CHANNEL = 1,
MAD_MODE_JOINT_STEREO = 2,
MAD_MODE_STEREO = 3
};
enum mad_emphasis {
MAD_EMPHASIS_NONE = 0,
MAD_EMPHASIS_50_15_US = 1,
MAD_EMPHASIS_CCITT_J_17 = 3,
MAD_EMPHASIS_RESERVED = 2
};
struct mad_header {
enum mad_layer layer;
enum mad_mode mode;
int mode_extension;
enum mad_emphasis emphasis;
unsigned long bitrate;
unsigned int samplerate;
unsigned short crc_check;
unsigned short crc_target;
int flags;
int private_bits;
};
struct mad_frame {
struct mad_header header;
int options;
mad_fixed_t sbsample[2][36][32];
mad_fixed_t (*overlap)[2][32][18];
};
# define MAD_NCHANNELS(header) ((header)->mode ? 2 : 1)
# define MAD_NSBSAMPLES(header) \
((header)->layer == MAD_LAYER_I ? 12 : \
(((header)->layer == MAD_LAYER_III && \
((header)->flags & MAD_FLAG_LSF_EXT)) ? 18 : 36))
enum {
MAD_FLAG_NPRIVATE_III = 0x0007,
MAD_FLAG_INCOMPLETE = 0x0008,
MAD_FLAG_PROTECTION = 0x0010,
MAD_FLAG_COPYRIGHT = 0x0020,
MAD_FLAG_ORIGINAL = 0x0040,
MAD_FLAG_PADDING = 0x0080,
MAD_FLAG_I_STEREO = 0x0100,
MAD_FLAG_MS_STEREO = 0x0200,
MAD_FLAG_FREEFORMAT = 0x0400,
MAD_FLAG_LSF_EXT = 0x1000,
MAD_FLAG_MC_EXT = 0x2000,
MAD_FLAG_MPEG_2_5_EXT = 0x4000
};
enum {
MAD_PRIVATE_HEADER = 0x0100,
MAD_PRIVATE_III = 0x001f
};
void mad_header_init(struct mad_header *);
# define mad_header_finish(header)
int mad_header_decode(struct mad_header *, struct mad_stream *);
void mad_frame_init(struct mad_frame *);
void mad_frame_finish(struct mad_frame *);
int mad_frame_decode(struct mad_frame *, struct mad_stream *);
void mad_frame_mute(struct mad_frame *);
# endif