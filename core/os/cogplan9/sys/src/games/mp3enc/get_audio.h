#ifndef LAME_GET_AUDIO_H
#define LAME_GET_AUDIO_H
typedef enum sound_file_format_e {
sf_unknown,
sf_raw,
sf_wave,
sf_aiff,
sf_mp1,
sf_mp2,
sf_mp3,
sf_ogg
} sound_file_format;
FILE* init_outfile ( char *outPath, int decode );
void init_infile(lame_global_flags *, char *inPath);
void close_infile(void);
int get_audio(lame_global_flags *gfp,short buffer[2][1152]);
int lame_decoder(lame_global_flags *gfp,FILE *outf,int skip, char *inPath, char *outPath);
void SwapBytesInWords( short *loc, int words );
#ifdef LIBSNDFILE
#include "sndfile.h"
#else
#include "portableio.h"
typedef struct blockAlign_struct {
unsigned long offset;
unsigned long blockSize;
} blockAlign;
typedef struct IFF_AIFF_struct {
short numChannels;
unsigned long numSampleFrames;
short sampleSize;
double sampleRate;
unsigned long sampleType;
blockAlign blkAlgn;
} IFF_AIFF;
extern int aiff_read_headers(FILE*, IFF_AIFF*);
extern int aiff_seek_to_sound_data(FILE*);
extern int aiff_write_headers(FILE*, IFF_AIFF*);
extern int parse_wavheader(void);
extern int parse_aiff(const char fn[]);
extern void aiff_check(const char*, IFF_AIFF*, int*);
#endif
#endif