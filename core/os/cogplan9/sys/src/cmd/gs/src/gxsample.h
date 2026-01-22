#ifndef gxsample_INCLUDED
# define gxsample_INCLUDED
typedef union sample_lookup_s {
bits32 lookup4x1to32[16];
bits16 lookup2x2to16[16];
byte lookup8[256];
} sample_lookup_t;
extern const bits32 lookup4x1to32_identity[16];
extern const bits32 lookup4x1to32_inverted[16];
#ifndef sample_map_DEFINED
#define sample_map_DEFINED
typedef struct sample_map_s sample_map;
#endif
#define SAMPLE_UNPACK_PROC(proc)\
const byte *proc(byte *bptr, int *pdata_x, const byte * data, int data_x,\
uint dsize, const sample_map *smap, int spread,\
int num_components_per_plane)
typedef SAMPLE_UNPACK_PROC((*sample_unpack_proc_t));
SAMPLE_UNPACK_PROC(sample_unpack_copy);
SAMPLE_UNPACK_PROC(sample_unpack_1);
SAMPLE_UNPACK_PROC(sample_unpack_2);
SAMPLE_UNPACK_PROC(sample_unpack_4);
SAMPLE_UNPACK_PROC(sample_unpack_8);
SAMPLE_UNPACK_PROC(sample_unpack_1_interleaved);
SAMPLE_UNPACK_PROC(sample_unpack_2_interleaved);
SAMPLE_UNPACK_PROC(sample_unpack_4_interleaved);
SAMPLE_UNPACK_PROC(sample_unpack_8_interleaved);
#endif