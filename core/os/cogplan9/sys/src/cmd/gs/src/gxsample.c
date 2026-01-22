#include "gx.h"
#include "gxsample.h"
#include "gxfixed.h"
#include "gximage.h"
#define map4tox(z,a,b,c,d)\
z, z^a, z^b, z^(a+b),\
z^c, z^(a+c), z^(b+c), z^(a+b+c),\
z^d, z^(a+d), z^(b+d), z^(a+b+d),\
z^(c+d), z^(a+c+d), z^(b+c+d), z^(a+b+c+d)
#ifdef __STDC__
#  define n0L 0xffffffffU
#  define ffL8 0x0000ff00U
#  define ffL16 0x00ff0000U
#  define ffL24 0xff000000U
#else
#if arch_sizeof_long == 4
#  define n0L (-1)
#  define ffL8 0x0000ff00
#  define ffL16 0x00ff0000
#  define ffL24 (-0x01000000)
#else
#  define n0L 0xffffffffL
#  define ffL8 0x0000ff00L
#  define ffL16 0x00ff0000L
#  define ffL24 0xff000000L
#endif
#endif
#if arch_is_big_endian
const bits32 lookup4x1to32_identity[16] = {
map4tox(0, 0xff, ffL8, ffL16, ffL24)
};
const bits32 lookup4x1to32_inverted[16] = {
map4tox(n0L, 0xff, ffL8, ffL16, ffL24)
};
#else
const bits32 lookup4x1to32_identity[16] = {
map4tox(0, ffL24, ffL16, ffL8, 0xff)
};
const bits32 lookup4x1to32_inverted[16] = {
map4tox(n0L, ffL24, ffL16, ffL8, 0xff)
};
#endif
#undef n0L
#undef ffL8
#undef ffL16
#undef ffL24
const byte *
sample_unpack_copy(byte * bptr, int *pdata_x, const byte * data, int data_x,
uint dsize, const sample_map *ignore_smap, int spread,
int ignore_num_components_per_plane)
{
*pdata_x = data_x;
return data;
}
#define MULTIPLE_MAPS 0
#define TEMPLATE_sample_unpack_1 sample_unpack_1
#define TEMPLATE_sample_unpack_2 sample_unpack_2
#define TEMPLATE_sample_unpack_4 sample_unpack_4
#define TEMPLATE_sample_unpack_8 sample_unpack_8
#include "gxsamplp.h"
#undef MULTIPLE_MAPS
#undef TEMPLATE_sample_unpack_1
#undef TEMPLATE_sample_unpack_2
#undef TEMPLATE_sample_unpack_4
#undef TEMPLATE_sample_unpack_8
#define MULTIPLE_MAPS 1
#define TEMPLATE_sample_unpack_1 sample_unpack_1_interleaved
#define TEMPLATE_sample_unpack_2 sample_unpack_2_interleaved
#define TEMPLATE_sample_unpack_4 sample_unpack_4_interleaved
#define TEMPLATE_sample_unpack_8 sample_unpack_8_interleaved
#include "gxsamplp.h"
#undef TEMPLATE_sample_unpack_1
#undef TEMPLATE_sample_unpack_2
#undef TEMPLATE_sample_unpack_4
#undef TEMPLATE_sample_unpack_8
#undef MULTIPLE_MAPS