#ifndef gdevdsp_INCLUDED
# define gdevdsp_INCLUDED
#define DISPLAY_VERSION_MAJOR 2
#define DISPLAY_VERSION_MINOR 0
#define DISPLAY_VERSION_MAJOR_V1 1
#define DISPLAY_VERSION_MINOR_V1 0
typedef enum {
DISPLAY_COLORS_NATIVE = (1<<0),
DISPLAY_COLORS_GRAY = (1<<1),
DISPLAY_COLORS_RGB = (1<<2),
DISPLAY_COLORS_CMYK = (1<<3),
DISPLAY_COLORS_SEPARATION = (1<<19)
} DISPLAY_FORMAT_COLOR;
#define DISPLAY_COLORS_MASK 0x8000fL
typedef enum {
DISPLAY_ALPHA_NONE = (0<<4),
DISPLAY_ALPHA_FIRST = (1<<4),
DISPLAY_ALPHA_LAST = (1<<5),
DISPLAY_UNUSED_FIRST = (1<<6),
DISPLAY_UNUSED_LAST = (1<<7)
} DISPLAY_FORMAT_ALPHA;
#define DISPLAY_ALPHA_MASK 0x00f0L
typedef enum {
DISPLAY_DEPTH_1 = (1<<8),
DISPLAY_DEPTH_2 = (1<<9),
DISPLAY_DEPTH_4 = (1<<10),
DISPLAY_DEPTH_8 = (1<<11),
DISPLAY_DEPTH_12 = (1<<12),
DISPLAY_DEPTH_16 = (1<<13)
} DISPLAY_FORMAT_DEPTH;
#define DISPLAY_DEPTH_MASK 0xff00L
typedef enum {
DISPLAY_BIGENDIAN = (0<<16),
DISPLAY_LITTLEENDIAN = (1<<16)
} DISPLAY_FORMAT_ENDIAN;
#define DISPLAY_ENDIAN_MASK 0x00010000L
typedef enum {
DISPLAY_TOPFIRST = (0<<17),
DISPLAY_BOTTOMFIRST = (1<<17)
} DISPLAY_FORMAT_FIRSTROW;
#define DISPLAY_FIRSTROW_MASK 0x00020000L
typedef enum {
DISPLAY_NATIVE_555 = (0<<18),
DISPLAY_NATIVE_565 = (1<<18)
} DISPLAY_FORMAT_555;
#define DISPLAY_555_MASK 0x00040000L
typedef enum {
DISPLAY_ROW_ALIGN_DEFAULT = (0<<20),
DISPLAY_ROW_ALIGN_4 = (3<<20),
DISPLAY_ROW_ALIGN_8 = (4<<20),
DISPLAY_ROW_ALIGN_16 = (5<<20),
DISPLAY_ROW_ALIGN_32 = (6<<20),
DISPLAY_ROW_ALIGN_64 = (7<<20)
} DISPLAY_FORMAT_ROW_ALIGN;
#define DISPLAY_ROW_ALIGN_MASK 0x00700000L
#ifndef display_callback_DEFINED
#define display_callback_DEFINED
typedef struct display_callback_s display_callback;
#endif
struct display_callback_s {
int size;
int version_major;
int version_minor;
int (*display_open)(void *handle, void *device);
int (*display_preclose)(void *handle, void *device);
int (*display_close)(void *handle, void *device);
int (*display_presize)(void *handle, void *device,
int width, int height, int raster, unsigned int format);
int (*display_size)(void *handle, void *device, int width, int height,
int raster, unsigned int format, unsigned char *pimage);
int (*display_sync)(void *handle, void *device);
int (*display_page)(void *handle, void *device, int copies, int flush);
int (*display_update)(void *handle, void *device, int x, int y,
int w, int h);
void *(*display_memalloc)(void *handle, void *device, unsigned long size);
int (*display_memfree)(void *handle, void *device, void *mem);
int (*display_separation)(void *handle, void *device,
int component, const char *component_name,
unsigned short c, unsigned short m,
unsigned short y, unsigned short k);
};
struct display_callback_v1_s {
int size;
int version_major;
int version_minor;
int (*display_open)(void *handle, void *device);
int (*display_preclose)(void *handle, void *device);
int (*display_close)(void *handle, void *device);
int (*display_presize)(void *handle, void *device,
int width, int height, int raster, unsigned int format);
int (*display_size)(void *handle, void *device, int width, int height,
int raster, unsigned int format, unsigned char *pimage);
int (*display_sync)(void *handle, void *device);
int (*display_page)(void *handle, void *device, int copies, int flush);
int (*display_update)(void *handle, void *device, int x, int y,
int w, int h);
void *(*display_memalloc)(void *handle, void *device, unsigned long size);
int (*display_memfree)(void *handle, void *device, void *mem);
};
#define DISPLAY_CALLBACK_V1_SIZEOF sizeof(struct display_callback_v1_s)
#endif