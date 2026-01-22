#if !defined(__draw_h)
#define __draw_h 1
#include "drawftypes.h"
typedef struct draw_jpegstrhdr_tag {
draw_tagtyp tag;
draw_sizetyp size;
draw_bboxtyp bbox;
int width;
int height;
int xdpi;
int ydpi;
int trfm[6];
int len;
} draw_jpegstrhdr;
typedef struct draw_jpegstr_tag {
draw_tagtyp tag;
draw_sizetyp size;
draw_bboxtyp bbox;
int width;
int height;
int xdpi;
int ydpi;
int trfm[6];
int len;
unsigned char *jpeg;
} draw_jpegstr;
typedef union draw_imageType_tag {
draw_spristr *sprite;
draw_jpegstr *jpeg;
char *bytep;
int *wordp;
} draw_imageType;
#endif