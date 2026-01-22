#ifndef dwimg_INCLUDED
#  define dwimg_INCLUDED
typedef struct IMAGE_DEVICEN_S IMAGE_DEVICEN;
struct IMAGE_DEVICEN_S {
int used;
int visible;
char name[64];
int cyan;
int magenta;
int yellow;
int black;
int menu;
};
#define IMAGE_DEVICEN_MAX 8
typedef struct IMAGE_S IMAGE;
struct IMAGE_S {
void *handle;
void *device;
HWND hwnd;
HBRUSH hBrush;
int raster;
unsigned int format;
unsigned char *image;
BITMAPINFOHEADER bmih;
HPALETTE palette;
int bytewidth;
int devicen_gray;
IMAGE_DEVICEN devicen[IMAGE_DEVICEN_MAX];
UINT update_timer;
int update_tick;
int update_count;
int update_interval;
int pending_update;
int pending_sync;
int cxClient, cyClient;
int cxAdjust, cyAdjust;
int nVscrollPos, nVscrollMax;
int nHscrollPos, nHscrollMax;
HANDLE hmutex;
IMAGE *next;
HWND hwndtext;
int x, y, cx, cy;
};
extern IMAGE *first_image;
IMAGE *image_find(void *handle, void *device);
IMAGE *image_new(void *handle, void *device);
void image_delete(IMAGE *img);
int image_size(IMAGE *img, int new_width, int new_height, int new_raster,
unsigned int new_format, void *pimage);
void image_open(IMAGE *img);
void image_close(IMAGE *img);
void image_sync(IMAGE *img);
void image_page(IMAGE *img);
void image_presize(IMAGE *img, int new_width, int new_height, int new_raster,
unsigned int new_format);
void image_poll(IMAGE *img);
void image_updatesize(IMAGE *img);
#endif