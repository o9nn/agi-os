#include "gdevprn.h"
#include <math.h>
#define MAX_WIDTH 11.46
#define MAX_PIXELS 8250
#define MAX_BYTES (MAX_PIXELS+7)/8
#define MARGIN_L 0.12
#define MARGIN_R 0.12
#define MARGIN_T 0.12
#define MARGIN_B 0.50
#define Y_DPI 720
#define X_DPI 720
#define RESCODE( x ) (3600/(x))
#define DCOLN 6
#define DEV_BLACK 0
#define DEV_CYAN 1
#define DEV_MAGENTA 2
#define DEV_YELLOW 3
#define DEV_LCYAN 4
#define DEV_LMAGENTA 5
#define NOZZLES 32
#define HEAD_SPACING 8
#define CR 13
#define FF 12
#define ESC "\033"
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
#define ICOLN 4
#define MAX_ED_LINES 3
#define MAP_RGB_ADOBE 0
#define OFFS_C 0
#define OFFS_M 1
#define OFFS_Y 2
#define OFFS_K 3
#define DECOMPOSE_CMYK( index, c, m, y, k ) \
{ \
(k) = (index) & 255; \
(y) = ( (index) >> 8 ) & 255; \
(m) = ( (index) >> 16 ) & 255; \
(c) = ( (index) >> 24 ) & 255; \
}
#define BUILD_CMYK( c, m, y, k ) \
((((long)(c)&255)<<24)|(((long)(m)&255)<<16)|\
(((long)(y)&255)<<8)|((long)(k)&255))
typedef struct {
int ra;
int ia;
int c;
int m;
int y;
} CCOMP;
typedef struct gx_photoex_device_s {
gx_device_common;
gx_prn_device_common;
int shingling;
int depletion;
int halftoner;
int splash;
int leakage;
int mono;
int pureblack;
int midcyan;
int midmagenta;
int dotsize;
} gx_photoex_device;
typedef gx_device DEV;
typedef gx_device_printer PDEV;
typedef gx_photoex_device EDEV;
typedef gx_color_index CINX;
typedef gx_color_value CVAL;
typedef gs_param_list PLIST;
typedef gs_param_name PNAME;
#define MAX_MARK ((NOZZLES)*(HEAD_SPACING))
typedef struct {
int first;
int last;
byte data[ MAX_BYTES ];
} RAWLINE;
#define BAND_1440 13
#define BAND_720 31
#define BAND_360 1
#define NOZZLE_1440 (NOZZLES)
#define NOZZLE_720 (NOZZLES)
#define NOZZLE_360 1
typedef struct {
int last;
int resol;
int nozzle;
int down;
int head[ NOZZLES ];
int offset;
int top;
int markbeg;
byte mark[ MAX_MARK ];
} SCHEDUL;
#define SendByte( s, x ) fputc( (x), (s) )
#define SendWord( s, x ) SendByte((s), (x) & 255); \
SendByte((s), ((x) >> 8 ) & 255);
typedef struct {
EDEV *dev;
FILE *stream;
int yres;
int xres;
int start;
int width;
int lines;
int mono;
byte *dbuff;
int htone_thold;
int htone_last;
SCHEDUL schedule;
short err[ MAX_ED_LINES ][ ICOLN ][ MAX_PIXELS*2 ];
short ( *error[ MAX_ED_LINES ] )[ MAX_PIXELS*2 ];
byte res[ ICOLN ][ MAX_PIXELS*2 ];
RAWLINE raw[ 2 ][ DCOLN ][ MAX_MARK ];
byte rle[ MAX_PIXELS * 2 ];
} RENDER;
typedef struct {
RENDER *render;
byte *data;
int step;
byte *res;
byte *block;
short **err;
int lim1;
int lim2;
int mval;
} HTONE;
typedef struct {
int (*hthld)( RENDER *rend );
void (*hstrt)( RENDER *rend, int line );
void (*hteol)( RENDER *rend, int line );
void (*htone)( HTONE *htone, int line );
} HFUNCS;
#define MAXHTONE 3
#define DMATRIX_X 16
#define DMATRIX_Y 16
private int photoex_open( gx_device *pdev );
private int photoex_print_page( PDEV *dev, FILE *prn_stream );
private CINX photoex_map_rgb_color( DEV *dev, CVAL r, CVAL g, CVAL b );
private int photoex_map_color_rgb( DEV *dev, CINX index, CVAL prgb[3] );
private int photoex_get_params( DEV *dev, PLIST *plist );
private int photoex_put_params( DEV *dev, PLIST *plist );
private int PutInt( PLIST *plist, PNAME name, int *val,
int minval, int maxval, int code );
private int GetInt( PLIST *list, PNAME name, int *value, int code );
private int Cmy2A( int c, int m, int y );
private void SchedulerInit( SCHEDUL *p );
private int ScheduleLines( SCHEDUL *p );
private void ScheduleLeading( SCHEDUL *p );
private void ScheduleMiddle( SCHEDUL *p );
private void ScheduleTrailing( SCHEDUL *p );
private void ScheduleBand( SCHEDUL *p, int mask );
private void RenderPage( RENDER *p );
private void RenderLine( RENDER *p, int line );
private int IsScanlineEmpty( RENDER *p, byte *line );
private int RleCompress( RAWLINE *raw, int min, int max, byte *rle_data );
private int RleFlush( byte *first, byte *reps, byte *now, byte *out );
private void SendReset( FILE *stream );
private void SendMargin( FILE *stream, int top, int bot );
private void SendPaper( FILE *stream, int length );
private void SendGmode( FILE *stream, int on );
private void SendUnit( FILE *stream, int res );
private void SendUnidir( FILE *stream, int on );
private void SendMicro( FILE *stream, int on );
private void SendInk( FILE *stream, int x );
private void SendDown( FILE *stream, int x );
private void SendRight( FILE *stream, int amount );
private void SendColour( FILE *stream, int col );
private void SendData( FILE *stream, int hres, int vres, int noz, int col );
private void SendString( FILE *stream, const char *s );
private void HalftonerStart( RENDER *render, int line );
private int HalftoneThold( RENDER *render );
private void HalftoneLine( RENDER *render, int line, byte *data );
private int BendorThold( RENDER *p );
private void BendorStart( RENDER *p, int line );
private void BendorEol( RENDER *p, int line );
private void BendorLine( HTONE *htone, int y );
private int FloydSThold( RENDER *p );
private void FloydSStart( RENDER *p, int line );
private void FloydSEol( RENDER *p, int line );
private void FloydSLine( HTONE *htone, int y );
private int DitherThold( RENDER *p );
private void DitherStart( RENDER *p, int line );
private void DitherEol( RENDER *p, int line );
private void DitherLine( HTONE *htone, int y );
private const HFUNCS htable[ MAXHTONE ] = {
{ FloydSThold, FloydSStart, FloydSEol, FloydSLine },
{ DitherThold, DitherStart, DitherEol, DitherLine },
{ BendorThold, BendorStart, BendorEol, BendorLine }
};
private gx_device_procs photoex_device_procs = prn_color_params_procs(
photoex_open,
gdev_prn_output_page,
gdev_prn_close,
photoex_map_rgb_color,
photoex_map_color_rgb,
photoex_get_params,
photoex_put_params
);
gx_photoex_device far_data gs_photoex_device = {
prn_device_body(
gx_photoex_device,
photoex_device_procs,
"photoex",
DEFAULT_WIDTH_10THS,
DEFAULT_HEIGHT_10THS,
X_DPI,
Y_DPI,
MARGIN_L,
MARGIN_B,
MARGIN_R,
MARGIN_T,
ICOLN,
32,
255,
255,
256,
256,
photoex_print_page
),
0,
0,
0,
0,
0,
0,
1,
127,
127,
0
};
private const int start_720[ HEAD_SPACING ][ NOZZLES ] = {
{ 0, 8, 16, 24, 32, 40, 48, 56,
64, 72, 80, 88, 96, 104, 112, 120,
128, 136, 144, 152, 160, 168, 176, 184,
192, 200, 208, 216, 224, 232, 240, 248 },
{ 1, 9, 17, 25, 33, 41, 49, 57,
65, 73, 81, 89, 97, 105, 113, 121,
129, 137, 145, 153, 161, 169, 177, 185,
193, 201, 209, -1, -1, -1, -1, -1 },
{ 2, 10, 18, 26, 34, 42, 50, 58,
66, 74, 82, 90, 98, 106, 114, 122,
130, 138, 146, 154, 162, 170, 178, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 3, 11, 19, 27, 35, 43, 51, 59,
67, 75, 83, 91, 99, 107, 115, 123,
131, 139, 147, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 4, 12, 20, 28, 36, 44, 52, 60,
68, 76, 84, 92, 100, 108, 116, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 5, 13, 21, 29, 37, 45, 53, 61,
69, 77, 85, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 6, 14, 22, 30, 38, 46, 54, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 7, 15, 23, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 }
};
private const int start_1440[ 2 ][ HEAD_SPACING ][ NOZZLES ] = {
{
{ 0, 8, 16, 24, 32, 40, 48, 56,
64, 72, 80, 88, 96, 104, 112, 120,
128, 136, 144, 152, 160, 168, 176, 184,
192, 200, 208, 216, 224, 232, 240, 248 },
{ 1, 9, 17, 25, 33, 41, 49, 57,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 2, 10, 18, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 3, 11, 19, 27, 35, 43, 51, 59,
67, 75, 83, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 4, 12, 20, 28, 36, 44, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 5, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 6, 14, 22, 30, 38, 46, 54, 62,
70, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 7, 15, 23, 31, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
},
{
{ 0, 8, 16, 24, 32, 40, 48, 56,
64, 72, 80, 88, 96, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 1, 9, 17, 25, 33, 41, 49, 57,
65, 73, 81, 89, 97, 105, 113, 121,
129, 137, 145, 153, 161, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 2, 10, 18, 26, 34, 42, 50, 58,
66, 74, 82, 90, 98, 106, 114, 122,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 3, 11, 19, 27, 35, 43, 51, 59,
67, 75, 83, 91, 99, 107, 115, 123,
131, 139, 147, 155, 163, 171, 179, 187,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 4, 12, 20, 28, 36, 44, 52, 60,
68, 76, 84, 92, 100, 108, 116, 124,
132, 140, 148, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 5, 13, 21, 29, 37, 45, 53, 61,
69, 77, 85, 93, 101, 109, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 6, 14, 22, 30, 38, 46, 54, 62,
70, 78, 86, 94, 102, 110, 118, 126,
134, 142, 150, 158, 166, 174, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
{ 7, 15, 23, 31, 39, 47, 55, 63,
71, 79, 87, 95, 103, 111, 119, 127,
135, -1, -1, -1, -1, -1, -1, -1,
-1, -1, -1, -1, -1, -1, -1, -1 },
}
};
private byte dmatrix[ DMATRIX_Y ][ DMATRIX_X ] = {
{
0x0e, 0x8e, 0x2e, 0xae, 0x06, 0x86, 0x26, 0xa6,
0x0c, 0x8c, 0x2c, 0xac, 0x04, 0x84, 0x24, 0xa4
},
{
0xce, 0x4e, 0xee, 0x6e, 0xc6, 0x46, 0xe6, 0x66,
0xcc, 0x4c, 0xec, 0x6c, 0xc4, 0x44, 0xe4, 0x64
},
{
0x3e, 0xbe, 0x1e, 0x9e, 0x36, 0xb6, 0x16, 0x96,
0x3c, 0xbc, 0x1c, 0x9c, 0x34, 0xb4, 0x14, 0x94
},
{
0xfe, 0x7e, 0xde, 0x5e, 0xf6, 0x76, 0xd6, 0x56,
0xfc, 0x7c, 0xdc, 0x5c, 0xf4, 0x74, 0xd4, 0x54
},
{
0x01, 0x81, 0x21, 0xa1, 0x09, 0x89, 0x29, 0xa9,
0x03, 0x83, 0x23, 0xa3, 0x0b, 0x8b, 0x2b, 0xab
},
{
0xc1, 0x41, 0xe1, 0x61, 0xc9, 0x49, 0xe9, 0x69,
0xc3, 0x43, 0xe3, 0x63, 0xcb, 0x4b, 0xeb, 0x6b
},
{
0x31, 0xb1, 0x11, 0x91, 0x39, 0xb9, 0x19, 0x99,
0x33, 0xb3, 0x13, 0x93, 0x3b, 0xbb, 0x1b, 0x9b
},
{
0xf1, 0x71, 0xd1, 0x51, 0xf9, 0x79, 0xd9, 0x59,
0xf3, 0x73, 0xd3, 0x53, 0xfb, 0x7b, 0xdb, 0x5b
},
{
0x0d, 0x8d, 0x2d, 0xad, 0x05, 0x85, 0x25, 0xa5,
0x0f, 0x8f, 0x2f, 0xaf, 0x07, 0x87, 0x27, 0xa7
},
{
0xcd, 0x4d, 0xed, 0x6d, 0xc5, 0x45, 0xe5, 0x65,
0xcf, 0x4f, 0xef, 0x6f, 0xc7, 0x47, 0xe7, 0x67
},
{
0x3d, 0xbd, 0x1d, 0x9d, 0x35, 0xb5, 0x15, 0x95,
0x3f, 0xbf, 0x1f, 0x9f, 0x37, 0xb7, 0x17, 0x97
},
{
0xfd, 0x7d, 0xdd, 0x5d, 0xf5, 0x75, 0xd5, 0x55,
0xff, 0x7f, 0xdf, 0x5f, 0xf7, 0x77, 0xd7, 0x57
},
{
0x02, 0x82, 0x22, 0xa2, 0x0a, 0x8a, 0x2a, 0xaa,
0x01, 0x80, 0x20, 0xa0, 0x08, 0x88, 0x28, 0xa8
},
{
0xc2, 0x42, 0xe2, 0x62, 0xca, 0x4a, 0xea, 0x6a,
0xc0, 0x40, 0xe0, 0x60, 0xc8, 0x48, 0xe8, 0x68
},
{
0x32, 0xb2, 0x12, 0x92, 0x3a, 0xba, 0x1a, 0x9a,
0x30, 0xb0, 0x10, 0x90, 0x38, 0xb8, 0x18, 0x98
},
{
0xf2, 0x72, 0xd2, 0x52, 0xfa, 0x7a, 0xda, 0x5a,
0xf0, 0x70, 0xd0, 0x50, 0xf8, 0x78, 0xd8, 0x58
}
};
static CCOMP ctable[] = {
{ -255, -255, 0, 0, 255 },
{ 102, 0, 255, 0, 0 },
{ 255, 255, 255, 255, 0 },
{ 560, 512, 0, 255, 0 },
{ 765, 765, 0, 255, 255 },
{ 1045, 1020, 0, 0, 255 },
{ 1275, 1275, 255, 0, 255 },
{ 1632, 1530, 255, 0, 0 }
};
static const unsigned char xtrans[ 256 ] = {
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 3, 3,
3, 3, 3, 3, 3, 4, 4, 4,
4, 4, 4, 5, 5, 5, 5, 5,
6, 6, 6, 6, 6, 7, 7, 7,
7, 8, 8, 8, 8, 9, 9, 9,
10, 10, 10, 11, 11, 11, 12, 12,
12, 13, 13, 13, 14, 14, 14, 15,
15, 16, 16, 17, 17, 17, 18, 18,
19, 19, 20, 20, 21, 21, 22, 22,
23, 23, 24, 24, 25, 26, 26, 27,
27, 28, 29, 29, 30, 30, 31, 32,
32, 33, 34, 34, 35, 36, 37, 37,
38, 39, 40, 40, 41, 42, 43, 44,
44, 45, 46, 47, 48, 49, 50, 51,
51, 52, 53, 54, 55, 56, 57, 58,
59, 60, 61, 62, 63, 64, 65, 67,
68, 69, 70, 71, 72, 73, 74, 76,
77, 78, 79, 80, 82, 83, 84, 86,
87, 88, 89, 91, 92, 94, 95, 96,
98, 99, 101, 102, 103, 105, 106, 108,
109, 111, 112, 114, 116, 117, 119, 120,
122, 124, 125, 127, 129, 130, 132, 134,
136, 137, 139, 141, 143, 145, 146, 148,
150, 152, 154, 156, 158, 160, 162, 164,
166, 168, 170, 172, 174, 176, 178, 180
};
private int photoex_open( DEV *pdev )
{
double height;
double width;
float margins[ 4 ];
height = pdev->height / pdev->y_pixels_per_inch;
width = pdev->width / pdev->x_pixels_per_inch;
margins[ 0 ] = 0.12;
margins[ 1 ] = 0.5;
margins[ 2 ] = 0.12;
margins[ 3 ] = ( width > 11.46+0.12 ) ? width - (11.46+0.12) : 0.12;
gx_device_set_margins( pdev, margins, true );
return( gdev_prn_open( pdev ) );
}
private CINX photoex_map_rgb_color( DEV *dev, CVAL r, CVAL g, CVAL b )
{
int c, y, m, k;
int a, s, f;
EDEV *edev;
int i;
edev = (EDEV *) dev;
if ( ( r & g & b ) == ( 1 << gx_color_value_bits ) - 1 ) {
return( BUILD_CMYK( 0, 0, 0, 0 ) );
}
if ( ( r | g | b ) == 0 ) {
return( BUILD_CMYK( 0, 0, 0, xtrans[ 0xff ] ) );
}
c = 255 - ( r >> ( gx_color_value_bits - 8 ) );
m = 255 - ( g >> ( gx_color_value_bits - 8 ) );
y = 255 - ( b >> ( gx_color_value_bits - 8 ) );
k = xtrans[ min( c, min( m, y ) ) ] * 0.8;
c -= k;
m -= k;
y -= k;
s = max ( c, max( y, m ) );
a = Cmy2A( c, m, y );
for ( i = 1 ; a > ctable[ i ].ra ; i++ );
f = ((a - ctable[ i-1 ].ra) << 16 ) / (ctable[ i ].ra - ctable[ i-1 ].ra);
c = (( ctable[i-1].c << 16 ) + ( ctable[i].c - ctable[i-1].c ) * f ) >> 16;
m = (( ctable[i-1].m << 16 ) + ( ctable[i].m - ctable[i-1].m ) * f ) >> 16;
y = (( ctable[i-1].y << 16 ) + ( ctable[i].y - ctable[i-1].y ) * f ) >> 16;
s = xtrans[ s ];
c = ( c * s ) >> 8;
m = ( m * s ) >> 8;
y = ( y * s ) >> 8;
return( BUILD_CMYK( c, m, y, k ) );
}
private int photoex_map_color_rgb( DEV *dev, CINX index, CVAL prgb[3] )
{
uint c, m, y, k;
CVAL r, g, b;
DECOMPOSE_CMYK( index, c, m, y, k );
k = index & 255;
y = ( index >> 8 ) & 255;
m = ( index >> 16 ) & 255;
c = ( index >> 24 ) & 255;
if ( MAP_RGB_ADOBE ) {
r = gx_max_color_value * ( 1.0 - min( 1.0, (c / 255.0 + k / 255.0) ) );
g = gx_max_color_value * ( 1.0 - min( 1.0, (m / 255.0 + k / 255.0) ) );
b = gx_max_color_value * ( 1.0 - min( 1.0, (y / 255.0 + k / 255.0) ) );
}
else {
r = gx_max_color_value * ( 1.0 - c / 255.0 ) * ( 1.0 - k / 255.0);
g = gx_max_color_value * ( 1.0 - m / 255.0 ) * ( 1.0 - k / 255.0);
b = gx_max_color_value * ( 1.0 - y / 255.0 ) * ( 1.0 - k / 255.0);
}
prgb[ 0 ] = r;
prgb[ 1 ] = g;
prgb[ 2 ] = b;
return( 0 );
}
private int Cmy2A( int c, int m, int y )
{
int black;
int maxim;
int a;
black = min( c, min( m, y ) );
c -= black;
m -= black;
y -= black;
if ( ! c && ! m && ! y ) return( 0 );
maxim = max( c, max( m, y ) );
c = ( 255 * c ) / maxim;
m = ( 255 * m ) / maxim;
y = ( 255 * y ) / maxim;
if ( c == 255 ) {
if ( ! y )
a = m;
else
a = 1530 - y;
}
else if ( m == 255 ) {
if ( ! c )
a = 510 + y;
else
a = 510 - c;
}
else {
if ( ! m )
a = 1020 + c;
else
a = 1020 - m;
}
return( a );
}
private int photoex_get_params( DEV *device, PLIST *plist )
{
int code;
EDEV *dev;
dev = (EDEV *) device;
code = gdev_prn_get_params( device, plist );
code = GetInt( plist, "Depletion", &dev->depletion, code );
code = GetInt( plist, "Shingling", &dev->shingling, code );
code = GetInt( plist, "Render", &dev->halftoner, code );
code = GetInt( plist, "Splash", &dev->splash, code );
code = GetInt( plist, "Leakage", &dev->leakage, code );
code = GetInt( plist, "Binhibit", &dev->pureblack, code );
code = GetInt( plist, "DotSize", &dev->dotsize, code );
return( code );
}
private int photoex_put_params( DEV *device, PLIST *plist )
{
int code;
EDEV *dev;
dev = (EDEV *) device;
code = 0;
code = PutInt( plist, "Depletion", &dev->depletion, 0, 2, code );
code = PutInt( plist, "Shingling", &dev->shingling, 0, 2, code );
code = PutInt( plist, "Render", &dev->halftoner, 0,MAXHTONE-1, code );
code = PutInt( plist, "Splash", &dev->splash, 0, 50, code );
code = PutInt( plist, "Leakage", &dev->leakage, 0, 25, code );
code = PutInt( plist, "Binhibit", &dev->pureblack, 0, 1, code );
code = PutInt( plist, "DotSize", &dev->dotsize, 0, 4, code );
if ( code < 0 )
return( code );
else
return( gdev_prn_put_params( device, plist ) );
}
private int PutInt( PLIST *plist, PNAME name, int *val,
int minval, int maxval, int code )
{
int new;
if ( code ) return( code );
new = *val;
switch ( code = param_read_int( plist, name, &new ) ) {
case 1:
code = 0;
break;
case 0:
if ( minval > new || new > maxval )
param_signal_error( plist, name, gs_error_rangecheck );
else
*val = new;
break;
default:
break;
}
return( code );
}
private int GetInt( PLIST *list, PNAME name, int *value, int code )
{
if ( code < 0 ) return( code );
return( param_write_int( list, name, value ) );
}
private int photoex_print_page( PDEV *device, FILE *stream )
{
int pixels;
int x;
EDEV *dev;
RENDER *render;
int xres, yres;
int start, width;
int unit;
double psize;
dev = (EDEV *) device;
yres = (int) dev->y_pixels_per_inch;
xres = (int) dev->x_pixels_per_inch;
if ( ! ( ( xres == 360 && yres == 360 ) ||
( xres == 720 && yres == 720 ) ||
( xres == 1440 && yres == 720 ) ) )
return( gs_error_rangecheck );
pixels = gdev_prn_raster( device ) / sizeof( long );
psize = device->height / device->y_pixels_per_inch;
start = 1440.0 * dev_l_margin( device );
x = xres == 360 ? 4 : xres == 720 ? 2 : 1;
if ( start + x * pixels > 2 * MAX_PIXELS ) {
width = ( 2 * MAX_PIXELS - start ) / x;
if ( width <= 0 ) return( gs_error_rangecheck );
}
else {
width = pixels;
}
if ( ! ( render = (RENDER *) gs_malloc( dev->memory, 1, sizeof( RENDER ), "PhotoEX" )))
return_error( gs_error_VMerror );
if ( ! ( render->dbuff = (byte *) gs_malloc( dev->memory, pixels, sizeof( long ),
"PhotoEX" ) ) ) {
gs_free( dev->memory, render, 1, sizeof( RENDER ), "PhotoEX" );
return_error( gs_error_VMerror );
}
render->dev = dev;
render->yres = yres;
render->xres = xres;
render->width = width;
render->lines = dev->height;
render->stream = stream;
render->mono = dev->mono;
SendReset( stream );
SendReset( stream );
SendGmode( stream, 1 );
unit = ( yres == 360 ) ? 360 : 720;
SendUnit( stream, RESCODE( unit ) );
SendPaper( stream, device->height / device->y_pixels_per_inch * unit );
SendMargin( stream, ( psize - dev_b_margin( device ) ) * unit,
dev_t_margin( device ) * unit );
if ( dev->dotsize )
SendInk( stream, dev->dotsize );
else
SendInk( stream, yres == 360 ? 3 : ( xres == 720 ? 2 : 1 ) );
SendMicro( stream, 0 );
SendUnidir( stream, 1 );
RenderPage( render );
SendByte( stream, FF );
SendReset( stream );
gs_free( dev->memory, render->dbuff, pixels, sizeof( long ), "PhotoEX" );
gs_free( dev->memory, render, 1, sizeof( RENDER ), "PhotoEX" );
return( 0 );
}
private void RenderPage( RENDER *p )
{
int last_done;
int last_need;
int move_down;
int last_band;
int min, max;
int phase;
int i, j, l, col;
p->htone_thold = HalftoneThold( p );
p->htone_last = -1 - p->htone_thold;
p->schedule.top = -1;
p->schedule.resol = p->xres;
p->schedule.last = p->lines;
last_done = -1;
move_down = 0;
do {
last_band = ScheduleLines( &p->schedule );
last_need = last_done;
for ( i = NOZZLES-1 ; i >= 0 && p->schedule.head[ i ] == -1 ; i-- );
if ( i >= 0 ) last_need = p->schedule.head[ i ];
while ( last_need > last_done ) RenderLine( p, ++last_done );
phase = p->schedule.offset;
for ( col = 0 ; col < DCOLN ; col++ ) {
min = MAX_BYTES;
max = 0;
for ( i = 0 ; i < NOZZLES && i < p->schedule.nozzle ; i++ ) {
if ( ( j = p->schedule.head[ i ] ) != -1 ) {
j %= MAX_MARK;
if ( p->raw[ phase ][ col ][ j ].first < min )
min = p->raw[ phase ][ col ][ j ].first;
if ( p->raw[ phase ][ col ][ j ].last > max )
max = p->raw[ phase ][ col ][ j ].last;
}
}
if ( min <= max ) {
max++;
if ( move_down ) {
SendDown( p->stream, move_down );
move_down = 0;
}
SendColour( p->stream, col );
if ( p->xres == 360 )
SendRight( p->stream, 4 * 8 * min );
else if ( p->xres == 720 )
SendRight( p->stream, 2 * 8 * min );
else
SendRight( p->stream, 8 * min + phase );
SendData( p->stream, p->xres, p->yres, p->schedule.nozzle,
( max-min ) * 8 );
for ( i = 0 ; i < p->schedule.nozzle ; i++ ) {
if ( ( j = p->schedule.head[ i ] ) == -1 ||
( p->raw[ phase ][ col ][ j % MAX_MARK ].last <
p->raw[ phase ][ col ][ j % MAX_MARK ].first ) ) {
l = RleCompress( NULL, min, max, p->rle );
}
else {
l = RleCompress( p->raw[ phase ][ col ] + j % MAX_MARK,
min, max, p->rle );
}
fwrite( p->rle, l, 1, p->stream );
}
SendByte( p->stream, CR );
}
}
move_down += p->schedule.down;
} while ( ! last_band );
}
private void RenderLine( RENDER *p, int line )
{
byte *data;
int i;
gdev_prn_get_bits( (PDEV *) p->dev, line, p->dbuff, &data );
if ( IsScanlineEmpty( p, data ) ) {
if ( line - p->htone_last > p->htone_thold ) {
for ( i = 0 ; i < DCOLN ; i++ ) {
p->raw[ 0 ][ i ][ line % MAX_MARK ].first = MAX_BYTES;
p->raw[ 0 ][ i ][ line % MAX_MARK ].last = 0;
p->raw[ 1 ][ i ][ line % MAX_MARK ].first = MAX_BYTES;
p->raw[ 1 ][ i ][ line % MAX_MARK ].last = 0;
}
}
else {
HalftoneLine( p, line, data );
}
}
else {
if ( line - p->htone_last >= p->htone_thold ) {
HalftonerStart( p, line );
}
HalftoneLine( p, line, data );
p->htone_last = line;
}
}
private int IsScanlineEmpty( RENDER *r, byte *line )
{
int i;
long *p;
p = (long *) line;
for ( i = 0 ; i < r->width ; i++ ) {
if ( *p++ ) return( FALSE );
}
return( TRUE );
}
private int ScheduleLines( SCHEDUL *p )
{
int i;
if ( p->top == -1 ) {
SchedulerInit( p );
}
if ( p->nozzle == 1 ) {
p->head[ 0 ] = p->top;
p->down = 1;
p->top++;
return( p->top == p->last );
}
for ( i = p->markbeg ; i < p->top ; i++ ) p->mark[ i % MAX_MARK ] = 0;
p->markbeg = p->top;
if ( p->top < HEAD_SPACING ) {
ScheduleLeading( p );
return( FALSE );
}
if ( p->top + p->resol + (NOZZLES) * HEAD_SPACING > p->last ) {
ScheduleTrailing( p );
if ( p->down )
return( p->top + (NOZZLES-1) * HEAD_SPACING >= p->last );
else
return( FALSE );
}
ScheduleMiddle( p );
return( FALSE );
}
private void SchedulerInit( SCHEDUL *p )
{
int i;
p->top = 0;
switch ( p->resol ) {
case 360:
p->offset = 0;
p->resol = BAND_360;
p->nozzle = NOZZLE_360;
break;
case 720:
p->offset = 0;
p->resol = BAND_720;
p->nozzle = NOZZLE_720;
break;
case 1440:
p->offset = 1;
p->resol = BAND_1440;
p->nozzle = NOZZLE_1440;
break;
}
for ( i = 0 ; i < NOZZLES ; i++ ) p->head[ i ] = -1;
for ( i = 0 ; i < MAX_MARK ; i++ ) p->mark[ i ] = 0;
p->markbeg = 0;
}
private void ScheduleLeading( SCHEDUL *p )
{
int i;
if ( p->resol == BAND_720 ) {
memcpy( p->head, start_720[ p->top ], sizeof( int ) * NOZZLES );
for ( i = 0 ; i < NOZZLES ; i++ )
if ( p->head[ i ] != -1 )
p->mark[ p->head[ i ] % MAX_MARK ] = 1;
if ( p->top == HEAD_SPACING - 1 ) {
p->down = BAND_720 - p->top;
p->top = BAND_720;
}
else {
p->down = 1;
p->top++;
}
}
else {
if ( p->offset ) {
memcpy( p->head, start_1440[0][p->top], sizeof( int ) * NOZZLES );
for ( i = 0 ; i < NOZZLES ; i++ )
if ( p->head[ i ] != -1 )
p->mark[ p->head[ i ] % MAX_MARK ] = 1;
p->offset = 0;
p->down = 0;
}
else {
memcpy( p->head, start_1440[1][p->top], sizeof( int ) * NOZZLES );
for ( i = 0 ; i < NOZZLES ; i++ )
if ( p->head[ i ] != -1 )
p->mark[ p->head[ i ] % MAX_MARK ] |= 2;
if ( p->top == HEAD_SPACING - 1 ) {
p->down = BAND_1440 - p->top;
p->top = BAND_1440;
}
else {
p->down = 1;
p->top++;
}
p->offset = 1;
}
}
}
private void ScheduleMiddle( SCHEDUL *p )
{
int ph0, ph1;
int line, mask;
int i;
if ( p->resol == BAND_720 ) {
ScheduleBand( p, 1 );
p->down = BAND_720;
p->top += BAND_720;
}
else {
ph0 = ph1 = 0;
for ( line = p->top, i=0 ; i < NOZZLES ; i++, line += HEAD_SPACING ) {
line = p->top + i * HEAD_SPACING;
ph0 += p->mark[ line % MAX_MARK ] & 1;
ph1 += p->mark[ line % MAX_MARK ] & 2;
}
ph1 >>= 1;
if ( ph0 <= ph1 ) {
p->offset = 0;
mask = 1;
}
else {
p->offset = 1;
mask = 2;
}
ScheduleBand( p, mask );
p->down = BAND_1440;
p->top += BAND_1440;
}
}
private void ScheduleTrailing( SCHEDUL *p )
{
int mask;
if ( p->down > 1 ) {
p->offset = 1;
}
if ( p->resol == BAND_720 ) {
p->offset = 0;
p->down = 1;
mask = 1;
}
else {
if ( p->offset ) {
p->offset = 0;
p->down = 0;
mask = 1;
}
else {
p->offset = 1;
p->down = 1;
mask = 2;
}
}
ScheduleBand( p, mask );
p->top += p->down;
}
private void ScheduleBand( SCHEDUL *p, int mask )
{
int i;
int line;
for ( line = p->top, i = 0 ; i < NOZZLES ; i++, line += HEAD_SPACING ) {
if ( p->mark[ line % MAX_MARK ] & mask ) {
p->head[ i ] = -1;
}
else {
p->head[ i ] = line;
p->mark[ line % MAX_MARK ] |= mask;
}
}
}
private void PackLine( byte *input, int pixnum, int lev_on, int step,
RAWLINE *line )
{
byte bits;
char *result;
int i, j, k;
result = line->data;
line->first = MAX_PIXELS;
line->last = 0;
for ( j = 0x80, bits = k = i = 0 ; i < pixnum ; i += step, input += step ){
if ( *input == lev_on ) bits |= j;
if ( ! ( j >>= 1 ) ) {
if ( bits ) {
if ( line->first > k ) line->first = k;
if ( line->last < k ) line->last = k;
}
*result++ = bits;
j = 0x80;
bits = 0;
k++;
}
}
if ( j != 0x80 ) {
*result = bits;
if ( bits ) {
if ( line->first > k ) line->first = k;
if ( line->last < k ) line->last = k;
}
}
}
private int RleCompress( RAWLINE *raw, int min, int max, byte *rle_data )
{
int i, n;
byte pbyte;
byte *start, *rstrt;
int length;
byte *input;
int len;
if ( ! raw ) {
for ( n = 0, i = max - min ; i >= 129 ; i -= 129 ) {
*rle_data++ = 128;
*rle_data++ = 0;
n += 2;
}
if ( i >= 2 ) {
*rle_data++ = 257 - i;
*rle_data++ = 0;
n += 2;
}
else if ( i ) {
*rle_data++ = 0;
*rle_data++ = 0;
n+= 2;
}
return( n );
}
input = raw->data + min;
len = max - min;
length = 0;
start = input;
rstrt = NULL;
pbyte = *input++;
for ( i = 1 ; i < len ; i++, input++ ) {
if ( *input == pbyte ) {
if ( ! rstrt ) {
rstrt = input - 1;
}
}
else {
if ( rstrt ) {
if ( rstrt - input < 4 ) {
rstrt = NULL;
}
else {
n = RleFlush( start, rstrt, input, rle_data );
rle_data += n;
length += n;
start = rle_data;
rstrt = NULL;
}
}
pbyte = *rle_data;
}
}
length += RleFlush( start, rstrt, input, rle_data );
return( length );
}
private int RleFlush( byte *first, byte *reps, byte *now, byte *out )
{
int count;
int l;
if ( ! first ) return( 0 );
if ( ! reps ) reps = now;
count = 0;
while ( ( l = reps - first ) ) {
if ( l > 128 ) {
*out++ = 127;
memcpy( out, first, 128 );
out += 128;
first += 128;
count += 129;
}
else {
*out++ = l - 1;
memcpy( out, first, l );
count += l + 1;
first += l;
out += l;
}
}
while ( ( l = now - reps ) ) {
if ( l > 128 ) {
*out++ = 128;
*out++ = *reps;
count += 2;
reps += 129;
}
else {
if ( l == 1 ) {
*out++ = 0;
*out++ = *reps;
count += 2;
reps++;
}
else {
*out++ = 257 - l;
*out++ = *reps;
count += 2;
reps = now;
}
}
}
return( count );
}
private void SendReset( FILE *stream )
{
SendString( stream, ESC "@" );
}
private void SendMargin( FILE *stream, int top, int bot )
{
SendString( stream, ESC "(c" );
SendWord( stream, 4 );
SendWord( stream, bot );
SendWord( stream, top );
}
private void SendPaper( FILE *stream, int length )
{
SendString( stream, ESC "(C" );
SendWord( stream, 2 );
SendWord( stream, length );
}
private void SendGmode( FILE *stream, int on )
{
SendString( stream, ESC "(G" );
SendWord( stream, 1 );
SendByte( stream, on );
}
private void SendUnit( FILE *stream, int res )
{
SendString( stream, ESC "(U" );
SendWord( stream, 1 );
SendByte( stream, res );
}
private void SendUnidir( FILE *stream, int on )
{
SendString( stream, ESC "U" );
SendByte( stream, on );
}
private void SendMicro( FILE *stream, int on )
{
SendString( stream, ESC "(i" );
SendWord( stream, 1 );
SendByte( stream, on );
}
private void SendInk( FILE *stream, int x )
{
SendString( stream, ESC "(e" );
SendWord( stream, 2 );
SendByte( stream, 0 );
SendByte( stream, x );
}
private void SendDown( FILE *stream, int x )
{
SendString( stream, ESC "(v" );
SendWord( stream, 2 );
SendWord( stream, x );
}
private void SendRight( FILE *stream, int amount )
{
SendString( stream, ESC "(\\" );
SendWord( stream, 4 );
SendWord( stream, 1440 );
SendWord( stream, amount );
}
private void SendColour( FILE *stream, int col )
{
static int ccode[] = { 0x000, 0x200, 0x100, 0x400, 0x201, 0x101 };
SendString( stream, ESC "(r" );
SendWord( stream, 2 );
SendWord( stream, ccode[ col ] );
}
private void SendData( FILE *stream, int hres, int vres, int noz, int col )
{
SendString( stream, ESC "." );
SendByte( stream, 1 );
if ( noz == 1 )
SendByte( stream, RESCODE( vres ) );
else
SendByte( stream, RESCODE( 90 ) );
if ( hres > 720 )
SendByte( stream, RESCODE( 720 ) );
else
SendByte( stream, RESCODE( hres ) );
SendByte( stream, noz );
SendWord( stream, col );
}
private void SendString( FILE *stream, const char *s )
{
while ( *s ) SendByte( stream, *s++ );
}
private void HalftonerStart( RENDER *render, int line )
{
(*(htable[ render->dev->halftoner ].hstrt))( render, line );
}
private int HalftoneThold( RENDER *render )
{
return( (*(htable[ render->dev->halftoner ].hthld))( render ) );
}
private void HalftoneLine( RENDER *render, int line, byte *data )
{
void (*htone)( HTONE *, int );
EDEV *dev;
int offs;
HTONE hdata;
short *errs[ MAX_ED_LINES ];
int i;
dev = render->dev;
htone = htable[ render->dev->halftoner ].htone;
offs = render->mono ? 0 : OFFS_K;
if ( dev->mono ) {
for ( i = 0 ; i < MAX_ED_LINES ; i++ )
errs[ i ] = render->error[ i ][ OFFS_K ];
hdata.render = render;
hdata.data = data + OFFS_K;
hdata.step = sizeof( byte );
hdata.res = render->res[ OFFS_K ];
hdata.block = NULL;
hdata.err = errs;
hdata.mval = 255;
(*htone)( &hdata, line );
}
else {
for ( i = 0 ; i < MAX_ED_LINES ; i++ )
errs[ i ] = render->error[ i ][ OFFS_K ];
hdata.render = render;
hdata.step = sizeof( long );
hdata.data = data + OFFS_K;
hdata.res = render->res[ OFFS_K ];
hdata.block = NULL;
hdata.err = errs;
hdata.mval = 255;
(*htone)( &hdata, line );
for ( i = 0 ; i < MAX_ED_LINES ; i++ )
errs[ i ] = render->error[ i ][ OFFS_Y ];
hdata.render = render;
hdata.step = sizeof( long );
hdata.data = data + OFFS_Y;
hdata.res = render->res[ OFFS_Y ];
hdata.block = dev->pureblack ? render->res[ OFFS_K ] : NULL;
hdata.err = errs;
hdata.mval = 255;
(*htone)( &hdata, line );
for ( i = 0 ; i < MAX_ED_LINES ; i++ )
errs[ i ] = render->error[ i ][ OFFS_C ];
hdata.data = data + OFFS_C;
hdata.res = render->res[ OFFS_C ];
hdata.block = dev->pureblack ? render->res[ OFFS_K ] : NULL;
hdata.mval = dev->midcyan;
(*htone)( &hdata, line );
for ( i = 0 ; i < MAX_ED_LINES ; i++ )
errs[ i ] = render->error[ i ][ OFFS_M ];
hdata.data = data + OFFS_M;
hdata.res = render->res[ OFFS_M ];
hdata.block = dev->pureblack ? render->res[ OFFS_K ] : NULL;
hdata.mval = dev->midmagenta;
(*htone)( &hdata, line );
}
if ( dev->mono ) {
if ( render->xres == 1440 ) {
PackLine( render->res[ OFFS_K ], render->width, 255, 2,
render->raw[ 0 ][ DEV_BLACK ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_K ]+1, render->width-1, 255, 2,
render->raw[ 1 ][ DEV_BLACK ]+ line % MAX_MARK );
}
else {
PackLine( render->res[ OFFS_K ], render->width, 255, 1,
render->raw[ 0 ][ DEV_BLACK ]+ line % MAX_MARK );
}
}
else {
if ( render->xres == 1440 ) {
PackLine( render->res[ OFFS_K ], render->width, 255, 2,
render->raw[ 0 ][ DEV_BLACK ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_K ]+1, render->width-1, 255, 2,
render->raw[ 1 ][ DEV_BLACK ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_C ], render->width, 255, 2,
render->raw[ 0 ][ DEV_CYAN ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_C ]+1, render->width-1, 255, 2,
render->raw[ 1 ][ DEV_CYAN ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_M ], render->width, 255, 2,
render->raw[ 0 ][ DEV_MAGENTA ]+ line % MAX_MARK);
PackLine( render->res[ OFFS_M ]+1, render->width-1, 255, 2,
render->raw[ 1 ][ DEV_MAGENTA ]+ line % MAX_MARK);
PackLine( render->res[ OFFS_Y ], render->width, 255, 2,
render->raw[ 0 ][ DEV_YELLOW ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_Y ]+1, render->width-1, 255, 2,
render->raw[ 1 ][ DEV_YELLOW ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_C ], render->width, dev->midcyan,
2, render->raw[ 0 ][ DEV_LCYAN ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_C ]+1, render->width-1, dev->midcyan,
2, render->raw[ 1 ][ DEV_LCYAN ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_M ], render->width, dev->midmagenta,
2, render->raw[0][ DEV_LMAGENTA ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_M ]+1, render->width-1,dev->midmagenta,
2, render->raw[1][ DEV_LMAGENTA ]+ line % MAX_MARK );
}
else {
PackLine( render->res[ OFFS_K ], render->width, 255, 1,
render->raw[ 0 ][ DEV_BLACK ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_C ], render->width, 255, 1,
render->raw[ 0 ][ DEV_CYAN ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_M ], render->width, 255, 1,
render->raw[ 0 ][ DEV_MAGENTA ]+ line % MAX_MARK);
PackLine( render->res[ OFFS_Y ], render->width, 255, 1,
render->raw[ 0 ][ DEV_YELLOW ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_C ], render->width, dev->midcyan,
1, render->raw[ 0 ][ DEV_LCYAN ]+ line % MAX_MARK );
PackLine( render->res[ OFFS_M ], render->width, dev->midmagenta,
1, render->raw[0][ DEV_LMAGENTA ]+ line % MAX_MARK );
}
}
(*htable[ render->dev->halftoner ].hteol)( render, line );
}
private int FloydSThold( RENDER *p )
{
return( 5 );
}
private void FloydSStart( RENDER *p, int line )
{
memset( p->err, 0, ICOLN * MAX_PIXELS*2 );
p->error[ 0 ] = p->err[ 0 ];
}
private void FloydSEol( RENDER *p, int line )
{
}
private void FloydSLine( HTONE *htone, int y )
{
int x;
int pixel;
int pixerr;
int length;
byte *res;
byte *data;
byte *block;
int lim1, lim2;
short e0, e1;
short *l0;
length = htone->render->width;
res = htone->res;
data = htone->data;
block = htone->block;
lim1 = htone->mval / 2;
lim2 = ( htone->mval + 256 ) / 2;
l0 = htone->err[ 0 ];
e0 = l0[ 1 ];
e1 = l0[ 2 ];
l0[ 1 ] = 0;
l0[ 2 ] = 0;
for ( x = 0 ; x < length ; x++ ) {
*res = 0;
pixel = ( ( *data << 4 ) + e0 );
e0 = e1;
e1 = l0[ 3 ] + ( pixel & 15 );
l0[ 3 ] = 0;
pixel >>= 4;
if ( ( block && *block ) || ( pixel < lim1 ) )
*res = 0;
else if ( pixel >= lim2 )
*res = 255;
else
*res = htone->mval;
pixerr = pixel - *res;
e0 += ( pixerr << 3 ) - pixerr;
l0[ 0 ] += ( pixerr << 2 ) - pixerr;
l0[ 1 ] += ( pixerr << 2 ) + pixerr;
l0[ 2 ] += pixerr;
res++;
if ( block ) block++;
data += htone->step;
l0++;
}
}
private int DitherThold( RENDER *p )
{
return( 0 );
}
private void DitherStart( RENDER *p, int line )
{
}
private void DitherEol( RENDER *p, int line )
{
}
private void DitherLine( HTONE *htone, int y )
{
int x;
int pixel;
int length;
byte *res;
byte *data;
byte *block;
byte *matrix;
int mx;
int lval, hval;
length = htone->render->width;
res = htone->res;
data = htone->data;
block = htone->block;
matrix = dmatrix[ y % DMATRIX_Y ];
for ( mx = x = 0 ; x < length ; x++ ) {
*res = 0;
if ( ( pixel = *data ) > htone->mval ) {
lval = htone->mval;
hval = 255;
if ( htone->mval == 127 )
pixel = ( ( pixel - htone->mval ) * 2 - 1 ) / 2;
else
pixel = ( pixel - htone->mval ) * 255 / ( 255 - htone->mval );
}
else {
lval = 0;
hval = htone->mval;
if ( htone->mval != 255 ) {
if ( htone->mval == 127 )
pixel = ( pixel * 4 + 1 ) / 2;
else
pixel = pixel * 255 / htone->mval;
}
}
if ( block && *block ) {
*res = 0;
}
else {
if ( pixel >= matrix[ mx ] )
*res = hval;
else
*res = lval;
}
res++;
if ( ++mx == DMATRIX_X ) mx = 0;
if ( block ) block++;
data += htone->step;
}
}
private int BendorThold( RENDER *p )
{
return( 5 );
}
private void BendorStart( RENDER *p, int line )
{
memset( p->err, 0, 2 * ICOLN * MAX_PIXELS*2 );
p->error[ 0 ] = p->err[ 0 ];
p->error[ 1 ] = p->err[ 1 ];
}
private void BendorEol( RENDER *p, int line )
{
void *x;
x = p->error[ 0 ];
p->error[ 0 ] = p->error[ 1 ];
p->error[ 1 ] = x;
}
private void BendorLine( HTONE *htone, int y )
{
int x;
int pixel;
int pixerr;
int pixe14;
int sval;
int splash;
int leakage;
int length;
byte *res;
byte *data;
byte *block;
int lim1, lim2;
short e0, e1;
short *l0, *l1;
splash = htone->render->dev->splash;
leakage = htone->render->dev->splash;
length = htone->render->width;
res = htone->res;
data = htone->data;
block = htone->block;
lim1 = htone->mval / 2;
lim2 = ( htone->mval + 256 ) / 2;
l0 = htone->err[ 0 ];
l1 = htone->err[ 1 ];
e0 = l0[ 2 ];
e1 = l0[ 3 ];
l0[ 2 ] = 0;
l0[ 3 ] = 0;
for ( x = 0 ; x < length ; x++ ) {
*res = 0;
pixel = ( ( *data << 7 ) + e0 );
e0 = e1;
e1 = l0[ 4 ] + ( pixel & 127 );
l0[ 4 ] = 0;
pixel >>= 7;
if ( ( block && *block ) || ( pixel < lim1 ) )
*res = 0;
else if ( pixel >= lim2 )
*res = 255;
else
*res = htone->mval;
pixerr = pixel - *res;
if ( leakage ) pixerr -= ( pixerr * leakage ) / 100;
pixerr <<= 1;
pixe14 = pixerr;
pixerr <<= 1;
pixe14 += pixerr;
l0[ 0 ] += pixerr;
l0[ 4 ] += pixerr;
pixerr <<= 1;
pixe14 += pixerr;
l0[ 1 ] += pixerr;
l0[ 3 ] += pixerr;
l1[ 0 ] += pixerr;
l1[ 4 ] += pixerr;
pixerr += pixerr >> 2;
l0[ 2 ] += pixerr;
e1 += pixerr;
pixerr <<= 1;
l1[ 2 ] += pixerr;
e0 += pixerr;
l1[ 1 ] += pixe14;
l1[ 3 ] += pixe14;
if ( splash && *res ) {
sval = splash * *res;
l1[ 1 ] -= sval;
l1[ 3 ] -= sval;
sval += sval >> 1;
e0 -= sval;
l1[ 2 ] -= sval;
}
res++;
if ( block ) block++;
data += htone->step;
l0++, l1++;
}
}