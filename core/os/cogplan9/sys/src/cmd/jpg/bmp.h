#define BMP_RGB 0
#define BMP_RLE8 1
#define BMP_RLE4 2
#define BMP_BITFIELDS 3
typedef struct {
uchar red;
uchar green;
uchar blue;
uchar alpha;
} Rgb;
#define Filehdrsz 14
typedef struct {
short type;
long size;
short reserved1;
short reserved2;
long offbits;
} Filehdr;
typedef struct {
long size;
long lReserved;
long dataoff;
long hsize;
long width;
long height;
short planes;
short bpp;
long compression;
long imagesize;
long hres;
long vres;
long colours;
long impcolours;
} Infohdr;