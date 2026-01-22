#ifndef gdevsgi_INCLUDED
#  define gdevsgi_INCLUDED
#define IMAGIC 	0732
#define CM_NORMAL		0
#define CM_DITHERED		1
#define CM_SCREEN		2
#define CM_COLORMAP		3
#define TYPEMASK		0xff00
#define BPPMASK			0x00ff
#define ITYPE_VERBATIM		0x0000
#define ITYPE_RLE		0x0100
#define ISRLE(type)		(((type) & 0xff00) == ITYPE_RLE)
#define ISVERBATIM(type)	(((type) & 0xff00) == ITYPE_VERBATIM)
#define BPP(type)		((type) & BPPMASK)
#define RLE(bpp)		(ITYPE_RLE | (bpp))
#define VERBATIM(bpp)		(ITYPE_VERBATIM | (bpp))
#define	IBUFSIZE(pixels)	((pixels+(pixels>>6))<<2)
#define	RLE_NOP			0x00
#define	ierror(p)		(((p)->flags&_IOERR)!=0)
#define	ifileno(p)		((p)->file)
#define	getpix(p)		(--(p)->cnt>=0 ? *(p)->ptr++ : ifilbuf(p))
#define putpix(p,x)		(--(p)->cnt>=0 \
? ((int)(*(p)->ptr++=(unsigned)(x))) \
: iflsbuf(p,(unsigned)(x)))
typedef struct {
unsigned short	imagic;
unsigned short 	type;
unsigned short 	dim;
unsigned short 	xsize;
unsigned short 	ysize;
unsigned short 	zsize;
unsigned long 	min_color;
unsigned long 	max_color;
unsigned long	wastebytes;
char 		name[80];
unsigned long	colormap;
long 		file;
unsigned short 	flags;
short		dorev;
short		x;
short		y;
short		z;
short		cnt;
unsigned short	*ptr;
unsigned short	*base;
unsigned short	*tmpbuf;
unsigned long	offset;
unsigned long	rleend;
unsigned long	*rowstart;
long		*rowsize;
} IMAGE;
#endif