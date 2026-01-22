typedef struct Rawimage Rawimage;
struct Rawimage
{
Rectangle	r;
uchar	*cmap;
int		cmaplen;
int		nchans;
uchar	*chans[4];
int		chandesc;
int		chanlen;
int		fields;
int		gifflags;
int		gifdelay;
int		giftrindex;
int		gifloopcount;
};
enum
{
CRGB	= 0,
CYCbCr	= 1,
CY	= 2,
CRGB1	= 3,
CRGBV	= 4,
CRGB24	= 5,
CRGBA32	= 6,
CYA16	= 7,
CRGBVA16= 8,
TRANSP	= 1,
INPUT	= 2,
DISPMASK = 7<<2
};
enum{
II_GAMMA =	1 << 0,
II_COMMENT =	1 << 1,
};
typedef struct ImageInfo {
ulong	fields_set;
double	gamma;
char	*comment;
} ImageInfo;
Rawimage**	readjpg(int, int);
Rawimage**	Breadjpg(Biobuf *b, int);
Rawimage**	readpng(int, int);
Rawimage**	Breadpng(Biobuf *b, int);
Rawimage**	readgif(int, int);
Rawimage**	readpixmap(int, int);
Rawimage*	torgbv(Rawimage*, int);
Rawimage*	totruecolor(Rawimage*, int);
int		writerawimage(int, Rawimage*);
void*		_remaperror(char*, ...);
typedef struct Memimage Memimage;
char*		startgif(Biobuf*, Image*, int);
char*		writegif(Biobuf*, Image*, char*, int, int);
void		endgif(Biobuf*);
char*		memstartgif(Biobuf*, Memimage*, int);
char*		memwritegif(Biobuf*, Memimage*, char*, int, int);
void		memendgif(Biobuf*);
Image*		onechan(Image*);
Memimage*	memonechan(Memimage*);
char*		writeppm(Biobuf*, Image*, char*);
char*		memwriteppm(Biobuf*, Memimage*, char*);
Image*		multichan(Image*);
Memimage*	memmultichan(Memimage*);
char*		memwritepng(Biobuf*, Memimage*, ImageInfo*);