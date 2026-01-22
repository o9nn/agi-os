#pragma src "/sys/src/libbio"
#pragma lib "libbio.a"
typedef struct Biobuf Biobuf;
typedef struct Biobufhdr Biobufhdr;
enum
{
Bsize = 8*1024,
Bungetsize = UTFmax+1,
Bmagic = 0x314159,
Beof = -1,
Bbad = -2,
Binactive = 0,
Bractive,
Bwactive,
Bracteof,
};
struct Biobufhdr
{
int icount;
int ocount;
int rdline;
int runesize;
int state;
int fid;
int flag;
vlong offset;
int bsize;
uchar* bbuf;
uchar* ebuf;
uchar* gbuf;
};
struct Biobuf
{
Biobufhdr;
uchar b[Bungetsize+Bsize];
};
#define BGETC(bp) Bgetc(bp)
#define BPUTC(bp,c) Bputc(bp,c)
#define BOFFSET(bp) Boffset(bp)
#define BLINELEN(bp) Blinelen(bp)
#define BFILDES(bp) Bfildes(bp)
int Bbuffered(Biobufhdr*);
int Bfildes(Biobufhdr*);
int Bflush(Biobufhdr*);
int Bgetc(Biobufhdr*);
int Bgetd(Biobufhdr*, double*);
long Bgetrune(Biobufhdr*);
int Binit(Biobuf*, int, int);
int Binits(Biobufhdr*, int, int, uchar*, int);
int Blinelen(Biobufhdr*);
vlong Boffset(Biobufhdr*);
Biobuf* Bopen(char*, int);
int Bprint(Biobufhdr*, char*, ...);
int Bvprint(Biobufhdr*, char*, va_list);
int Bputc(Biobufhdr*, int);
int Bputrune(Biobufhdr*, long);
void* Brdline(Biobufhdr*, int);
char* Brdstr(Biobufhdr*, int, int);
long Bread(Biobufhdr*, void*, long);
vlong Bseek(Biobufhdr*, vlong, int);
int Bterm(Biobufhdr*);
int Bungetc(Biobufhdr*);
int Bungetrune(Biobufhdr*);
long Bwrite(Biobufhdr*, void*, long);
#pragma varargck argpos Bprint 2