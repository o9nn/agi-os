#define DIR "/lib/sky"
typedef enum
{
Planet,
Patch,
SAO,
NGC,
M,
Constel_deprecated,
Nonstar_deprecated,
NamedSAO,
NamedNGC,
NamedAbell,
Abell,
Galaxy,
PlanetaryN,
OpenCl,
GlobularCl,
DiffuseN,
NebularCl,
Asterism,
Knot,
Triple,
Double,
Single,
Uncertain,
Nonexistent,
Unknown,
PlateDefect,
NGCN,
PatchC,
NONGC,
}Type;
enum
{
Pppo1 = 0,
Pppo2,
Pppo3,
Pppo4,
Pppo5,
Pppo6,
Pamdx1,
Pamdx2,
Pamdx3,
Pamdx4,
Pamdx5,
Pamdx6,
Pamdx7,
Pamdx8,
Pamdx9,
Pamdx10,
Pamdx11,
Pamdx12,
Pamdx13,
Pamdx14,
Pamdx15,
Pamdx16,
Pamdx17,
Pamdx18,
Pamdx19,
Pamdx20,
Pamdy1,
Pamdy2,
Pamdy3,
Pamdy4,
Pamdy5,
Pamdy6,
Pamdy7,
Pamdy8,
Pamdy9,
Pamdy10,
Pamdy11,
Pamdy12,
Pamdy13,
Pamdy14,
Pamdy15,
Pamdy16,
Pamdy17,
Pamdy18,
Pamdy19,
Pamdy20,
Ppltscale,
Pxpixelsz,
Pypixelsz,
Ppltra,
Ppltrah,
Ppltram,
Ppltras,
Ppltdec,
Ppltdecd,
Ppltdecm,
Ppltdecs,
Pnparam,
};
#define UNKNOWNMAG 32767
#define NPlanet 20
typedef float Angle;
typedef long DAngle;
typedef short Mag;
typedef long Key;
typedef struct NGCrec NGCrec;
struct NGCrec{
DAngle ra;
DAngle dec;
DAngle dummy1;
DAngle diam;
Mag mag;
short ngc;
char diamlim;
char type;
char magtype;
char dummy2;
char desc[52];
};
typedef struct Abellrec Abellrec;
struct Abellrec{
DAngle ra;
DAngle dec;
DAngle glat;
DAngle glong;
Mag mag10;
short abell;
DAngle rad;
short pop;
short dist;
char distgrp;
char richgrp;
char flag;
char pad;
};
typedef struct Planetrec Planetrec;
struct Planetrec{
DAngle ra;
DAngle dec;
DAngle az;
DAngle alt;
DAngle semidiam;
double phase;
char name[16];
};
typedef struct SAOrec SAOrec;
struct SAOrec{
DAngle ra;
DAngle dec;
DAngle dra;
DAngle ddec;
Mag mag;
Mag mpg;
char spec[3];
char code;
char compid[2];
char hdcode;
char pad1;
long hd;
char name[3];
char nname;
};
typedef struct Mindexrec Mindexrec;
struct Mindexrec{
char m;
char dummy;
short ngc;
};
typedef struct Bayerec Bayerec;
struct Bayerec{
long sao;
char name[3];
char pad;
};
typedef struct Namedrec Namedrec;
struct Namedrec{
char name[36];
};
typedef struct Namerec Namerec;
struct Namerec{
long sao;
long ngc;
long abell;
char name[36];
};
typedef struct Patchrec Patchrec;
struct Patchrec{
int nkey;
long key[60];
};
typedef struct Record Record;
struct Record{
Type type;
long index;
union{
SAOrec sao;
NGCrec ngc;
Abellrec abell;
Namedrec named;
Patchrec patch;
Planetrec planet;
};
};
typedef struct Name Name;
struct Name{
char *name;
int type;
};
typedef struct Plate Plate;
struct Plate
{
char rgn[7];
char disk;
Angle ra;
Angle dec;
};
typedef struct Header Header;
struct Header
{
float param[Pnparam];
int amdflag;
float x;
float y;
float xi;
float eta;
};
typedef long Pix;
typedef struct Img Img;
struct Img
{
int nx;
int ny;
Pix a[1];
};
#define RAD(x) ((x)*PI_180)
#define DEG(x) ((x)/PI_180)
#define ARCSECONDS_PER_RADIAN (DEG(1)*3600)
#define MILLIARCSEC (1000*60*60)
int nplate;
Plate plate[2000];
double PI_180;
double TWOPI;
double LN2;
int debug;
struct
{
float min;
float max;
float gamma;
float absgamma;
float mult1;
float mult2;
int neg;
} gam;
typedef struct Picture Picture;
struct Picture
{
int minx;
int miny;
int maxx;
int maxy;
char name[16];
uchar *data;
};
typedef struct Image Image;
extern double PI_180;
extern double TWOPI;
extern char *progname;
extern char *desctab[][2];
extern Name names[];
extern Record *rec;
extern long nrec;
extern Planetrec *planet;
extern int folded;
extern DAngle ramin;
extern DAngle ramax;
extern DAngle decmin;
extern DAngle decmax;
extern Biobuf bout;
extern void saoopen(void);
extern void ngcopen(void);
extern void patchopen(void);
extern void mopen(void);
extern void constelopen(void);
extern void lowercase(char*);
extern void lookup(char*, int);
extern int typetab(int);
extern char*ngcstring(int);
extern char*skip(int, char*);
extern void prrec(Record*);
extern int equal(char*, char*);
extern int parsename(char*);
extern void radec(int, int*, int*, int*);
extern int btag(short);
extern long patcha(Angle, Angle);
extern long patch(int, int, int);
extern char*hms(Angle);
extern char*dms(Angle);
extern char*ms(Angle);
extern char*hm(Angle);
extern char*dm(Angle);
extern char*deg(Angle);
extern char*hm5(Angle);
extern long dangle(Angle);
extern Angle angle(DAngle);
extern void prdesc(char*, char*(*)[2], short*);
extern double xsqrt(double);
extern Angle dist(Angle, Angle, Angle, Angle);
extern Header* getheader(char*);
extern char* getword(char*, char*);
extern void amdinv(Header*, Angle, Angle, float, float);
extern void ppoinv(Header*, Angle, Angle);
extern void xypos(Header*, Angle, Angle, float, float);
extern void traneqstd(Header*, Angle, Angle);
extern Angle getra(char*);
extern Angle getdec(char*);
extern void getplates(void);
extern Img* dssread(char*);
extern void hinv(Pix*, int, int);
extern int input_bit(Biobuf*);
extern int input_nbits(Biobuf*, int);
extern int input_huffman(Biobuf*);
extern int input_nybble(Biobuf*);
extern void qtree_decode(Biobuf*, Pix*, int, int, int, int);
extern void start_inputing_bits(void);
extern Picture* image(Angle, Angle, Angle, Angle);
extern char* dssmount(int);
extern int dogamma(Pix);
extern void displaypic(Picture*);
extern void displayimage(Image*);
extern void plot(char*);
extern void astro(char*, int);
extern char* alpha(char*, char*);
extern char* skipbl(char*);
extern void flatten(void);
extern int bbox(long, long, int);
extern int inbbox(DAngle, DAngle);
extern char* nameof(Record*);
#define NINDEX 400