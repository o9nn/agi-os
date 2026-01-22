#define	SQMAGIC	(ulong)0xFEEF0F1E
typedef struct Sqhdr Sqhdr;
struct Sqhdr {
uchar	magic[4];
uchar	text[4];
uchar	data[4];
uchar	asis[4];
uchar	toptxt[4];
uchar	topdat[4];
uchar	sum[4];
uchar	flags[4];
};
#define	SQHDRLEN	(8*4)
#define	QREMAP(X)\
switch((X)>>26){\
case 19: case 31: case 59: case 63:\
(X) = (((X) & 0xFC00F801) | (((X)>>15)&0x7FE) | (((X)&0x7FE)<<15));\
}