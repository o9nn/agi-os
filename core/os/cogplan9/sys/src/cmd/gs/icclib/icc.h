#ifndef ICC_H
#define ICC_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <math.h>
#include <time.h>
#include <sys/types.h>
#ifdef ICCLIB_SHARED
# ifdef ICCLIB_EXPORTS
# ifdef NT
# define ICCLIB_API __declspec(dllexport)
# endif
# else
# ifdef NT
# define ICCLIB_API __declspec(dllimport)
# ifdef ICCLIB_DEBUG
# pragma comment (lib, "icclibd.lib")
# else
# pragma comment (lib, "icclib.lib")
# endif
# endif
# endif
#else
# define ICCLIB_API
#endif
#ifdef __cplusplus
extern "C" {
#endif
#ifndef INR8
#define INR8 signed char
#endif
#ifndef INR16
#define INR16 signed short
#endif
#ifndef INR32
#define INR32 signed long
#endif
#ifndef ORD8
#define ORD8 unsigned char
#endif
#ifndef ORD16
#define ORD16 unsigned short
#endif
#ifndef ORD32
#define ORD32 unsigned long
#endif
#include "icc9809.h"
#define ICM_FILE_BASE \
\
\
\
int (*seek) (struct _icmFile *p, long int offset); \
\
\
size_t (*read) (struct _icmFile *p, void *buffer, size_t size, size_t count); \
\
\
size_t (*write)(struct _icmFile *p, void *buffer, size_t size, size_t count); \
\
\
int (*flush)(struct _icmFile *p); \
\
\
int (*del)(struct _icmFile *p); \
struct _icmFile {
ICM_FILE_BASE
}; typedef struct _icmFile icmFile;
struct _icmFileStd {
ICM_FILE_BASE
FILE *fp;
int doclose;
}; typedef struct _icmFileStd icmFileStd;
icmFile *new_icmFileStd_name(char *name, char *mode);
icmFile *new_icmFileStd_fp(FILE *fp);
struct _icmFileMem {
ICM_FILE_BASE
unsigned char *start, *cur, *end;
}; typedef struct _icmFileMem icmFileMem;
icmFile *new_icmFileMem(void *base, size_t length);
#define ICM_ALLOC_BASE \
\
\
void *(*malloc) (struct _icmAlloc *p, size_t size); \
void *(*calloc) (struct _icmAlloc *p, size_t num, size_t size); \
void *(*realloc)(struct _icmAlloc *p, void *ptr, size_t size); \
void (*free) (struct _icmAlloc *p, void *ptr); \
\
\
void (*del)(struct _icmAlloc *p); \
struct _icmAlloc {
ICM_ALLOC_BASE
}; typedef struct _icmAlloc icmAlloc;
struct _icmAllocStd {
ICM_ALLOC_BASE
}; typedef struct _icmAllocStd icmAllocStd;
icmAlloc *new_icmAllocStd(void);
#define MAX_CHAN 15
typedef int icmSig;
typedef struct {
ORD32 l;
INR32 h;
} icmInt64;
typedef struct {
ORD32 l,h;
} icmUint64;
typedef struct {
double X;
double Y;
double Z;
} icmXYZNumber;
typedef struct {
double deviceValue;
double measurement;
} icmResponse16Number;
#define ICM_BASE_MEMBERS \
\
icTagTypeSignature ttype; \
struct _icc *icp; \
int touched; \
int refcount; \
unsigned int (*get_size)(struct _icmBase *p); \
int (*read)(struct _icmBase *p, unsigned long len, unsigned long of); \
int (*write)(struct _icmBase *p, unsigned long of); \
void (*del)(struct _icmBase *p); \
\
\
void (*dump)(struct _icmBase *p, FILE *op, int verb); \
int (*allocate)(struct _icmBase *p);
struct _icmBase {
ICM_BASE_MEMBERS
}; typedef struct _icmBase icmBase;
struct _icmUInt8Array {
ICM_BASE_MEMBERS
unsigned int _size;
unsigned long size;
unsigned int *data;
}; typedef struct _icmUInt8Array icmUInt8Array;
struct _icmUInt16Array {
ICM_BASE_MEMBERS
unsigned int _size;
unsigned long size;
unsigned int *data;
}; typedef struct _icmUInt16Array icmUInt16Array;
struct _icmUInt32Array {
ICM_BASE_MEMBERS
unsigned int _size;
unsigned long size;
unsigned int *data;
}; typedef struct _icmUInt32Array icmUInt32Array;
struct _icmUInt64Array {
ICM_BASE_MEMBERS
unsigned int _size;
unsigned long size;
icmUint64 *data;
}; typedef struct _icmUInt64Array icmUInt64Array;
struct _icmU16Fixed16Array {
ICM_BASE_MEMBERS
unsigned int _size;
unsigned long size;
double *data;
}; typedef struct _icmU16Fixed16Array icmU16Fixed16Array;
struct _icmS15Fixed16Array {
ICM_BASE_MEMBERS
unsigned int _size;
unsigned long size;
double *data;
}; typedef struct _icmS15Fixed16Array icmS15Fixed16Array;
struct _icmXYZArray {
ICM_BASE_MEMBERS
unsigned int _size;
unsigned long size;
icmXYZNumber *data;
}; typedef struct _icmXYZArray icmXYZArray;
typedef enum {
icmCurveUndef = -1,
icmCurveLin = 0,
icmCurveGamma = 1,
icmCurveSpec = 2
} icmCurveStyle;
typedef struct {
int inited;
double rmin, rmax;
double qscale;
long rsize;
int **rlists;
unsigned long size;
double *data;
} icmRevTable;
struct _icmCurve {
ICM_BASE_MEMBERS
unsigned int _size;
icmRevTable rt;
icmCurveStyle flag;
unsigned long size;
double *data;
int (*lookup_fwd) (struct _icmCurve *p, double *out, double *in);
int (*lookup_bwd) (struct _icmCurve *p, double *out, double *in);
}; typedef struct _icmCurve icmCurve;
typedef enum {
icmDataUndef = -1,
icmDataASCII = 0,
icmDataBin = 1
} icmDataStyle;
struct _icmData {
ICM_BASE_MEMBERS
unsigned int _size;
icmDataStyle flag;
unsigned long size;
unsigned char *data;
}; typedef struct _icmData icmData;
struct _icmText {
ICM_BASE_MEMBERS
unsigned int _size;
unsigned long size;
char *data;
}; typedef struct _icmText icmText;
struct _icmDateTimeNumber {
ICM_BASE_MEMBERS
unsigned int year;
unsigned int month;
unsigned int day;
unsigned int hours;
unsigned int minutes;
unsigned int seconds;
}; typedef struct _icmDateTimeNumber icmDateTimeNumber;
#ifdef NEW
/ * DeviceSettings */
struct _icmSettingStruct {
ICM_BASE_MEMBERS
unsigned int _num;
icSettingsSig settingSig;
unsigned long numSettings;
union {
icUInt64Number *resolution;
icDeviceMedia *media;
icDeviceDither *halftone;
}
}; typedef struct _icmSettingStruct icmSettingStruct;
struct _icmSettingComb {
unsigned int _num;
unsigned long numStructs;
icmSettingStruct *data;
}; typedef struct _icmSettingComb icmSettingComb;
struct _icmPlatformEntry {
unsigned int _num;
icPlatformSignature platform;
unsigned long numCombinations;
icmSettingComb *data;
}; typedef struct _icmPlatformEntry icmPlatformEntry;
struct _icmDeviceSettings {
unsigned int _num;
unsigned long numPlatforms;
icmPlatformEntry *data;
}; typedef struct _icmDeviceSettings icmDeviceSettings;
#endif
struct _icmLut {
ICM_BASE_MEMBERS
int dinc[MAX_CHAN];
int dcube[1 << MAX_CHAN];
icmRevTable rit;
icmRevTable rot;
unsigned int inputTable_size;
unsigned int clutTable_size;
unsigned int outputTable_size;
void (*min_max) (struct _icmLut *pp, double *minv, double *maxv, int chan);
int (*lookup_matrix) (struct _icmLut *pp, double *out, double *in);
int (*lookup_input) (struct _icmLut *pp, double *out, double *in);
int (*lookup_clut_nl) (struct _icmLut *pp, double *out, double *in);
int (*lookup_clut_sx) (struct _icmLut *pp, double *out, double *in);
int (*lookup_output) (struct _icmLut *pp, double *out, double *in);
int (*nu_matrix) (struct _icmLut *pp);
unsigned int inputChan;
unsigned int outputChan;
unsigned int clutPoints;
unsigned int inputEnt;
unsigned int outputEnt;
double e[3][3];
double *inputTable;
double *clutTable;
double *outputTable;
int (*set_tables) (
struct _icmLut *p,
void *cbctx,
icColorSpaceSignature insig,
icColorSpaceSignature outsig,
void (*infunc)(void *cbctx, double *out, double *in),
double *inmin, double *inmax,
void (*clutfunc)(void *cbntx, double *out, double *in),
double *clutmin, double *clutmax,
void (*outfunc)(void *cbntx, double *out, double *in));
}; typedef struct _icmLut icmLut;
struct _icmMeasurement {
ICM_BASE_MEMBERS
icStandardObserver observer;
icmXYZNumber backing;
icMeasurementGeometry geometry;
double flare;
icIlluminant illuminant;
}; typedef struct _icmMeasurement icmMeasurement;
typedef struct {
struct _icc *icp;
char root[32];
double pcsCoords[3];
double deviceCoords[MAX_CHAN];
} icmNamedColorVal;
struct _icmNamedColor {
ICM_BASE_MEMBERS
unsigned int _count;
unsigned int vendorFlag;
unsigned int count;
unsigned int nDeviceCoords;
char prefix[32];
char suffix[32];
icmNamedColorVal *data;
}; typedef struct _icmNamedColor icmNamedColor;
struct _icmTextDescription {
ICM_BASE_MEMBERS
unsigned long _size;
unsigned long uc_size;
int (*core_read)(struct _icmTextDescription *p, char **bpp, char *end);
int (*core_write)(struct _icmTextDescription *p, char **bpp);
unsigned long size;
char *desc;
unsigned int ucLangCode;
unsigned long ucSize;
ORD16 *ucDesc;
ORD16 scCode;
unsigned long scSize;
ORD8 scDesc[67];
}; typedef struct _icmTextDescription icmTextDescription;
struct _icmDescStruct {
struct _icc *icp;
int (*allocate)(struct _icmDescStruct *p);
icmSig deviceMfg;
unsigned int deviceModel;
icmUint64 attributes;
icTechnologySignature technology;
icmTextDescription device;
icmTextDescription model;
}; typedef struct _icmDescStruct icmDescStruct;
struct _icmProfileSequenceDesc {
ICM_BASE_MEMBERS
unsigned int _count;
unsigned int count;
icmDescStruct *data;
}; typedef struct _icmProfileSequenceDesc icmProfileSequenceDesc;
struct _icmSignature {
ICM_BASE_MEMBERS
icTechnologySignature sig;
}; typedef struct _icmSignature icmSignature;
typedef struct {
double frequency;
double angle;
icSpotShape spotShape;
} icmScreeningData;
struct _icmScreening {
ICM_BASE_MEMBERS
unsigned int _channels;
unsigned int screeningFlag;
unsigned int channels;
icmScreeningData *data;
}; typedef struct _icmScreening icmScreening;
struct _icmUcrBg {
ICM_BASE_MEMBERS
unsigned int UCR_count;
unsigned int BG_count;
unsigned long _size;
unsigned int UCRcount;
double *UCRcurve;
unsigned int BGcount;
double *BGcurve;
unsigned long size;
char *string;
}; typedef struct _icmUcrBg icmUcrBg;
struct _icmViewingConditions {
ICM_BASE_MEMBERS
icmXYZNumber illuminant;
icmXYZNumber surround;
icIlluminant stdIlluminant;
}; typedef struct _icmViewingConditions icmViewingConditions;
struct _icmCrdInfo {
ICM_BASE_MEMBERS
unsigned long _ppsize;
unsigned long _crdsize[4];
unsigned long ppsize;
char *ppname;
unsigned long crdsize[4];
char *crdname[4];
}; typedef struct _icmCrdInfo icmCrdInfo;
struct _icmVideoCardGammaTable {
unsigned short channels;
unsigned short entryCount;
unsigned short entrySize;
void *data;
}; typedef struct _icmVideoCardGammaTable icmVideoCardGammaTable;
struct _icmVideoCardGammaFormula {
double redGamma;
double redMin;
double redMax;
double greenGamma;
double greenMin;
double greenMax;
double blueGamma;
double blueMin;
double blueMax;
}; typedef struct _icmVideoCardGammaFormula icmVideoCardGammaFormula;
enum {
icmVideoCardGammaTableType = 0,
icmVideoCardGammaFormulaType = 1
};
struct _icmVideoCardGamma {
ICM_BASE_MEMBERS
unsigned long tagType;
union {
icmVideoCardGammaTable table;
icmVideoCardGammaFormula formula;
} u;
}; typedef struct _icmVideoCardGamma icmVideoCardGamma;
struct _icmHeader {
unsigned int (*get_size)(struct _icmHeader *p);
int (*read)(struct _icmHeader *p, unsigned long len, unsigned long of);
int (*write)(struct _icmHeader *p, unsigned long of);
void (*del)(struct _icmHeader *p);
struct _icc *icp;
unsigned int size;
void (*dump)(struct _icmHeader *p, FILE *op, int verb);
icProfileClassSignature deviceClass;
icColorSpaceSignature colorSpace;
icColorSpaceSignature pcs;
icRenderingIntent renderingIntent;
icmSig manufacturer;
icmSig model;
icmUint64 attributes;
unsigned int flags;
icmSig creator;
icmSig cmmId;
int majv, minv, bfv;
icmDateTimeNumber date;
icPlatformSignature platform;
icmXYZNumber illuminant;
}; typedef struct _icmHeader icmHeader;
typedef enum {
icmFwd = 0,
icmBwd = 1,
icmGamut = 2,
icmPreview = 3
} icmLookupFunc;
typedef enum {
icmLuOrdNorm = 0,
icmLuOrdRev = 1
} icmLookupOrder;
typedef enum {
icmMonoFwdType = 0,
icmMonoBwdType = 1,
icmMatrixFwdType = 2,
icmMatrixBwdType = 3,
icmLutType = 4
} icmLuAlgType;
#define LU_ICM_BASE_MEMBERS \
\
icmLuAlgType ttype; \
struct _icc *icp; \
icRenderingIntent intent; \
icmLookupFunc function; \
icmXYZNumber pcswht, whitePoint, blackPoint; \
double toAbs[3][3]; \
double fromAbs[3][3]; \
icColorSpaceSignature inSpace; \
icColorSpaceSignature outSpace; \
icColorSpaceSignature pcs; \
icColorSpaceSignature e_inSpace; \
icColorSpaceSignature e_outSpace; \
icColorSpaceSignature e_pcs; \
\
\
void (*del)(struct _icmLuBase *p); \
\
void (*lutspaces) (struct _icmLuBase *p, icColorSpaceSignature *ins, int *inn, \
icColorSpaceSignature *outs, int *outn); \
\
\
void (*spaces) (struct _icmLuBase *p, icColorSpaceSignature *ins, int *inn, \
icColorSpaceSignature *outs, int *outn, \
icmLuAlgType *alg, icRenderingIntent *intt, \
icmLookupFunc *fnc, icColorSpaceSignature *pcs); \
\
\
void (*get_ranges) (struct _icmLuBase *p, \
double *inmin, double *inmax, \
double *outmin, double *outmax); \
\
void (*wh_bk_points)(struct _icmLuBase *p, icmXYZNumber *wht, icmXYZNumber *blk); \
int (*lookup) (struct _icmLuBase *p, double *out, double *in);
struct _icmLuBase {
LU_ICM_BASE_MEMBERS
}; typedef struct _icmLuBase icmLuBase;
struct _icmLuMono {
LU_ICM_BASE_MEMBERS
icmCurve *grayCurve;
int (*fwd_lookup) (struct _icmLuBase *p, double *out, double *in);
int (*bwd_lookup) (struct _icmLuBase *p, double *out, double *in);
int (*fwd_curve) (struct _icmLuMono *p, double *out, double *in);
int (*fwd_map) (struct _icmLuMono *p, double *out, double *in);
int (*fwd_abs) (struct _icmLuMono *p, double *out, double *in);
int (*bwd_abs) (struct _icmLuMono *p, double *out, double *in);
int (*bwd_map) (struct _icmLuMono *p, double *out, double *in);
int (*bwd_curve) (struct _icmLuMono *p, double *out, double *in);
}; typedef struct _icmLuMono icmLuMono;
struct _icmLuMatrix {
LU_ICM_BASE_MEMBERS
icmCurve *redCurve, *greenCurve, *blueCurve;
icmXYZArray *redColrnt, *greenColrnt, *blueColrnt;
double mx[3][3];
double bmx[3][3];
int (*fwd_lookup) (struct _icmLuBase *p, double *out, double *in);
int (*bwd_lookup) (struct _icmLuBase *p, double *out, double *in);
int (*fwd_curve) (struct _icmLuMatrix *p, double *out, double *in);
int (*fwd_matrix) (struct _icmLuMatrix *p, double *out, double *in);
int (*fwd_abs) (struct _icmLuMatrix *p, double *out, double *in);
int (*bwd_abs) (struct _icmLuMatrix *p, double *out, double *in);
int (*bwd_matrix) (struct _icmLuMatrix *p, double *out, double *in);
int (*bwd_curve) (struct _icmLuMatrix *p, double *out, double *in);
}; typedef struct _icmLuMatrix icmLuMatrix;
struct _icmLuLut {
LU_ICM_BASE_MEMBERS
icmLut *lut;
int usematrix;
double imx[3][3];
int imx_valid;
void (*in_normf)(double *out, double *in);
void (*in_denormf)(double *out, double *in);
void (*out_normf)(double *out, double *in);
void (*out_denormf)(double *out, double *in);
void (*e_in_denormf)(double *out, double *in);
void (*e_out_denormf)(double *out, double *in);
int (*lookup_clut) (struct _icmLut *pp, double *out, double *in);
int (*in_abs) (struct _icmLuLut *p, double *out, double *in);
int (*matrix) (struct _icmLuLut *p, double *out, double *in);
int (*input) (struct _icmLuLut *p, double *out, double *in);
int (*clut) (struct _icmLuLut *p, double *out, double *in);
int (*output) (struct _icmLuLut *p, double *out, double *in);
int (*out_abs) (struct _icmLuLut *p, double *out, double *in);
int (*inv_out_abs) (struct _icmLuLut *p, double *out, double *in);
int (*inv_output) (struct _icmLuLut *p, double *out, double *in);
int (*inv_input) (struct _icmLuLut *p, double *out, double *in);
int (*inv_matrix) (struct _icmLuLut *p, double *out, double *in);
int (*inv_in_abs) (struct _icmLuLut *p, double *out, double *in);
void (*get_info) (struct _icmLuLut *p, icmLut **lutp,
icmXYZNumber *pcswhtp, icmXYZNumber *whitep,
icmXYZNumber *blackp);
void (*get_lutranges) (struct _icmLuLut *p,
double *inmin, double *inmax,
double *outmin, double *outmax);
void (*get_matrix) (struct _icmLuLut *p, double m[3][3]);
}; typedef struct _icmLuLut icmLuLut;
typedef struct {
icTagSignature sig;
icTagTypeSignature ttype;
unsigned int offset;
unsigned int size;
icmBase *objp;
} icmTag;
#define icmDefaultIntent ((icRenderingIntent)98)
#define icmSigDefaultData ((icColorSpaceSignature) 0x0)
struct _icc {
unsigned int (*get_size)(struct _icc *p);
int (*read)(struct _icc *p, icmFile *fp, unsigned long of);
int (*write)(struct _icc *p, icmFile *fp, unsigned long of);
void (*dump)(struct _icc *p, FILE *op, int verb);
void (*del)(struct _icc *p);
int (*find_tag)(struct _icc *p, icTagSignature sig);
icmBase * (*read_tag)(struct _icc *p, icTagSignature sig);
icmBase * (*add_tag)(struct _icc *p, icTagSignature sig, icTagTypeSignature ttype);
int (*rename_tag)(struct _icc *p, icTagSignature sig, icTagSignature sigNew);
icmBase * (*link_tag)(struct _icc *p, icTagSignature sig, icTagSignature ex_sig);
int (*unread_tag)(struct _icc *p, icTagSignature sig);
int (*read_all_tags)(struct _icc *p);
int (*delete_tag)(struct _icc *p, icTagSignature sig);
icmLuBase * (*get_luobj) (struct _icc *p,
icmLookupFunc func,
icRenderingIntent intent,
icColorSpaceSignature pcsor,
icmLookupOrder order);
icmHeader *header;
char err[512];
int errc;
icmAlloc *al;
int del_al;
icmFile *fp;
unsigned long of;
unsigned int count;
icmTag *data;
}; typedef struct _icc icc;
struct _psh {
int di;
unsigned res;
unsigned bits;
unsigned ix;
unsigned tmask;
unsigned count;
}; typedef struct _psh psh;
typedef enum {
icmScreenEncodings,
icmDeviceAttributes,
icmProfileHeaderFlags,
icmAsciiOrBinaryData,
icmTagSignature,
icmTechnologySignature,
icmTypeSignature,
icmColorSpaceSignature,
icmProfileClassSignaure,
icmPlatformSignature,
icmMeasurementFlare,
icmMeasurementGeometry,
icmRenderingIntent,
icmSpotShape,
icmStandardObserver,
icmIlluminant,
icmLuAlg
} icmEnumType;
extern ICCLIB_API icc *new_icc(void);
extern ICCLIB_API icc *new_icc_a(icmAlloc *al);
extern ICCLIB_API char *tag2str(int tag);
extern ICCLIB_API int str2tag(const char *str);
extern ICCLIB_API const char *icm2str(icmEnumType etype, int enumval);
extern ICCLIB_API void icmXYZ2Lab(icmXYZNumber *w, double *out, double *in);
extern ICCLIB_API void icmLab2XYZ(icmXYZNumber *w, double *out, double *in);
extern ICCLIB_API icmXYZNumber icmD50;
extern ICCLIB_API icmXYZNumber icmD65;
extern ICCLIB_API icmXYZNumber icmBlack;
extern ICCLIB_API unsigned psh_init(psh *p, int di, unsigned res, int co[]);
extern ICCLIB_API void psh_reset(psh *p);
extern ICCLIB_API int psh_inc(psh *p, int co[]);
void icmChromAdaptMatrix(
int flags,
icmXYZNumber d_wp,
icmXYZNumber s_wp,
double mat[3][3]
);
#define ICM_CAM_BRADFORD 0x0001
#define ICM_CAM_MULMATRIX 0x0002
extern ICCLIB_API double icmLabDE(double *in1, double *in2);
extern ICCLIB_API double icmLabDEsq(double *in1, double *in2);
extern ICCLIB_API double icmCIE94(double *in1, double *in2);
extern ICCLIB_API double icmCIE94sq(double *in1, double *in2);
#define icmAry2XYZ(xyz, ary) ((xyz).X = (ary)[0], (xyz).Y = (ary)[1], (xyz).Z = (ary)[2])
#define icmXYZ2Ary(ary, xyz) ((ary)[0] = (xyz).X, (ary)[1] = (xyz).Y, (ary)[2] = (xyz).Z)
#ifdef __cplusplus
}
#endif
#endif