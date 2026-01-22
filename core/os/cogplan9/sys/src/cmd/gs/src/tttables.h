#ifndef TTTABLES_H
#define TTTABLES_H
#include "tttypes.h"
#ifdef __cplusplus
extern "C" {
#endif
struct _TTTCHeader
{
Long Tag;
TT_Fixed version;
ULong DirCount;
PULong TableDirectory;
};
typedef struct _TTTCHeader TTTCHeader;
typedef TTTCHeader* PTTCHeader;
struct _TTableDir
{
TT_Fixed version;
UShort numTables;
UShort searchRange;
UShort entrySelector;
UShort rangeShift;
};
typedef struct _TTableDir TTableDir;
typedef TTableDir* PTableDir;
struct _TTableDirEntry
{
Long Tag;
Long CheckSum;
Long Offset;
Long Length;
};
typedef struct _TTableDirEntry TTableDirEntry;
typedef TTableDirEntry* PTableDirEntry;
struct _TCMapDir
{
UShort tableVersionNumber;
UShort numCMaps;
};
typedef struct _TCMapDir TCMapDir;
typedef TCMapDir* PCMapDir;
struct _TCMapDirEntry
{
UShort platformID;
UShort platformEncodingID;
Long offset;
};
typedef struct _TCMapDirEntry TCMapDirEntry;
typedef TCMapDirEntry* PCMapDirEntries;
struct _TMaxProfile
{
TT_Fixed version;
UShort numGlyphs,
maxPoints,
maxContours,
maxCompositePoints,
maxCompositeContours,
maxZones,
maxTwilightPoints,
maxStorage,
maxFunctionDefs,
maxInstructionDefs,
maxStackElements,
maxSizeOfInstructions,
maxComponentElements,
maxComponentDepth;
};
typedef struct _TMaxProfile TMaxProfile;
typedef TMaxProfile* PMaxProfile;
# define GASP_GRIDFIT 0x01
# define GASP_DOGRAY 0x02
struct _GaspRange
{
UShort maxPPEM;
UShort gaspFlag;
};
typedef struct _GaspRange GaspRange;
struct _TGasp
{
UShort version;
UShort numRanges;
GaspRange* gaspRanges;
};
typedef struct _TGasp TGasp;
struct _TLongHorMetric
{
UShort advance_Width;
Short lsb;
};
typedef struct _TLongHorMetric TLongHorMetric;
typedef TLongHorMetric* PTableHorMetrics;
struct _TLoca
{
UShort Size;
PStorage Table;
};
typedef struct _TLoca TLoca;
struct _TNameRec
{
UShort platformID;
UShort encodingID;
UShort languageID;
UShort nameID;
UShort stringLength;
UShort stringOffset;
PByte string;
};
typedef struct _TNameRec TNameRec;
struct _TName_Table
{
UShort format;
UShort numNameRecords;
UShort storageOffset;
TNameRec* names;
PByte storage;
};
typedef struct _TName_Table TName_Table;
#ifdef __cplusplus
}
#endif
#endif