#ifndef sfntIncludes
#define sfntIncludes
#include "stdint_.h"
typedef uint8_t uint8;
typedef int8_t int8;
typedef uint16_t uint16;
typedef int16_t int16;
typedef uint32_t uint32;
typedef int32_t int32;
#if 0
typedef int32_t Fixed;
#endif
typedef int16_t FUnit;
typedef int16_t FWord;
typedef uint16_t uFWord;
typedef int16_t F2Dot14;
#if 0
typedef int32_t F26Dot6;
#endif
typedef struct {
uint32 bc;
uint32 ad;
} BigDate;
typedef struct {
uint32 tag;
uint32 checkSum;
uint32 offset;
uint32 length;
} sfnt_DirectoryEntry;
#define SFNT_VERSION 0x10000
typedef struct {
Fixed version;
uint16 numOffsets;
uint16 searchRange;
uint16 entrySelector;
uint16 rangeShift;
sfnt_DirectoryEntry table[1];
} sfnt_OffsetTable;
#define OFFSETTABLESIZE 12
typedef enum sfntHeaderFlagBits {
Y_POS_SPECS_BASELINE = 1,
X_POS_SPECS_LSB = 2,
HINTS_USE_POINTSIZE = 4,
USE_INTEGER_SCALING = 8,
INSTRUCTIONS_CHANGE_ADVANCEWIDTHS = 16,
X_POS_SPECS_BASELINE = 32,
Y_POS_SPECS_TSB = 64
} sfntHeaderFlagBits;
#define SFNT_MAGIC 0x5F0F3CF5
#define SHORT_INDEX_TO_LOC_FORMAT 0
#define LONG_INDEX_TO_LOC_FORMAT 1
#define GLYPH_DATA_FORMAT 0
#define FONT_HEADER_VERSION 0x10000
typedef struct {
Fixed version;
Fixed fontRevision;
uint32 checkSumAdjustment;
uint32 magicNumber;
uint16 flags;
uint16 unitsPerEm;
BigDate created;
BigDate modified;
int16 xMin;
int16 yMin;
int16 xMax;
int16 yMax;
uint16 macStyle;
uint16 lowestRecPPEM;
int16 fontDirectionHint;
int16 indexToLocFormat;
int16 glyphDataFormat;
} sfnt_FontHeader;
#define METRIC_HEADER_FORMAT 0x10000
typedef struct {
Fixed version;
int16 ascender;
int16 descender;
int16 lineGap;
uint16 advanceMax;
int16 sideBearingMin;
int16 otherSideBearingMin;
int16 extentMax;
int16 caretSlopeNumerator;
int16 caretSlopeDenominator;
int16 caretOffset;
uint32 reserved1, reserved2;
int16 metricDataFormat;
uint16 numberLongMetrics;
} sfnt_MetricsHeader;
typedef sfnt_MetricsHeader sfnt_HorizontalHeader;
typedef sfnt_MetricsHeader sfnt_VerticalHeader;
#define MAX_PROFILE_VERSION 0x10000
typedef struct {
Fixed version;
uint16 numGlyphs;
uint16 maxPoints;
uint16 maxContours;
uint16 maxCompositePoints;
uint16 maxCompositeContours;
uint16 maxElements;
uint16 maxTwilightPoints;
uint16 maxStorage;
uint16 maxFunctionDefs;
uint16 maxInstructionDefs;
uint16 maxStackElements;
uint16 maxSizeOfInstructions;
uint16 maxComponentElements;
uint16 maxComponentDepth;
} sfnt_maxProfileTable;
typedef struct {
uint16 advance;
int16 sideBearing;
} sfnt_GlyphMetrics;
typedef sfnt_GlyphMetrics sfnt_HorizontalMetrics;
typedef sfnt_GlyphMetrics sfnt_VerticalMetrics;
typedef int16 sfnt_ControlValue;
typedef struct {
uint16 format;
uint16 length;
uint16 version;
} sfnt_mappingTable;
typedef struct {
uint16 platformID;
uint16 specificID;
uint32 offset;
} sfnt_platformEntry;
typedef struct {
uint16 version;
uint16 numTables;
sfnt_platformEntry platform[1];
} sfnt_char2IndexDirectory;
#define SIZEOFCHAR2INDEXDIR 4
typedef struct {
uint16 platformID;
uint16 specificID;
uint16 languageID;
uint16 nameID;
uint16 length;
uint16 offset;
} sfnt_NameRecord;
typedef struct {
uint16 format;
uint16 count;
uint16 stringOffset;
} sfnt_NamingTable;
#define DEVWIDTHEXTRA 2
typedef struct {
int16 version;
int16 numRecords;
int32 recordSize;
} sfnt_DeviceMetrics;
#define stdPostTableFormat 0x10000
#define wordPostTableFormat 0x20000
#define bytePostTableFormat 0x28000
#define richardsPostTableFormat 0x30000
typedef struct {
Fixed version;
Fixed italicAngle;
int16 underlinePosition;
int16 underlineThickness;
int16 isFixedPitch;
int16 pad;
uint32 minMemType42;
uint32 maxMemType42;
uint32 minMemType1;
uint32 maxMemType1;
} sfnt_PostScriptInfo;
typedef enum outlinePacking {
ONCURVE = 1,
XSHORT = 2,
YSHORT = 4,
REPEAT_FLAGS = 8,
SHORT_X_IS_POS = 16,
NEXT_X_IS_ZERO = 16,
SHORT_Y_IS_POS = 32,
NEXT_Y_IS_ZERO = 32
} outlinePacking;
typedef enum componentPacking {
COMPONENTCTRCOUNT = -1,
ARG_1_AND_2_ARE_WORDS = 1,
ARGS_ARE_XY_VALUES = 2,
ROUND_XY_TO_GRID = 4,
WE_HAVE_A_SCALE = 8,
NON_OVERLAPPING = 16,
MORE_COMPONENTS = 32,
WE_HAVE_AN_X_AND_Y_SCALE = 64,
WE_HAVE_A_TWO_BY_TWO = 128,
WE_HAVE_INSTRUCTIONS = 256,
USE_MY_METRICS = 512
} componentPacking;
typedef struct {
uint16 firstCode;
uint16 entryCount;
int16 idDelta;
uint16 idRangeOffset;
} sfnt_subheader;
typedef struct {
uint16 segCountX2;
uint16 searchRange;
uint16 entrySelector;
uint16 rangeShift;
} sfnt_4_subheader;
typedef enum {
plat_Unicode,
plat_Macintosh,
plat_ISO,
plat_MS
} platformEnums;
#define tag_FontHeader 'daeh'
#define tag_HoriHeader 'aehh'
#define tag_VertHeader 'aehv'
#define tag_IndexToLoc 'acol'
#define tag_MaxProfile 'pxam'
#define tag_ControlValue ' tvc'
#define tag_PreProgram 'perp'
#define tag_GlyphData 'fylg'
#define tag_HorizontalMetrics 'xtmh'
#define tag_VerticalMetrics 'xtmv'
#define tag_CharToIndexMap 'pamc'
#define tag_FontProgram 'mgpf'
#define tag_Kerning 'nrek'
#define tag_HoriDeviceMetrics 'xmdh'
#define tag_NamingTable 'eman'
#define tag_PostScript 'tsop'
#if 0
#define fNoError 0
#define fTableNotFound -1
#define fNameNotFound -2
#define fMemoryError -3
#define fUnimplemented -4
#define fCMapNotFound -5
#define fGlyphNotFound -6
typedef int32 FontError;
#endif
typedef struct FontTableInfo {
int32 offset;
int32 length;
int32 checkSum;
} FontTableInfo;
#define RAW_TRUE_TYPE_SIZE 512
#endif