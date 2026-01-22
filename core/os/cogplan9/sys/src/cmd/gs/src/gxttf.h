#ifndef gxttf_INCLUDED
# define gxttf_INCLUDED
#define TT_CG_ARGS_ARE_WORDS (1<<0)
#define TT_CG_ARGS_ARE_XY_VALUES (1<<1)
#define TT_CG_ROUND_XY_TO_GRID (1<<2)
#define TT_CG_HAVE_SCALE (1<<3)
#define TT_CG_MORE_COMPONENTS (1<<5)
#define TT_CG_HAVE_XY_SCALE (1<<6)
#define TT_CG_HAVE_2X2 (1<<7)
#define TT_CG_HAVE_INSTRUCTIONS (1<<8)
#define TT_CG_USE_MY_METRICS (1<<9)
typedef struct ttf_head_s {
byte
version[4],
fontRevision[4],
checkSumAdjustment[4],
magicNumber[4],
flags[2],
unitsPerEm[2],
created[8],
modified[8],
xMin[2],
yMin[2],
xMax[2],
yMax[2],
macStyle[2],
lowestRecPPM[2],
fontDirectionHint[2],
indexToLocFormat[2],
glyphDataFormat[2];
} ttf_head_t;
typedef struct ttf_hhea_s {
byte
version[4],
ascender[2],
descender[2],
lineGap[2],
advanceWidthMax[2],
minLeftSideBearing[2],
minRightSideBearing[2],
xMaxExtent[2],
caretSlopeRise[2],
caretSlopeRun[2],
caretOffset[2],
reserved[8],
metricDataFormat[2],
numHMetrics[2];
} ttf_hhea_t;
typedef struct longHorMetric_s {
byte
advanceWidth[2],
lsb[2];
} longHorMetric_t;
typedef struct ttf_maxp_s {
byte
version[4],
numGlyphs[2],
maxPoints[2],
maxContours[2],
maxCompositePoints[2],
maxCompositeContours[2],
maxZones[2],
maxTwilightPoints[2],
maxStorage[2],
maxFunctionDefs[2],
maxInstructionDefs[2],
maxStackElements[2],
maxSizeOfInstructions[2],
maxComponentElements[2],
maxComponentDepth[2];
} ttf_maxp_t;
typedef struct ttf_OS_2_s {
byte
version[2],
xAvgCharWidth[2],
usWeightClass[2],
usWidthClass[2],
fsType[2],
ySubscriptXSize[2],
ySubscriptYSize[2],
ySubscriptXOffset[2],
ySubscriptYOffset[2],
ySuperscriptXSize[2],
ySuperscriptYSize[2],
ySuperscriptXOffset[2],
ySuperscriptYOffset[2],
yStrikeoutSize[2],
yStrikeoutPosition[2],
sFamilyClass[2],
bFamilyType, bSerifStyle, bWeight, bProportion, bContrast,
bStrokeVariation, bArmStyle, bLetterform, bMidline, bXHeight,
ulUnicodeRanges[16],
achVendID[4],
fsSelection[2],
usFirstCharIndex[2],
usLastCharIndex[2],
sTypoAscender[2],
sTypoDescender[2],
sTypoLineGap[2],
usWinAscent[2],
usWinDescent[2],
ulCodePageRanges[8];
} ttf_OS_2_t;
typedef struct ttf_vhea_s {
byte
version[4],
ascent[2],
descent[2],
lineGap[2],
advanceHeightMax[2],
minTopSideBearing[2],
minBottomSideBearing[2],
yMaxExtent[2],
caretSlopeRise[2],
caretSlopeRun[2],
caretOffset[2],
reserved[8],
metricDataFormat[2],
numVMetrics[2];
} ttf_vhea_t;
typedef struct longVerMetric_s {
byte
advanceHeight[2],
topSideBearing[2];
} longVerMetric_t;
#endif