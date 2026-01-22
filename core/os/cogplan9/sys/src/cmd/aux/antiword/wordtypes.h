#if !defined(__wordtypes_h)
#define __wordtypes_h 1
#include <time.h>
#if defined(__riscos)
#include "DeskLib:Font.h"
#include "DeskLib:Wimp.h"
#endif
typedef unsigned char UCHAR;
typedef unsigned short USHORT;
typedef unsigned int UINT;
typedef unsigned long ULONG;
#if defined(__riscos)
typedef struct diagram_tag {
drawfile_info tInfo;
window_handle tMainWindow;
window_handle tScaleWindow;
menu_ptr pSaveMenu;
long lXleft;
long lYtop;
size_t tMemorySize;
int iScaleFactorCurr;
int iScaleFactorTemp;
char szFilename[19+1];
} diagram_type;
#else
typedef struct diagram_tag {
FILE *pOutFile;
long lXleft;
long lYtop;
} diagram_type;
typedef UCHAR drawfile_fontref;
#endif
typedef struct output_tag {
char *szStorage;
long lStringWidth;
size_t tStorageSize;
size_t tNextFree;
USHORT usFontStyle;
USHORT usFontSize;
UCHAR ucFontColor;
drawfile_fontref tFontRef;
struct output_tag *pPrev;
struct output_tag *pNext;
} output_type;
typedef enum conversion_tag {
conversion_unknown = 0,
conversion_text,
conversion_draw,
conversion_ps,
conversion_xml,
conversion_pdf,
conversion_fmt_text
} conversion_type;
typedef enum encoding_tag {
encoding_neutral = 100,
encoding_latin_1 = 801,
encoding_latin_2 = 802,
encoding_cyrillic = 805,
encoding_utf_8 = 1601
} encoding_type;
typedef struct font_table_tag {
USHORT usFontStyle;
UCHAR ucWordFontNumber;
UCHAR ucFFN;
UCHAR ucEmphasis;
UCHAR ucInUse;
char szWordFontname[65];
char szOurFontname[33];
} font_table_type;
typedef enum image_level_tag {
level_gs_special = 0,
level_no_images,
level_ps_2,
level_ps_3,
level_default = level_ps_2
} image_level_enum;
typedef struct options_tag {
int iParagraphBreak;
conversion_type eConversionType;
BOOL bHideHiddenText;
BOOL bRemoveRemovedText;
BOOL bUseLandscape;
encoding_type eEncoding;
int iPageHeight;
int iPageWidth;
image_level_enum eImageLevel;
#if defined(__riscos)
BOOL bAutofiletypeAllowed;
int iScaleFactor;
#endif
} options_type;
typedef struct pps_tag {
ULONG ulSB;
ULONG ulSize;
} pps_type;
typedef struct pps_info_tag {
pps_type tWordDocument;
pps_type tData;
pps_type tTable;
pps_type tSummaryInfo;
pps_type tDocSummaryInfo;
pps_type t0Table;
pps_type t1Table;
} pps_info_type;
typedef struct data_block_tag {
ULONG ulFileOffset;
ULONG ulDataPos;
ULONG ulLength;
} data_block_type;
typedef struct text_block_tag {
ULONG ulFileOffset;
ULONG ulCharPos;
ULONG ulLength;
BOOL bUsesUnicode;
USHORT usPropMod;
} text_block_type;
typedef struct document_block_tag {
time_t tCreateDate;
time_t tRevisedDate;
USHORT usDefaultTabWidth;
UCHAR ucHdrFtrSpecification;
} document_block_type;
typedef struct row_block_tag {
ULONG ulFileOffsetStart;
ULONG ulFileOffsetEnd;
ULONG ulCharPosStart;
ULONG ulCharPosEnd;
short asColumnWidth[TABLE_COLUMN_MAX+1];
UCHAR ucNumberOfColumns;
UCHAR ucBorderInfo;
} row_block_type;
typedef enum level_type_tag {
level_type_none = 0,
level_type_outline,
level_type_numbering,
level_type_sequence,
level_type_pause
} level_type_enum;
typedef enum list_id_tag {
no_list = 0,
text_list,
footnote_list,
hdrftr_list,
macro_list,
annotation_list,
endnote_list,
textbox_list,
hdrtextbox_list,
end_of_lists
} list_id_enum;
typedef struct style_block_tag {
ULONG ulFileOffset;
list_id_enum eListID;
BOOL bNumPause;
BOOL bNoRestart;
USHORT usIstd;
USHORT usIstdNext;
USHORT usStartAt;
USHORT usBeforeIndent;
USHORT usAfterIndent;
USHORT usListIndex;
USHORT usListChar;
short sLeftIndent;
short sLeftIndent1;
short sRightIndent;
UCHAR ucAlignment;
UCHAR ucNFC;
UCHAR ucNumLevel;
UCHAR ucListLevel;
char szListChar[4];
} style_block_type;
typedef struct font_block_tag {
ULONG ulFileOffset;
USHORT usFontStyle;
USHORT usFontSize;
UCHAR ucFontNumber;
UCHAR ucFontColor;
} font_block_type;
typedef struct picture_block_tag {
ULONG ulFileOffset;
ULONG ulFileOffsetPicture;
ULONG ulPictureOffset;
} picture_block_type;
typedef struct section_block_tag {
BOOL bNewPage;
USHORT usNeedPrevLvl;
USHORT usHangingIndent;
UCHAR aucNFC[9];
UCHAR ucHdrFtrSpecification;
} section_block_type;
typedef struct hdrftr_block_tag {
output_type *pText;
long lHeight;
} hdrftr_block_type;
typedef struct footnote_block_tag {
char *szText;
} footnote_block_type;
typedef struct list_block_tag {
ULONG ulStartAt;
BOOL bNoRestart;
USHORT usListChar;
short sLeftIndent;
UCHAR ucNFC;
} list_block_type;
typedef enum imagetype_tag {
imagetype_is_unknown = 0,
imagetype_is_external,
imagetype_is_emf,
imagetype_is_wmf,
imagetype_is_pict,
imagetype_is_jpeg,
imagetype_is_png,
imagetype_is_dib
} imagetype_enum;
typedef enum compression_tag {
compression_unknown = 0,
compression_none,
compression_rle4,
compression_rle8,
compression_jpeg,
compression_zlib
} compression_enum;
typedef struct imagedata_tag {
imagetype_enum eImageType;
size_t tPosition;
size_t tLength;
int iHorSizeScaled;
int iVerSizeScaled;
int iWidth;
int iHeight;
int iComponents;
UINT uiBitsPerComponent;
BOOL bAdobe;
compression_enum eCompression;
BOOL bColorImage;
int iColorsUsed;
UCHAR aucPalette[256][3];
} imagedata_type;
typedef enum row_info_tag {
found_nothing,
found_a_cell,
found_not_a_cell,
found_end_of_row,
found_not_end_of_row
} row_info_enum;
typedef enum notetype_tag {
notetype_is_footnote,
notetype_is_endnote,
notetype_is_unknown
} notetype_enum;
typedef enum image_info_tag {
image_no_information,
image_minimal_information,
image_full_information
} image_info_enum;
#endif