#ifndef gsccode_INCLUDED
#  define gsccode_INCLUDED
typedef ulong gs_char;
#define GS_NO_CHAR ((gs_char)~0L)
#define gs_no_char GS_NO_CHAR
typedef ulong gs_glyph;
#define GS_NO_GLYPH ((gs_glyph)0x7fffffff)
#if arch_sizeof_long > 4
#  define GS_MIN_CID_GLYPH ((gs_glyph)0x80000000L)
#else
#  define GS_MIN_CID_GLYPH ((gs_glyph)~0x7fffffff)
#endif
#define GS_MIN_GLYPH_INDEX (GS_MIN_CID_GLYPH | (GS_MIN_CID_GLYPH >> 1))
#define GS_GLYPH_TAG (gs_glyph)(GS_MIN_CID_GLYPH | GS_MIN_GLYPH_INDEX)
#define GS_MAX_GLYPH max_ulong
#define gs_no_glyph GS_NO_GLYPH
#define gs_min_cid_glyph GS_MIN_CID_GLYPH
#define gs_max_glyph GS_MAX_GLYPH
typedef bool (*gs_glyph_mark_proc_t)(const gs_memory_t *mem, gs_glyph glyph, void *proc_data);
typedef enum {
ENCODING_INDEX_UNKNOWN = -1,
ENCODING_INDEX_STANDARD = 0,
ENCODING_INDEX_ISOLATIN1,
ENCODING_INDEX_SYMBOL,
ENCODING_INDEX_DINGBATS,
ENCODING_INDEX_WINANSI,
ENCODING_INDEX_MACROMAN,
ENCODING_INDEX_MACEXPERT,
#define NUM_KNOWN_REAL_ENCODINGS 7
ENCODING_INDEX_MACGLYPH,
ENCODING_INDEX_ALOGLYPH,
ENCODING_INDEX_ALXGLYPH,
ENCODING_INDEX_CFFSTRINGS
#define NUM_KNOWN_ENCODINGS 11
} gs_encoding_index_t;
#define KNOWN_REAL_ENCODING_NAMES\
"StandardEncoding", "ISOLatin1Encoding", "SymbolEncoding",\
"DingbatsEncoding", "WinAnsiEncoding", "MacRomanEncoding",\
"MacExpertEncoding"
typedef enum gs_glyph_space_s {
GLYPH_SPACE_NAME,
GLYPH_SPACE_INDEX,
GLYPH_SPACE_NOGEN
} gs_glyph_space_t;
typedef int (*gs_glyph_name_proc_t)(const gs_memory_t *mem,
gs_glyph glyph, gs_const_string *pstr,
void *proc_data);
#endif