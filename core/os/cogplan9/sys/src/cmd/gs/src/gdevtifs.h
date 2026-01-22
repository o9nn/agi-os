#ifndef gdevtifs_INCLUDED
#  define gdevtifs_INCLUDED
#if arch_sizeof_short == 2
typedef short TIFF_short;
typedef unsigned short TIFF_ushort;
#endif
#if arch_sizeof_int == 4
typedef int TIFF_long;
typedef unsigned int TIFF_ulong;
#else
# if arch_sizeof_long == 4
typedef long TIFF_long;
typedef unsigned long TIFF_ulong;
# endif
#endif
typedef struct TIFF_header_s {
TIFF_ushort magic;
TIFF_ushort version;
TIFF_ulong diroff;
} TIFF_header;
#define	TIFF_magic_big_endian		0x4d4d
#define	TIFF_magic_little_endian	0x4949
#define	TIFF_version_value	42
typedef struct TIFF_dir_entry_s {
TIFF_ushort tag;
TIFF_ushort type;
TIFF_ulong count;
TIFF_ulong value;
} TIFF_dir_entry;
typedef enum {
TIFF_BYTE = 1,
TIFF_ASCII = 2,
TIFF_SHORT = 3,
TIFF_LONG = 4,
TIFF_RATIONAL = 5,
TIFF_SBYTE = 6,
TIFF_UNDEFINED = 7,
TIFF_SSHORT = 8,
TIFF_SLONG = 9,
TIFF_SRATIONAL = 10,
TIFF_FLOAT = 11,
TIFF_DOUBLE = 12,
TIFF_INDIRECT = 128
} TIFF_data_type;
typedef enum {
TIFFTAG_SubFileType = 254,
#define	    SubFileType_reduced_image	0x1
#define	    SubFileType_page		0x2
#define	    SubFileType_mask		0x4
TIFFTAG_ImageWidth = 256,
TIFFTAG_ImageLength = 257,
TIFFTAG_BitsPerSample = 258,
TIFFTAG_Compression = 259,
#define	    Compression_none		1
#define	    Compression_CCITT_RLE	2
#define	    Compression_CCITT_T4	3
#define	    Compression_CCITT_T6	4
#define	    Compression_LZW		5
#define	    Compression_JPEG		6
#define	    Compression_NeXT		32766
#define	    Compression_CCITT_RLEW	32771
#define	    Compression_PackBits	32773
#define	    Compression_Thunderscan	32809
TIFFTAG_Photometric = 262,
#define	    Photometric_min_is_white	0
#define	    Photometric_min_is_black	1
#define	    Photometric_RGB		2
#define	    Photometric_palette		3
#define	    Photometric_mask		4
#define	    Photometric_separated	5
#define	    Photometric_YCbCr		6
#define	    Photometric_CIE_Lab		8
TIFFTAG_FillOrder = 266,
#define	    FillOrder_MSB2LSB		1
#define	    FillOrder_LSB2MSB		2
TIFFTAG_StripOffsets = 273,
TIFFTAG_Orientation = 274,
#define	    Orientation_top_left	1
#define	    Orientation_top_right	2
#define	    Orientation_bot_right	3
#define	    Orientation_bot_left	4
#define	    Orientation_left_top	5
#define	    Orientation_right_top	6
#define	    Orientation_right_bot	7
#define	    Orientation_left_bot	8
TIFFTAG_SamplesPerPixel = 277,
TIFFTAG_RowsPerStrip = 278,
TIFFTAG_StripByteCounts = 279,
TIFFTAG_XResolution = 282,
TIFFTAG_YResolution = 283,
TIFFTAG_PlanarConfig = 284,
#define	    PlanarConfig_contig		1
#define	    PlanarConfig_separate	2
TIFFTAG_T4Options = 292,
#define	    T4Options_2D_encoding	0x1
#define	    T4Options_uncompressed	0x2
#define	    T4Options_fill_bits		0x4
TIFFTAG_T6Options = 293,
#define	    T6Options_uncompressed	0x2
TIFFTAG_ResolutionUnit = 296,
#define	    ResolutionUnit_none		1
#define	    ResolutionUnit_inch		2
#define	    ResolutionUnit_centimeter	3
TIFFTAG_PageNumber = 297,
TIFFTAG_Software = 305,
TIFFTAG_DateTime = 306,
TIFFTAG_CleanFaxData = 327
#define	    CleanFaxData_clean		0
#define	    CleanFaxData_regenerated	1
#define	    CleanFaxData_unclean	2
} TIFF_tag;
typedef struct gdev_tiff_state_s {
gs_memory_t *mem;
long prev_dir;
long dir_off;
int ntags;
long strip_index;
long strip_count;
long rows;
int offset_StripOffsets;
int offset_StripByteCounts;
TIFF_ulong *StripOffsets;
TIFF_ulong *StripByteCounts;
} gdev_tiff_state;
int gdev_tiff_begin_page(gx_device_printer * pdev, gdev_tiff_state * tifs,
FILE * fp,
const TIFF_dir_entry * entries, int entry_count,
const byte * values, int value_size,
long max_strip_size);
int gdev_tiff_end_strip(gdev_tiff_state * tifs, FILE * fp);
int gdev_tiff_end_page(gdev_tiff_state * tifs, FILE * fp);
#endif