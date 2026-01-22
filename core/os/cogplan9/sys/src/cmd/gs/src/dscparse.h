#ifndef dscparse_INCLUDED
#  define dscparse_INCLUDED
typedef int GSBOOL;
typedef unsigned long GSDWORD;
typedef unsigned int GSWORD;
#ifndef FALSE
# define FALSE ((GSBOOL)0)
# define TRUE ((GSBOOL)(!FALSE))
#endif
#ifndef DSC_OFFSET
# define DSC_OFFSET unsigned long
#endif
#ifndef DSC_OFFSET_FORMAT
# define DSC_OFFSET_FORMAT "lu"
#endif
#ifndef dsc_private
# ifdef private
#  define dsc_private private
# else
#  define dsc_private static
# endif
#endif
#ifndef min
# define min(a,b)  ((a) < (b) ? (a) : (b))
#endif
#ifndef max
# define max(a,b)  ((a) > (b) ? (a) : (b))
#endif
#define DSC_LINE_LENGTH 255
#define CDSC_STRING_CHUNK 4096
#define CDSC_PAGE_CHUNK 128
#define CDSC_DATA_LENGTH 8192
typedef enum CDSC_RETURN_CODE_e {
CDSC_ERROR		= -1,
CDSC_OK		= 0,
CDSC_NOTDSC	 	= 1,
CDSC_UNKNOWNDSC	= 100,
CDSC_PSADOBE		= 200,
CDSC_BEGINCOMMENTS	= 201,
CDSC_ENDCOMMENTS	= 202,
CDSC_PAGES		= 203,
CDSC_CREATOR		= 204,
CDSC_CREATIONDATE	= 205,
CDSC_TITLE		= 206,
CDSC_FOR		= 207,
CDSC_LANGUAGELEVEL	= 208,
CDSC_BOUNDINGBOX	= 209,
CDSC_ORIENTATION	= 210,
CDSC_PAGEORDER	= 211,
CDSC_DOCUMENTMEDIA	= 212,
CDSC_DOCUMENTPAPERSIZES    = 213,
CDSC_DOCUMENTPAPERFORMS    = 214,
CDSC_DOCUMENTPAPERCOLORS   = 215,
CDSC_DOCUMENTPAPERWEIGHTS  = 216,
CDSC_DOCUMENTDATA	     = 217,
CDSC_REQUIREMENTS	     = 218,
CDSC_DOCUMENTNEEDEDFONTS   = 219,
CDSC_DOCUMENTSUPPLIEDFONTS = 220,
CDSC_HIRESBOUNDINGBOX	     = 221,
CDSC_CROPBOX	     	     = 222,
CDSC_PLATEFILE     	     = 223,
CDSC_DOCUMENTPROCESSCOLORS = 224,
CDSC_DOCUMENTCUSTOMCOLORS  = 225,
CDSC_CMYKCUSTOMCOLOR       = 226,
CDSC_RGBCUSTOMCOLOR        = 227,
CDSC_BEGINPREVIEW	= 301,
CDSC_ENDPREVIEW	= 302,
CDSC_BEGINDEFAULTS	= 401,
CDSC_ENDDEFAULTS	= 402,
CDSC_BEGINPROLOG	= 501,
CDSC_ENDPROLOG	= 502,
CDSC_BEGINFONT	= 503,
CDSC_ENDFONT		= 504,
CDSC_BEGINFEATURE	= 505,
CDSC_ENDFEATURE	= 506,
CDSC_BEGINRESOURCE	= 507,
CDSC_ENDRESOURCE	= 508,
CDSC_BEGINPROCSET	= 509,
CDSC_ENDPROCSET	= 510,
CDSC_BEGINSETUP	= 601,
CDSC_ENDSETUP		= 602,
CDSC_FEATURE		= 603,
CDSC_PAPERCOLOR	= 604,
CDSC_PAPERFORM	= 605,
CDSC_PAPERWEIGHT	= 606,
CDSC_PAPERSIZE	= 607,
CDSC_PAGE		= 700,
CDSC_PAGETRAILER	= 701,
CDSC_BEGINPAGESETUP	= 702,
CDSC_ENDPAGESETUP	= 703,
CDSC_PAGEMEDIA	= 704,
CDSC_PAGEORIENTATION	= 705,
CDSC_PAGEBOUNDINGBOX	= 706,
CDSC_INCLUDEFONT	= 707,
CDSC_VIEWINGORIENTATION = 708,
CDSC_PAGECROPBOX	= 709,
CDSC_TRAILER		= 800,
CDSC_EOF		= 900
} CDSC_RETURN_CODE;
typedef enum CDSC_PREVIEW_TYPE_e {
CDSC_NOPREVIEW = 0,
CDSC_EPSI = 1,
CDSC_TIFF = 2,
CDSC_WMF = 3,
CDSC_PICT = 4
} CDSC_PREVIEW_TYPE;
typedef enum CDSC_PAGE_ORDER_e {
CDSC_ORDER_UNKNOWN = 0,
CDSC_ASCEND = 1,
CDSC_DESCEND = 2,
CDSC_SPECIAL = 3
} CDSC_PAGE_ORDER;
typedef enum CDSC_ORIENTATION_ENUM_e {
CDSC_ORIENT_UNKNOWN = 0,
CDSC_PORTRAIT = 1,
CDSC_LANDSCAPE = 2,
CDSC_UPSIDEDOWN = 3,
CDSC_SEASCAPE = 4
} CDSC_ORIENTATION_ENUM;
typedef enum CDSC_DOCUMENT_DATA_e {
CDSC_DATA_UNKNOWN = 0,
CDSC_CLEAN7BIT = 1,
CDSC_CLEAN8BIT = 2,
CDSC_BINARY = 3
} CDSC_DOCUMENT_DATA ;
typedef struct CDSCBBOX_S {
int llx;
int lly;
int urx;
int ury;
} CDSCBBOX;
typedef struct CDSCFBBOX_S {
float fllx;
float flly;
float furx;
float fury;
} CDSCFBBOX;
typedef struct CDSCMEDIA_S {
const char *name;
float width;
float height;
float weight;
const char *colour;
const char *type;
CDSCBBOX *mediabox;
} CDSCMEDIA;
#define CDSC_KNOWN_MEDIA 11
extern const CDSCMEDIA dsc_known_media[CDSC_KNOWN_MEDIA];
typedef struct CDSCCTM_S {
float xx;
float xy;
float yx;
float yy;
} CDSCCTM;
typedef struct CDSCPAGE_S {
int ordinal;
const char *label;
DSC_OFFSET begin;
DSC_OFFSET end;
unsigned int orientation;
const CDSCMEDIA *media;
CDSCBBOX *bbox;
CDSCCTM *viewing_orientation;
CDSCFBBOX *crop_box;
} CDSCPAGE;
typedef struct CDSCDOSEPS_S {
GSDWORD ps_begin;
GSDWORD ps_length;
GSDWORD wmf_begin;
GSDWORD wmf_length;
GSDWORD tiff_begin;
GSDWORD tiff_length;
GSWORD checksum;
} CDSCDOSEPS;
typedef struct CDSCMACBIN_S {
GSDWORD data_begin;
GSDWORD data_length;
GSDWORD resource_begin;
GSDWORD resource_length;
} CDSCMACBIN;
typedef struct CDSCSTRING_S CDSCSTRING;
struct CDSCSTRING_S {
unsigned int index;
unsigned int length;
char *data;
CDSCSTRING *next;
};
typedef struct CDCS2_S CDCS2;
struct CDCS2_S {
char *colourname;
char *filetype;
char *location;
char *filename;
DSC_OFFSET begin;
DSC_OFFSET end;
CDCS2 *next;
};
typedef enum CDSC_COLOUR_TYPE_e {
CDSC_COLOUR_UNKNOWN=0,
CDSC_COLOUR_PROCESS=1,
CDSC_COLOUR_CUSTOM=2
} CDSC_COLOUR_TYPE;
typedef enum CDSC_CUSTOM_COLOUR_e {
CDSC_CUSTOM_COLOUR_UNKNOWN=0,
CDSC_CUSTOM_COLOUR_RGB=1,
CDSC_CUSTOM_COLOUR_CMYK=2
} CDSC_CUSTOM_COLOUR;
typedef struct CDSCCOLOUR_S CDSCCOLOUR;
struct CDSCCOLOUR_S {
char *name;
CDSC_COLOUR_TYPE type;
CDSC_CUSTOM_COLOUR custom;
float red;
float green;
float blue;
float cyan;
float magenta;
float yellow;
float black;
CDSCCOLOUR *next;
};
typedef enum CDSC_MESSAGE_ERROR_e {
CDSC_MESSAGE_BBOX = 0,
CDSC_MESSAGE_EARLY_TRAILER = 1,
CDSC_MESSAGE_EARLY_EOF = 2,
CDSC_MESSAGE_PAGE_IN_TRAILER = 3,
CDSC_MESSAGE_PAGE_ORDINAL = 4,
CDSC_MESSAGE_PAGES_WRONG = 5,
CDSC_MESSAGE_EPS_NO_BBOX = 6,
CDSC_MESSAGE_EPS_PAGES = 7,
CDSC_MESSAGE_NO_MEDIA = 8,
CDSC_MESSAGE_ATEND = 9,
CDSC_MESSAGE_DUP_COMMENT = 10,
CDSC_MESSAGE_DUP_TRAILER = 11,
CDSC_MESSAGE_BEGIN_END = 12,
CDSC_MESSAGE_BAD_SECTION = 13,
CDSC_MESSAGE_LONG_LINE = 14,
CDSC_MESSAGE_INCORRECT_USAGE = 15
} CDSC_MESSAGE_ERROR;
typedef enum CDSC_MESSAGE_SEVERITY_e {
CDSC_ERROR_INFORM	= 0,
CDSC_ERROR_WARN	= 1,
CDSC_ERROR_ERROR	= 2
} CDSC_MESSAGE_SEVERITY;
typedef enum CDSC_RESPONSE_e {
CDSC_RESPONSE_OK	= 0,
CDSC_RESPONSE_CANCEL	= 1,
CDSC_RESPONSE_IGNORE_ALL = 2
} CDSC_RESPONSE;
extern const char * const dsc_message[];
#ifndef CDSC_TYPEDEF
#define CDSC_TYPEDEF
typedef struct CDSC_s CDSC;
#endif
struct CDSC_s {
char dummy[1024];
GSBOOL dsc;
GSBOOL ctrld;
GSBOOL pjl;
GSBOOL epsf;
GSBOOL pdf;
unsigned int preview;
char *dsc_version;
unsigned int language_level;
unsigned int document_data;
DSC_OFFSET begincomments;
DSC_OFFSET endcomments;
DSC_OFFSET beginpreview;
DSC_OFFSET endpreview;
DSC_OFFSET begindefaults;
DSC_OFFSET enddefaults;
DSC_OFFSET beginprolog;
DSC_OFFSET endprolog;
DSC_OFFSET beginsetup;
DSC_OFFSET endsetup;
DSC_OFFSET begintrailer;
DSC_OFFSET endtrailer;
CDSCPAGE *page;
unsigned int page_count;
unsigned int page_pages;
unsigned int page_order;
unsigned int page_orientation;
CDSCCTM *viewing_orientation;
unsigned int media_count;
CDSCMEDIA **media;
const CDSCMEDIA *page_media;
CDSCBBOX *bbox;
CDSCBBOX *page_bbox;
CDSCDOSEPS *doseps;
char *dsc_title;
char *dsc_creator;
char *dsc_date;
char *dsc_for;
unsigned int max_error;
const int *severity;
void *caller_data;
int id;
int scan_section;
DSC_OFFSET doseps_end;
unsigned int page_chunk_length;
DSC_OFFSET file_length;
int skip_document;
int skip_bytes;
int skip_lines;
GSBOOL skip_pjl;
int begin_font_count;
int begin_feature_count;
int begin_resource_count;
int begin_procset_count;
char data[CDSC_DATA_LENGTH];
unsigned int data_length;
unsigned int data_index;
DSC_OFFSET data_offset;
GSBOOL eof;
char *line;
unsigned int line_length;
GSBOOL eol;
GSBOOL last_cr;
unsigned int line_count;
GSBOOL long_line;
char last_line[256];
CDSCSTRING *string_head;
CDSCSTRING *string;
void *(*memalloc)(size_t size, void *closure_data);
void (*memfree)(void *ptr, void *closure_data);
void *mem_closure_data;
void (*debug_print_fn)(void *caller_data, const char *str);
int (*dsc_error_fn)(void *caller_data, CDSC *dsc,
unsigned int explanation, const char *line, unsigned int line_len);
CDSCFBBOX *hires_bbox;
CDSCFBBOX *crop_box;
CDCS2 *dcs2;
CDSCCOLOUR *colours;
int ref_count;
CDSCMACBIN *macbin;
};
CDSC *dsc_init(void *caller_data);
CDSC *dsc_init_with_alloc(
void *caller_data,
void *(*memalloc)(size_t size, void *closure_data),
void (*memfree)(void *ptr, void *closure_data),
void *closure_data);
void dsc_free(CDSC *dsc);
CDSC *dsc_new(void *caller_data);
int dsc_ref(CDSC *dsc);
int dsc_unref(CDSC *dsc);
void dsc_set_length(CDSC *dsc, DSC_OFFSET len);
int dsc_scan_data(CDSC *dsc, const char *data, int len);
int dsc_fixup(CDSC *dsc);
void dsc_set_error_function(CDSC *dsc,
int (*dsc_error_fn)(void *caller_data, CDSC *dsc,
unsigned int explanation, const char *line, unsigned int line_len));
void dsc_set_debug_function(CDSC *dsc,
void (*debug_fn)(void *caller_data, const char *str));
void dsc_debug_print(CDSC *dsc, const char *str);
const char * dsc_find_platefile(CDSC *dsc, int page);
int dsc_stricmp(const char *s, const char *t);
int dsc_add_page(CDSC *dsc, int ordinal, char *label);
int dsc_add_media(CDSC *dsc, CDSCMEDIA *media);
int dsc_set_page_bbox(CDSC *dsc, unsigned int page_number,
int llx, int lly, int urx, int ury);
void dsc_display(CDSC *dsc, void (*dfn)(void *ptr, const char *str));
#endif