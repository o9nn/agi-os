#pragma incomplete struct _Recognizer
typedef struct _Recognizer* recognizer;
typedef struct _wordset *wordset;
typedef struct rc rc;
typedef struct rec_correlation rec_correlation;
typedef struct rec_alternative rec_alternative;
typedef struct rec_element rec_element;
typedef struct gesture gesture;
typedef uint wchar_t;
typedef int bool;
#define true 1
#define false 0
typedef void (*rec_fn)();
typedef uchar rec_confidence;
typedef struct {
char* ri_locale;
char* ri_name;
char** ri_subset;
} rec_info;
#define GESTURE "GESTURE"
#define MATHSET "MATHSET"
#define MONEYSET "MONEYSET"
#define WHITESPACE "WHITESPACE"
#define KANJI_JIS1 "KANJI_JIS1"
#define KANJI_JIS1_PLUS "KANJI_JIS1_PLUS"
#define KANJI_JIS2 "KANJI_JIS2"
#define HIRIGANA "HIRIGANA"
#define KATAKANA "KATAKANA"
#define UPPERCASE "UPPERCASE"
#define LOWERCASE "LOWERCASE"
#define DIGITS "DIGITS"
#define PUNCTUATION "PUNCTUATION"
#define NONALPHABETIC "NONALPHABETIC"
#define ASCII "ASCII"
#define ISO_LATIN12 "ISO_LATIN12"
typedef Rectangle pen_rect;
typedef struct {
pen_rect pr_area;
short pr_row, pr_col;
} pen_frame;
typedef struct _letterset {
char ls_type;
union _ls_set {
char* aval;
wchar_t* wval;
} ls_set;
} letterset;
#define REC_NONE 0x0
#define REC_GESTURE 0x1
#define REC_ASCII 0x2
#define REC_VAR 0x4
#define REC_WCHAR 0x8
#define REC_OTHER 0x10
#define REC_CORR 0x20
struct rec_element {
char re_type;
union {
gesture * gval;
char* aval;
wchar_t* wval;
rec_correlation* rcval;
} re_result;
rec_confidence re_conf;
};
struct rec_alternative {
rec_element ra_elem;
uint ra_nalter;
rec_alternative* ra_next;
};
struct gesture {
char* g_name;
uint g_nhs;
pen_point* g_hspots;
pen_rect g_bbox;
void (*g_action)(gesture*);
void* g_wsinfo;
};
typedef void (*xgesture)(gesture*);
struct rec_correlation {
rec_element ro_elem;
uint ro_nstrokes;
Stroke* ro_strokes;
uint* ro_start;
uint* ro_stop;
};
recognizer recognizer_load(char*, char*, char**);
int recognizer_unload(recognizer);
const rec_info* recognizer_get_info(recognizer);
const char* recognizer_manager_version(recognizer);
int recognizer_load_state(recognizer, char*, char*);
int recognizer_save_state(recognizer, char*, char*);
char* recognizer_error(recognizer);
wordset recognizer_load_dictionary(recognizer, char*, char*);
int recognizer_save_dictionary(recognizer, char*, char*, wordset);
int recognizer_free_dictionary(recognizer, wordset);
int recognizer_add_to_dictionary(recognizer, letterset*, wordset);
int recognizer_delete_from_dictionary(recognizer, letterset*, wordset);
int recognizer_set_context(recognizer, rc*);
rc* recognizer_get_context(recognizer);
int recognizer_clear(recognizer, bool);
int recognizer_get_buffer(recognizer, uint*, Stroke**);
int recognizer_set_buffer(recognizer, uint, Stroke*);
int recognizer_translate(recognizer, uint, Stroke*, bool,
int*, rec_alternative**);
rec_fn* recognizer_get_extension_functions(recognizer);
char** recognizer_get_gesture_names(recognizer);
xgesture recognizer_set_gesture_action(recognizer, char*, xgesture, void*);
void delete_rec_alternative_array(uint, rec_alternative*, bool);
void delete_rec_correlation(rec_correlation*, bool);
Stroke* make_Stroke_array(uint);
void delete_Stroke_array(uint, Stroke*, bool);
pen_point* make_pen_point_array(uint);
void delete_pen_point_array(pen_point*);
Stroke* copy_Stroke_array(uint, Stroke*);
#define LI_ISA_LI 0
#define LI_TRAIN 1
#define LI_CLEAR 2
#define LI_GET_CLASSES 3
#define LI_NUM_EX_FNS 4
typedef bool (*li_isa_li)(recognizer r);
typedef int (*li_recognizer_train)(recognizer, rc*, uint,
Stroke*, rec_element*, bool);
typedef int (*li_recognizer_clearState)(recognizer);
typedef int (*li_recognizer_getClasses)(recognizer, char ***, int *);