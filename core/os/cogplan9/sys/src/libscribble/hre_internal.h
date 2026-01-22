struct _wordset {
char* ws_pathname;
recognizer ws_recognizer;
void* ws_internal;
};
struct _Recognizer {
uint		recognizer_magic;
char		*recognizer_version;
rec_info	*recognizer_info;
void		*recognizer_specific;
int		(*recognizer_load_state)(struct _Recognizer*, char*, char*);
int		(*recognizer_save_state)(struct _Recognizer*, char*, char*);
char*		(*recognizer_error)(struct _Recognizer*);
wordset		(*recognizer_load_dictionary)(struct _Recognizer*, char*, char*);
int		(*recognizer_save_dictionary)(struct _Recognizer*, char*, char*, wordset);
int		(*recognizer_free_dictionary)(struct _Recognizer*, wordset);
int		(*recognizer_add_to_dictionary)(struct _Recognizer*, letterset*, wordset);
int		(*recognizer_delete_from_dictionary)(struct _Recognizer*, letterset*, wordset);
int		(*recognizer_set_context)(struct _Recognizer*,rc*);
rc*		(*recognizer_get_context)(struct _Recognizer*);
int		(*recognizer_clear)(struct _Recognizer*, bool);
int		(*recognizer_get_buffer)(struct _Recognizer*, uint*, Stroke**);
int		(*recognizer_set_buffer)(struct _Recognizer*, uint, Stroke*);
int		(*recognizer_translate)(struct _Recognizer*, uint, Stroke*, bool, int*, rec_alternative**);
rec_fn*		(*recognizer_get_extension_functions)(struct _Recognizer*);
char**		(*recognizer_get_gesture_names)(struct _Recognizer*);
xgesture	(*recognizer_set_gesture_action)(struct _Recognizer*, char*, xgesture, void*);
uint recognizer_end_magic;
};
typedef recognizer (*recognizer_internal_initialize)(rec_info* ri);
#define RECOGNIZER_INITIALIZE(_a) \
recognizer __recognizer_internal_initialize(rec_info* _a)
typedef int (*recognizer_internal_finalize)(recognizer r);
#define RECOGNIZER_FINALIZE(_a) \
int __recognizer_internal_finalize(recognizer _a)
recognizer			make_recognizer(rec_info* ri);
void 				delete_recognizer(recognizer rec);
RECOGNIZER_FINALIZE(_a);
rec_alternative*	make_rec_alternative_array(uint size);
rec_correlation*	make_rec_correlation(char type, uint size, void* trans, rec_confidence conf, uint ps_size);
rec_fn*
make_rec_fn_array(uint size);
void
delete_rec_fn_array(rec_fn* rf);
gesture*
initialize_gesture(gesture* g,
char* name,
uint nhs,
pen_point* hspots,
pen_rect bbox,
xgesture cback,
void* wsinfo);
gesture*
make_gesture_array(uint size);
void
delete_gesture_array(uint size,gesture* ga,bool delete_points_p);
Stroke*
concatenate_Strokes(int nstrokes1,
Stroke* strokes1,
int nstrokes2,
Stroke* strokes2,
int* nstrokes3,
Stroke** strokes3);
rec_alternative* initialize_rec_alternative(rec_alternative* ra, uint);
rec_element* initialize_rec_element(rec_element*, char, uint, void*, rec_confidence);
#define REC_DEFAULT_LOCALE  	"C"
#define RECHOME			"RECHOME"
#define LANG			"LANG"