#ifndef ccfont_INCLUDED
# define ccfont_INCLUDED
#include "stdpre.h"
#include "gsmemory.h"
#include "iref.h"
#include "ivmspace.h"
#include "store.h"
#define ref_(t) struct { struct tas_s tas; t value; }
#define boolean_v(b) { {t_boolean<<r_type_shift}, (ushort)(b) }
#define integer_v(i) { {t_integer<<r_type_shift}, (long)(i) }
#define null_v() { {t_null<<r_type_shift} }
#define real_v(v) { {t_real<<r_type_shift}, (float)(v) }
typedef struct {
byte encx, charx;
} charindex;
typedef const char *cfont_string_array;
typedef struct {
const charindex *enc_keys;
uint num_enc_keys;
uint num_str_keys;
uint extra_slots;
uint dict_attrs;
uint value_attrs;
} cfont_dict_keys;
typedef struct cfont_procs_s {
int (*ref_dict_create) (i_ctx_t *, ref *, const cfont_dict_keys *,
cfont_string_array, const ref *);
int (*string_dict_create) (i_ctx_t *, ref *, const cfont_dict_keys *,
cfont_string_array, cfont_string_array);
int (*num_dict_create) (i_ctx_t *, ref *, const cfont_dict_keys *,
cfont_string_array, const ref *, const char *);
int (*name_array_create) (i_ctx_t *, ref *, cfont_string_array, int);
int (*string_array_create) (i_ctx_t *, ref *, cfont_string_array,
int , uint );
int (*scalar_array_create) (i_ctx_t *, ref *, const ref *,
int , uint );
int (*name_create) (i_ctx_t *, ref *, const char *);
int (*ref_from_string) (i_ctx_t *, ref *, const char *, uint);
} cfont_procs;
#define ccfont_proc(proc)\
int proc(i_ctx_t *, const cfont_procs *, ref *)
typedef ccfont_proc((*ccfont_fproc));
extern int ccfont_fprocs(int *, const ccfont_fproc **);
#define ccfont_version 19
#endif