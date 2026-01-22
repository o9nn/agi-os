#ifndef idparam_INCLUDED
#  define idparam_INCLUDED
#ifndef gs_matrix_DEFINED
#  define gs_matrix_DEFINED
typedef struct gs_matrix_s gs_matrix;
#endif
#ifndef gs_uid_DEFINED
#  define gs_uid_DEFINED
typedef struct gs_uid_s gs_uid;
#endif
int dict_bool_param(const ref * pdict, const char *kstr,
bool defaultval, bool * pvalue);
int dict_int_param(const ref * pdict, const char *kstr,
int minval, int maxval, int defaultval, int *pvalue);
int dict_int_null_param(const ref * pdict, const char *kstr,
int minval, int maxval, int defaultval,
int *pvalue);
int dict_uint_param(const ref * pdict, const char *kstr,
uint minval, uint maxval, uint defaultval,
uint * pvalue);
int dict_float_param(const ref * pdict, const char *kstr,
floatp defaultval, float *pvalue);
int dict_int_array_check_param(const ref * pdict, const char *kstr,
uint len, int *ivec,
int under_error, int over_error);
int dict_int_array_param(const ref * pdict, const char *kstr,
uint maxlen, int *ivec);
int dict_ints_param(const ref * pdict, const char *kstr,
uint len, int *ivec);
int dict_float_array_check_param(const gs_memory_t *mem,
const ref * pdict, const char *kstr,
uint len, float *fvec,
const float *defaultvec,
int under_error, int over_error);
int dict_float_array_param(const gs_memory_t *mem,
const ref * pdict, const char *kstr,
uint maxlen, float *fvec,
const float *defaultvec);
int dict_floats_param(const gs_memory_t *mem,
const ref * pdict, const char *kstr,
uint len, float *fvec,
const float *defaultvec);
int dict_proc_param(const ref * pdict, const char *kstr, ref * pproc,
bool defaultval);
int dict_matrix_param(const gs_memory_t *mem,
const ref * pdict, const char *kstr,
gs_matrix * pmat);
int dict_uid_param(const ref * pdict, gs_uid * puid, int defaultval,
gs_memory_t * mem, const i_ctx_t *i_ctx_p);
bool dict_check_uid_param(const ref * pdict, const gs_uid * puid);
#endif