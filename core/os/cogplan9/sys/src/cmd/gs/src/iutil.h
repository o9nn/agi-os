#ifndef iutil_INCLUDED
# define iutil_INCLUDED
void refcpy_to_new(ref * to, const ref * from, uint size,
gs_dual_memory_t *dmem);
int refcpy_to_old(ref * aref, uint index, const ref * from, uint size,
gs_dual_memory_t *dmem, client_name_t cname);
void refset_null_new(ref * to, uint size, uint new_mask);
#define refset_null(to, size) refset_null_new(to, size, ialloc_new_mask)
bool obj_eq(const gs_memory_t *mem, const ref *, const ref *);
bool obj_ident_eq(const gs_memory_t *mem, const ref *, const ref *);
int obj_string_data(const gs_memory_t *mem, const ref *op, const byte **pchars, uint *plen);
#define CVP_MAX_STRING 200
int obj_cvp(const ref * op, byte *str, uint len, uint * prlen,
int full_print, uint start_pos, const gs_memory_t *mem);
int obj_cvs(const gs_memory_t *mem, const ref * op, byte * str, uint len, uint * prlen,
const byte ** pchars);
int array_get(const gs_memory_t *mem, const ref *, long, ref *);
void packed_get(const gs_memory_t *mem, const ref_packed *, ref *);
int refs_check_space(const ref * refs, uint size, uint space);
int string_to_ref(const char *, ref *, gs_ref_memory_t *, client_name_t);
char *ref_to_string(const ref *, gs_memory_t *, client_name_t);
int num_params(const ref *, int, double *);
int float_params(const ref *, int, float *);
int process_float_array(const gs_memory_t *mem, const ref *, int, float *);
int real_param(const ref *, double *);
int float_param(const ref *, float *);
int int_param(const ref *, int, int *);
int make_reals(ref *, const double *, int);
int make_floats(ref *, const float *, int);
#ifndef gs_matrix_DEFINED
# define gs_matrix_DEFINED
typedef struct gs_matrix_s gs_matrix;
#endif
int read_matrix(const gs_memory_t *mem, const ref *, gs_matrix *);
int write_matrix_in(ref *op, const gs_matrix *pmat, gs_dual_memory_t *dmem,
gs_ref_memory_t *imem);
#define write_matrix_new(op, pmat, imem)\
write_matrix_in(op, pmat, NULL, imem)
#define write_matrix(op, pmat)\
write_matrix_in(op, pmat, idmemory, NULL)
#endif