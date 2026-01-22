#ifndef gdevpdfo_INCLUDED
#  define gdevpdfo_INCLUDED
#include "gsparam.h"
#ifndef gx_device_pdf_DEFINED
#  define gx_device_pdf_DEFINED
typedef struct gx_device_pdf_s gx_device_pdf;
#endif
#ifndef cos_types_DEFINED
#  define cos_types_DEFINED
typedef struct cos_object_s cos_object_t;
typedef struct cos_stream_s cos_stream_t;
typedef struct cos_dict_s cos_dict_t;
typedef struct cos_array_s cos_array_t;
typedef struct cos_value_s cos_value_t;
typedef struct cos_object_procs_s cos_object_procs_t;
typedef const cos_object_procs_t *cos_type_t;
#endif
typedef struct cos_element_s cos_element_t;
typedef struct cos_stream_piece_s cos_stream_piece_t;
struct cos_object_procs_s {
#define cos_proc_release(proc)\
void proc(cos_object_t *pco, client_name_t cname)
cos_proc_release((*release));
#define cos_proc_write(proc)\
int proc(const cos_object_t *pco, gx_device_pdf *pdev, gs_id object_id)
cos_proc_write((*write));
#define cos_proc_equal(proc)\
int proc(const cos_object_t *pco0, const cos_object_t *pco1, gx_device_pdf *pdev)
cos_proc_equal((*equal));
} ;
#define cos_type(pco) ((pco)->cos_procs)
#define cos_object_struct(otype_s, etype)\
struct otype_s {\
const cos_object_procs_t *cos_procs;	\
long id;\
etype *elements;\
cos_stream_piece_t *pieces;\
gx_device_pdf *pdev;\
pdf_resource_t *pres;	\
byte is_open;		\
byte is_graphics;		\
byte written;		\
long length;                \
stream *input_strm;		\
\
\
}
cos_object_struct(cos_object_s, cos_element_t);
#define private_st_cos_object()	\
gs_private_st_ptrs5(st_cos_object, cos_object_t, "cos_object_t",\
cos_object_enum_ptrs, cos_object_reloc_ptrs, elements, pieces,\
pdev, pres, input_strm)
extern const cos_object_procs_t cos_generic_procs;
#define cos_type_generic (&cos_generic_procs)
#define COS_OBJECT(pc) ((cos_object_t *)&((pc)->cos_procs))
#define CONST_COS_OBJECT(pc) ((const cos_object_t *)&((pc)->cos_procs))
typedef enum {
COS_VALUE_SCALAR = 0,
COS_VALUE_CONST,
COS_VALUE_OBJECT,
COS_VALUE_RESOURCE
} cos_value_type_t;
struct cos_value_s {
cos_value_type_t value_type;
union vc_ {
gs_string chars;
cos_object_t *object;
} contents;
};
#define private_st_cos_value()	\
gs_private_st_composite(st_cos_value, cos_value_t,\
"cos_value_t", cos_value_enum_ptrs, cos_value_reloc_ptrs)
typedef struct cos_array_element_s cos_array_element_t;
cos_object_struct(cos_array_s, cos_array_element_t);
extern const cos_object_procs_t cos_array_procs;
#define cos_type_array (&cos_array_procs)
typedef struct cos_dict_element_s cos_dict_element_t;
cos_object_struct(cos_dict_s, cos_dict_element_t);
extern const cos_object_procs_t cos_dict_procs;
#define cos_type_dict (&cos_dict_procs)
cos_object_struct(cos_stream_s, cos_dict_element_t);
extern const cos_object_procs_t cos_stream_procs;
#define cos_type_stream (&cos_stream_procs)
cos_object_t *cos_object_alloc(gx_device_pdf *, client_name_t);
cos_array_t *cos_array_alloc(gx_device_pdf *, client_name_t);
cos_array_t *cos_array_from_floats(gx_device_pdf *, const float *, uint,
client_name_t);
cos_dict_t *cos_dict_alloc(gx_device_pdf *, client_name_t);
cos_stream_t *cos_stream_alloc(gx_device_pdf *, client_name_t);
gs_memory_t *cos_object_memory(const cos_object_t *);
#define COS_OBJECT_MEMORY(pc) cos_object_memory(CONST_COS_OBJECT(pc))
int cos_become(cos_object_t *, cos_type_t);
cos_proc_release(cos_release);
#define COS_RELEASE(pc, cname) cos_release(COS_OBJECT(pc), cname)
cos_proc_write(cos_write);
#define COS_WRITE(pc, pdev) cos_write(CONST_COS_OBJECT(pc), pdev, (pc)->id)
const cos_value_t *cos_string_value(cos_value_t *, const byte *, uint);
const cos_value_t *cos_c_string_value(cos_value_t *, const char *);
const cos_value_t *cos_object_value(cos_value_t *, cos_object_t *);
#define COS_OBJECT_VALUE(pcv, pc) cos_object_value(pcv, COS_OBJECT(pc))
const cos_value_t *cos_resource_value(cos_value_t *, cos_object_t *);
#define COS_RESOURCE_VALUE(pcv, pc) cos_resource_value(pcv, COS_OBJECT(pc))
#define COS_VALUE_IS_OBJECT(pv) ((pv)->value_type >= COS_VALUE_OBJECT)
int cos_array_put(cos_array_t *, long, const cos_value_t *);
int cos_array_put_no_copy(cos_array_t *, long, const cos_value_t *);
int cos_array_add(cos_array_t *, const cos_value_t *);
int cos_array_add_no_copy(cos_array_t *, const cos_value_t *);
int cos_array_add_c_string(cos_array_t *, const char *);
int cos_array_add_int(cos_array_t *, int);
int cos_array_add_real(cos_array_t *, floatp);
int cos_array_add_object(cos_array_t *, cos_object_t *);
int cos_array_unadd(cos_array_t *, cos_value_t *);
int cos_dict_put(cos_dict_t *, const byte *, uint, const cos_value_t *);
int cos_dict_put_no_copy(cos_dict_t *, const byte *, uint,
const cos_value_t *);
int cos_dict_put_c_key(cos_dict_t *, const char *, const cos_value_t *);
int cos_dict_put_c_key_string(cos_dict_t *, const char *, const byte *, uint);
int cos_dict_put_c_key_int(cos_dict_t *, const char *, int);
int cos_dict_put_c_key_bool(cos_dict_t *pcd, const char *key, bool value);
int cos_dict_put_c_key_real(cos_dict_t *, const char *, floatp);
int cos_dict_put_c_key_floats(cos_dict_t *, const char *, const float *, uint);
int cos_dict_put_c_key_object(cos_dict_t *, const char *, cos_object_t *);
int cos_dict_put_string(cos_dict_t *, const byte *, uint, const byte *, uint);
int cos_dict_put_string_copy(cos_dict_t *pcd, const char *key, const char *value);
int cos_dict_put_c_strings(cos_dict_t *, const char *, const char *);
int cos_dict_move_all(cos_dict_t *, cos_dict_t *);
int cos_stream_add(cos_stream_t *, uint);
int cos_stream_add_bytes(cos_stream_t *, const byte *, uint);
int cos_stream_add_stream_contents(cos_stream_t *, stream *);
int cos_stream_release_pieces(cos_stream_t *pcs);
cos_dict_t *cos_stream_dict(cos_stream_t *);
const cos_array_element_t *
cos_array_element_first(const cos_array_t *);
const cos_array_element_t *
cos_array_element_next(const cos_array_element_t *, long *,
const cos_value_t **);
const cos_value_t *cos_dict_find(const cos_dict_t *, const byte *, uint);
const cos_value_t *cos_dict_find_c_key(const cos_dict_t *, const char *);
typedef struct cos_param_list_writer_s {
gs_param_list_common;
cos_dict_t *pcd;
int print_ok;
} cos_param_list_writer_t;
int cos_param_list_writer_init(cos_param_list_writer_t *, cos_dict_t *,
int print_ok);
stream *cos_write_stream_alloc(cos_stream_t *pcs, gx_device_pdf *pdev,
client_name_t cname);
cos_stream_t * cos_stream_from_pipeline(stream *s);
stream * cos_write_stream_from_pipeline(stream *s);
int cos_value_write(const cos_value_t *, gx_device_pdf *);
int cos_dict_elements_write(const cos_dict_t *, gx_device_pdf *);
int cos_stream_elements_write(const cos_stream_t *, gx_device_pdf *);
int cos_stream_contents_write(const cos_stream_t *, gx_device_pdf *);
long cos_stream_length(const cos_stream_t *pcs);
int cos_dict_objects_write(const cos_dict_t *, gx_device_pdf *);
int cos_dict_objects_delete(cos_dict_t *);
int cos_write_object(cos_object_t *pco, gx_device_pdf *pdev);
#define COS_WRITE_OBJECT(pc, pdev) cos_write_object(COS_OBJECT(pc), pdev)
void cos_value_free(const cos_value_t *, const cos_object_t *, client_name_t);
void cos_free(cos_object_t *pco, client_name_t cname);
#define COS_FREE(pc, cname) cos_free(COS_OBJECT(pc), cname)
#endif