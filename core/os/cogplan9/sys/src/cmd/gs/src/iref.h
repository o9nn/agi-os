#ifndef iref_INCLUDED
# define iref_INCLUDED
#ifndef ref_DEFINED
typedef struct ref_s ref;
# define ref_DEFINED
#endif
typedef ushort ref_packed;
#define log2_sizeof_ref_packed arch_log2_sizeof_short
#define sizeof_ref_packed (1 << log2_sizeof_ref_packed)
typedef enum {
t__invalid,
t_boolean,
t_dictionary,
t_file,
#define _REF_T_ARRAY_SPAN 4
t_array,
t_mixedarray,
t_shortarray,
t_unused_array_,
#define _REF_T_STRUCT_SPAN 2
t_struct,
t_astruct,
t_fontID,
t_integer,
t_mark,
t_name,
t_null,
t_operator,
t_real,
t_save,
t_string,
t_device,
t_oparray,
t_next_index
} ref_type;
#define _REF_TYPE_USES_ACCESS 1
#define _REF_TYPE_USES_SIZE 2
#define _REF_TYPE_IS_NULL 4
#define _REF_TYPE_IS_DICTIONARY 8
extern const byte ref_type_properties[1 << 6];
#define REF_TYPE_PROPERTIES_DATA\
0, \
0, \
_REF_TYPE_USES_ACCESS | _REF_TYPE_IS_DICTIONARY, \
_REF_TYPE_USES_ACCESS | _REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_ACCESS | _REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_ACCESS | _REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_ACCESS | _REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_ACCESS | _REF_TYPE_USES_SIZE, \
0, \
_REF_TYPE_USES_ACCESS, \
0, \
0, \
0, \
_REF_TYPE_USES_SIZE, \
_REF_TYPE_IS_NULL, \
_REF_TYPE_USES_SIZE, \
0, \
0, \
_REF_TYPE_USES_ACCESS | _REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_ACCESS, \
_REF_TYPE_USES_SIZE, \
\
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE, \
_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE,_REF_TYPE_USES_SIZE
#define _REF_TYPE_HAS(rtype,props)\
((ref_type_properties[rtype] & (props)) != 0)
#define ref_type_uses_access(rtype)\
_REF_TYPE_HAS(rtype, _REF_TYPE_USES_ACCESS)
#define ref_type_uses_size(rtype)\
_REF_TYPE_HAS(rtype, _REF_TYPE_USES_SIZE)
#define ref_type_uses_size_or_null(rtype)\
_REF_TYPE_HAS(rtype, _REF_TYPE_USES_SIZE | _REF_TYPE_IS_NULL)
#define REF_TYPE_DEBUG_PRINT_STRINGS\
"INVL","bool","dict","file",\
"arry","mpry","spry","u?ry",\
"STRC","ASTR",\
"font","int ","mark","name","null",\
"oper","real","save","str ",\
"devc","opry"
#define REF_TYPE_NAME_STRINGS\
0,"booleantype","dicttype","filetype",\
"arraytype","packedarraytype","packedarraytype","arraytype",\
0,0,\
"fonttype","integertype","marktype","nametype","nulltype",\
"operatortype","realtype","savetype","stringtype",\
"devicetype","operatortype"
#define REF_TYPE_PRINT_STRINGS\
0,0,"-dict-","-file-",\
"-array-","-packedarray-","-packedarray-","-array-",\
0,0,\
"-fontID-",0,"-mark-",0,0,\
0,0,"-save-","-string-",\
"-device-",0
#define l_mark 1
#define l_new 2
#define r_space_bits 2
#define r_space_shift 2
#define a_write 0x10
#define a_read 0x20
#define a_execute 0x40
#define a_readonly (a_read + a_execute)
#define a_all (a_write + a_read+a_execute)
#define a_executable 0x80
#define r_type_shift 8
#define r_type_bits 6
typedef struct ref_attr_print_mask_s {
ushort mask;
ushort value;
char print;
} ref_attr_print_mask_t;
#define _REF_ATTR_PRINT_FLAG(m,c)\
{m,m,c},{m,0,'-'}
#define _REF_ATTR_PRINT_SPACE(v,c)\
{((1<<r_space_bits)-1)<<r_space_shift,v,c}
#define REF_ATTR_PRINT_MASKS\
_REF_ATTR_PRINT_FLAG(l_mark,'m'),\
_REF_ATTR_PRINT_FLAG(l_new,'n'),\
_REF_ATTR_PRINT_SPACE(avm_foreign,'F'),\
_REF_ATTR_PRINT_SPACE(avm_system,'S'),\
_REF_ATTR_PRINT_SPACE(avm_global,'G'),\
_REF_ATTR_PRINT_SPACE(avm_local,'L'),\
_REF_ATTR_PRINT_FLAG(a_write,'w'),\
_REF_ATTR_PRINT_FLAG(a_read,'r'),\
_REF_ATTR_PRINT_FLAG(a_execute,'x'),\
_REF_ATTR_PRINT_FLAG(a_executable,'e'),\
_REF_ATTR_PRINT_FLAG(0x4000,'?'),\
_REF_ATTR_PRINT_FLAG(0x8000,'?')
typedef struct dict_s dict;
typedef struct name_s name;
#ifndef stream_DEFINED
# define stream_DEFINED
typedef struct stream_s stream;
#endif
#ifndef gx_device_DEFINED
# define gx_device_DEFINED
typedef struct gx_device_s gx_device;
#endif
#ifndef obj_header_DEFINED
# define obj_header_DEFINED
typedef struct obj_header_s obj_header_t;
#endif
#ifndef i_ctx_t_DEFINED
# define i_ctx_t_DEFINED
typedef struct gs_context_state_s i_ctx_t;
#endif
typedef int (*op_proc_t)(i_ctx_t *i_ctx_p);
#define real_opproc(pref) ((pref)->value.opproc)
struct tas_s {
ushort type_attrs;
ushort rsize;
};
struct ref_s {
struct tas_s tas;
union v {
long intval;
ushort boolval;
float realval;
ulong saveid;
byte *bytes;
const byte *const_bytes;
ref *refs;
const ref *const_refs;
name *pname;
const name *const_pname;
dict *pdict;
const dict *const_pdict;
const ref_packed *packed;
ref_packed *writable_packed;
op_proc_t opproc;
struct stream_s *pfile;
struct gx_device_s *pdevice;
obj_header_t *pstruct;
} value;
};
#define _REF_HAS_MASKED_TYPE_ATTRS(rp,typ,tspan,mask)\
(((rp)->tas.type_attrs &\
((((1 << r_type_bits) - (tspan)) << r_type_shift) + (mask))) ==\
(((typ) << r_type_shift) + (mask)))
#define r_size(rp) ((rp)->tas.rsize)
#define r_inc_size(rp,inc) ((rp)->tas.rsize += (inc))
#define r_dec_size(rp,dec) ((rp)->tas.rsize -= (dec))
#define r_set_size(rp,siz) ((rp)->tas.rsize = (siz))
#if r_type_shift == 8
# if arch_is_big_endian
# define r_type(rp) (((const byte *)&((rp)->tas.type_attrs))[sizeof(ushort)-2])
# else
# define r_type(rp) (((const byte *)&((rp)->tas.type_attrs))[1])
# endif
# define r_has_type(rp,typ) (r_type(rp) == (typ))
#else
# define r_type(rp) ((rp)->tas.type_attrs >> r_type_shift)
# define r_has_type(rp,typ) r_has_type_attrs(rp,typ,0)
#endif
#define r_btype(rp)\
((rp)->tas.type_attrs >= (t_next_index << r_type_shift) ?\
t_operator : r_type(rp))
#define r_is_array(rp)\
_REF_HAS_MASKED_TYPE_ATTRS(rp,t_array,_REF_T_ARRAY_SPAN,0)
#define r_is_proc(rp)\
_REF_HAS_MASKED_TYPE_ATTRS(rp,t_array,_REF_T_ARRAY_SPAN,a_execute+a_executable)
#define r_is_struct(rp)\
_REF_HAS_MASKED_TYPE_ATTRS(rp,t_struct,_REF_T_STRUCT_SPAN,0)
#define r_has_stype(rp,mem,styp)\
(r_is_struct(rp) && gs_object_type(mem, (rp)->value.pstruct) == &styp)
#define r_set_type(rp,typ) ((rp)->tas.type_attrs = (typ) << r_type_shift)
#define r_type_attrs(rp) ((rp)->tas.type_attrs)
#define r_has_type_attrs(rp,typ,mask)\
_REF_HAS_MASKED_TYPE_ATTRS(rp,typ,1,mask)
#define r_set_type_attrs(rp,typ,mask)\
((rp)->tas.type_attrs = ((typ) << r_type_shift) + (mask))
#define _REF_TYPE_XE_SHIFT (r_type_shift - 2)
#define _REF_TAS_TYPE_XE(tas) ((tas) >> _REF_TYPE_XE_SHIFT)
#define r_type_xe(rp)\
_REF_TAS_TYPE_XE(((const ushort *)(rp))[offset_of(ref, tas.type_attrs) / sizeof(ushort)])
#define type_xe_value(typ,xe) _REF_TAS_TYPE_XE(((typ) << r_type_shift) + (xe))
#define r_has_attr(rp,mask1) \
(r_type_attrs(rp) & (mask1))
#define r_has_attrs(rp,mask) !(~r_type_attrs(rp) & (mask))
#define r_has_masked_attrs(rp,attrs,mask)\
((r_type_attrs(rp) & (mask)) == (attrs))
#define r_set_attrs(rp,mask) ((rp)->tas.type_attrs |= (mask))
#define r_clear_attrs(rp,mask) ((rp)->tas.type_attrs &= ~(mask))
#define r_store_attrs(rp,mask,attrs)\
((rp)->tas.type_attrs = ((rp)->tas.type_attrs & ~(mask)) | (attrs))
#define r_copy_attrs(rp,mask,sp)\
r_store_attrs(rp,mask,(sp)->tas.type_attrs & (mask))
#define r_ptr(rp,typ) ((typ *)((rp)->value.pstruct))
#define r_set_ptr(rp,ptr) ((rp)->value.pstruct = (obj_header_t *)(ptr))
#define empty_ref_data(type, attrs)\
{ { ((type) << r_type_shift) | (attrs),\
0 } }
#define arch_sizeof_ref sizeof(ref)
#define arch_align_ref_mod\
(((arch_align_long_mod - 1) | (arch_align_float_mod - 1) |\
(arch_align_ptr_mod - 1)) + 1)
#define max_array_size (max_ushort & (max_uint / (uint)arch_sizeof_ref))
#define max_string_size max_ushort
#endif