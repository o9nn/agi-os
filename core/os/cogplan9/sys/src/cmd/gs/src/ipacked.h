#ifndef ipacked_INCLUDED
# define ipacked_INCLUDED
#define r_packed_type_shift 13
#define r_packed_value_bits 12
typedef enum {
pt_full_ref = 0,
#define pt_min_packed 2
pt_executable_operator = 2,
pt_integer = 3,
pt_unused1 = 4,
pt_unused2 = 5,
#define pt_min_name 6
pt_literal_name = 6,
#define pt_min_exec_name 7
pt_executable_name = 7
} packed_type;
#define packed_per_ref (sizeof(ref) / sizeof(ref_packed))
#define align_packed_per_ref\
(arch_align_ref_mod / arch_align_short_mod)
#define pt_tag(pt) ((ref_packed)(pt) << r_packed_type_shift)
#define packed_value_mask ((1 << r_packed_value_bits) - 1)
#define packed_max_value packed_value_mask
#define r_is_packed(rp) (*(const ref_packed *)(rp) >= pt_tag(pt_min_packed))
#define r_packed_is_name(prp) (*(prp) >= pt_tag(pt_min_name))
#define r_packed_is_exec_name(prp) (*(prp) >= pt_tag(pt_min_exec_name))
#define packed_name_max_index packed_max_value
#define packed_name_index(prp) (*(prp) & packed_value_mask)
#define packed_min_intval (-(1 << (r_packed_value_bits - 1)))
#define packed_max_intval ((1 << (r_packed_value_bits - 1)) - 1)
#define packed_int_mask packed_value_mask
#define lp_mark_shift 12
#define lp_mark (1 << lp_mark_shift)
#define r_has_pmark(rp) (*(rp) & lp_mark)
#define r_set_pmark(rp) (*(rp) |= lp_mark)
#define r_clear_pmark(rp) (*(rp) &= ~lp_mark)
#define r_store_pmark(rp,pm) (*(rp) = (*(rp) & ~lp_mark) | (pm))
#define packed_next(prp)\
(r_is_packed(prp) ? prp + 1 : prp + packed_per_ref)
#define ref_array_packing_container i_ctx_p
#define ref_array_packing (ref_array_packing_container->array_packing)
#endif