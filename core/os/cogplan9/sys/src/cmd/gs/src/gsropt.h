#ifndef gsropt_INCLUDED
#  define gsropt_INCLUDED
#define TRANSPARENCY_PER_H_P
typedef enum {
rop2_0 = 0,
rop2_S = 0xc,
#define rop2_S_shift 2
rop2_D = 0xa,
#define rop2_D_shift 1
rop2_1 = 0xf,
#define rop2_operand(shift, d, s)\
((shift) == 2 ? (s) : (d))
rop2_default = rop2_S
} gs_rop2_t;
typedef enum {
rop3_0 = 0,
rop3_T = 0xf0,
#define rop3_T_shift 4
rop3_S = 0xcc,
#define rop3_S_shift 2
rop3_D = 0xaa,
#define rop3_D_shift 1
rop3_1 = 0xff,
rop3_default = rop3_T | rop3_S
} gs_rop3_t;
#define rop3_invert_(op, mask, shift)\
( (((op) & mask) >> shift) | (((op) & (rop3_1 - mask)) << shift) |\
((op) & ~rop3_1) )
#define rop3_invert_D(op) rop3_invert_(op, rop3_D, rop3_D_shift)
#define rop3_invert_S(op) rop3_invert_(op, rop3_S, rop3_S_shift)
#define rop3_invert_T(op) rop3_invert_(op, rop3_T, rop3_T_shift)
#define rop3_know_0_(op, mask, shift)\
( (((op) & (rop3_1 - mask)) << shift) | ((op) & ~mask) )
#define rop3_know_D_0(op) rop3_know_0_(op, rop3_D, rop3_D_shift)
#define rop3_know_S_0(op) rop3_know_0_(op, rop3_S, rop3_S_shift)
#define rop3_know_T_0(op) rop3_know_0_(op, rop3_T, rop3_T_shift)
#define rop3_know_1_(op, mask, shift)\
( (((op) & mask) >> shift) | ((op) & ~(rop3_1 - mask)) )
#define rop3_know_D_1(op) rop3_know_1_(op, rop3_D, rop3_D_shift)
#define rop3_know_S_1(op) rop3_know_1_(op, rop3_S, rop3_S_shift)
#define rop3_know_T_1(op) rop3_know_1_(op, rop3_T, rop3_T_shift)
#define rop3_swap_S_T(op)\
( (((op) & rop3_S & ~rop3_T) << (rop3_T_shift - rop3_S_shift)) |\
(((op) & ~rop3_S & rop3_T) >> (rop3_T_shift - rop3_S_shift)) |\
((op) & (~rop3_1 | (rop3_S ^ rop3_T))) )
#define rop3_use_D_when_0_(op, mask)\
(((op) & ~(rop3_1 - mask)) | (rop3_D & ~mask))
#define rop3_use_D_when_1_(op, mask)\
(((op) & ~mask) | (rop3_D & mask))
#define rop3_use_D_when_S_0(op) rop3_use_D_when_0_(op, rop3_S)
#define rop3_use_D_when_S_1(op) rop3_use_D_when_1_(op, rop3_S)
#define rop3_use_D_when_T_0(op) rop3_use_D_when_0_(op, rop3_T)
#define rop3_use_D_when_T_1(op) rop3_use_D_when_1_(op, rop3_T)
#define rop3_not(op) ((op) ^ rop3_1)
#define rop3_uses_(op, mask, shift)\
( ((((op) << shift) ^ (op)) & mask) != 0 )
#define rop3_uses_D(op) rop3_uses_(op, rop3_D, rop3_D_shift)
#define rop3_uses_S(op) rop3_uses_(op, rop3_S, rop3_S_shift)
#define rop3_uses_T(op) rop3_uses_(op, rop3_T, rop3_T_shift)
#define rop3_is_idempotent(op)\
!( (op) & ~((op) << rop3_D_shift) & rop3_D )
#define source_transparent_default false
#define pattern_transparent_default false
#define lop_rop(lop) ((gs_rop3_t)((lop) & 0xff))
#define lop_S_transparent 0x100
#define lop_T_transparent 0x200
#define lop_pdf14 0x4000
#define lop_ral_shift 10
#define lop_ral_mask 0xf
typedef uint gs_logical_operation_t;
#define lop_default\
(rop3_default |\
(source_transparent_default ? lop_S_transparent : 0) |\
(pattern_transparent_default ? lop_T_transparent : 0))
#ifdef TRANSPARENCY_PER_H_P
#define lop_uses_S(lop)\
(rop3_uses_S(lop) || ((lop) & (lop_S_transparent | lop_T_transparent)))
#else
#define lop_uses_S(lop)\
(rop3_uses_S(lop) || ((lop) & lop_S_transparent))
#endif
#define lop_uses_T(lop)\
(rop3_uses_T(lop) || ((lop) & lop_T_transparent))
#define lop_no_T_is_S(lop)\
(((lop) & (lop_S_transparent | (rop3_1 - rop3_T))) == (rop3_S & ~rop3_T))
#define lop_no_S_is_T(lop)\
(((lop) & (lop_T_transparent | (rop3_1 - rop3_S))) == (rop3_T & ~rop3_S))
#define lop_is_idempotent(lop) (rop3_is_idempotent(lop) && !(lop & lop_pdf14))
#define lop_know_S_0(lop)\
(rop3_know_S_0(lop) & ~lop_S_transparent)
#define lop_know_T_0(lop)\
(rop3_know_T_0(lop) & ~lop_T_transparent)
#define lop_know_S_1(lop)\
(lop & lop_S_transparent ? rop3_D : rop3_know_S_1(lop))
#define lop_know_T_1(lop)\
(lop & lop_T_transparent ? rop3_D : rop3_know_T_1(lop))
typedef unsigned long rop_operand;
typedef rop_operand (*rop_proc)(rop_operand D, rop_operand S, rop_operand T);
typedef enum {
rop_usage_none = 0,
rop_usage_D = 1,
rop_usage_S = 2,
rop_usage_DS = 3,
rop_usage_T = 4,
rop_usage_DT = 5,
rop_usage_ST = 6,
rop_usage_DST = 7
} rop_usage_t;
extern const rop_proc rop_proc_table[256];
extern const byte  rop_usage_table[256];
#endif