#ifndef ivmspace_INCLUDED
#  define ivmspace_INCLUDED
#include "gsgc.h"
#define a_space (((1 << r_space_bits) - 1) << r_space_shift)
typedef enum {
avm_foreign = (i_vm_foreign << r_space_shift),
avm_system = (i_vm_system << r_space_shift),
avm_global = (i_vm_global << r_space_shift),
avm_local = (i_vm_local << r_space_shift),
avm_max = avm_local
} avm_space;
#define r_space(rp) (avm_space)(r_type_attrs(rp) & a_space)
#define r_space_index(rp) ((int)r_space(rp) >> r_space_shift)
#define r_set_space(rp,space) r_store_attrs(rp, a_space, (uint)space)
#define r_is_local(rp) (r_space(rp) == avm_local)
#define r_is_foreign(rp) (r_space(rp) == avm_foreign)
#define store_check_space(destspace,rpnew)\
if ( r_space(rpnew) > (destspace) )\
return_error(e_invalidaccess)
#define store_check_dest(rpdest,rpnew)\
store_check_space(r_space(rpdest), rpnew)
#define check_store_space(rdest,rnewcont)\
store_check_dest(&(rdest),&(rnewcont))
#endif