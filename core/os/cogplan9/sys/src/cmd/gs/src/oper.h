#ifndef oper_INCLUDED
#  define oper_INCLUDED
#include "ierrors.h"
#include "ostack.h"
#include "opdef.h"
#include "opextern.h"
#include "opcheck.h"
#include "iutil.h"
int check_type_failed(const ref *);
#define return_op_typecheck(op)\
return_error(check_type_failed(op))
#define check_type(orf,typ)\
if ( !r_has_type(&orf,typ) ) return_op_typecheck(&orf)
#define check_stype(orf,styp)\
if ( !r_has_stype(&orf,imemory,styp) ) return_op_typecheck(&orf)
#define check_array(orf)\
check_array_else(orf, return_op_typecheck(&orf))
#define check_type_access(orf,typ,acc1)\
if ( !r_has_type_attrs(&orf,typ,acc1) )\
return_error((!r_has_type(&orf,typ) ? check_type_failed(&orf) :\
e_invalidaccess))
#define check_read_type(orf,typ)\
check_type_access(orf,typ,a_read)
#define check_write_type(orf,typ)\
check_type_access(orf,typ,a_write)
#define NYI(msg) if ( 1 ) return_error(e_undefined)
#define o_push_estack 5
#define o_pop_estack 14
#define o_reschedule 22
#endif