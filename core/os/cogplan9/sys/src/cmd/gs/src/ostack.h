#ifndef ostack_INCLUDED
# define ostack_INCLUDED
#include "iostack.h"
#include "icstate.h"
#define iop_stack (i_ctx_p->op_stack)
#define o_stack (iop_stack.stack)
#define osbot (o_stack.bot)
#define osp (o_stack.p)
#define ostop (o_stack.top)
#define check_ostack(n)\
if ( ostop - osp < (n) )\
{ o_stack.requested = (n); return_error(e_stackoverflow); }
#define push(n)\
BEGIN\
if ( (op += (n)) > ostop )\
{ o_stack.requested = (n); return_error(e_stackoverflow); }\
else osp = op;\
END
#define pop(n) (osp -= (n))
#define check_op(nargs)\
if ( op < osbot + ((nargs) - 1) ) return_error(e_stackunderflow)
#endif