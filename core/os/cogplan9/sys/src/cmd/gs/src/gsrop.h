#ifndef gsrop_INCLUDED
# define gsrop_INCLUDED
#include "gsropt.h"
int gs_setrasterop(gs_state *, gs_rop3_t);
gs_rop3_t gs_currentrasterop(const gs_state *);
int gs_setsourcetransparent(gs_state *, bool);
bool gs_currentsourcetransparent(const gs_state *);
int gs_settexturetransparent(gs_state *, bool);
bool gs_currenttexturetransparent(const gs_state *);
gs_logical_operation_t gs_current_logical_op(const gs_state *);
int gs_set_logical_op(gs_state *, gs_logical_operation_t);
#endif