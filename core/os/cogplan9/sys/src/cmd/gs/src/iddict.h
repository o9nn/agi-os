#ifndef iddict_INCLUDED
# define iddict_INCLUDED
#include "idict.h"
#include "icstate.h"
#define idict_stack (i_ctx_p->dict_stack)
#define idict_put(pdref, key, pvalue)\
dict_put(pdref, key, pvalue, &idict_stack)
#define idict_put_string(pdref, kstr, pvalue)\
dict_put_string(pdref, kstr, pvalue, &idict_stack)
#define idict_undef(pdref, key)\
dict_undef(pdref, key, &idict_stack)
#define idict_copy(dfrom, dto)\
dict_copy(dfrom, dto, &idict_stack)
#define idict_copy_new(dfrom, dto)\
dict_copy_new(dfrom, dto, &idict_stack)
#define idict_resize(pdref, newmax)\
dict_resize(pdref, newmax, &idict_stack)
#define idict_grow(pdref)\
dict_grow(pdref, &idict_stack)
#define idict_unpack(pdref)\
dict_unpack(pdref, &idict_stack)
#endif