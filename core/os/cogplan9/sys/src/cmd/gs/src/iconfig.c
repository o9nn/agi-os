#include "stdio_.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gconfigd.h"
#include "iref.h"
#include "ivmspace.h"
#include "opdef.h"
#include "ifunc.h"
#include "iapi.h"
#include "iminst.h"
#include "iplugin.h"
const gs_main_instance gs_main_instance_init_values =
{gs_main_instance_default_init_values};
#define ref_(t) struct { struct tas_s tas; t value; }
#define string_(s,len)\
{ { (t_string<<r_type_shift) + a_readonly + avm_foreign, len }, s },
#define psfile_(fns,len) string_(fns,len)
const ref_(const char *) gs_init_file_array[] = {
#include "gconf.h"
string_(0, 0)
};
#undef psfile_
#define emulator_(ems,len) string_(ems,len)
const ref_(const char *) gs_emulator_name_array[] = {
#include "gconf.h"
string_(0, 0)
};
#undef emulator_
#define function_type_(i,proc) extern build_function_proc(proc);
#include "gconf.h"
#undef function_type_
#define function_type_(i,proc) {i,proc},
const build_function_type_t build_function_type_table[] = {
#include "gconf.h"
{0}
};
#undef function_type_
const uint build_function_type_table_count =
countof(build_function_type_table) - 1;
#define oper_(xx_op_defs) extern const op_def xx_op_defs[];
oper_(interp_op_defs)
#include "gconf.h"
#undef oper_
const op_def *const op_defs_all[] = {
#define oper_(defs) defs,
oper_(interp_op_defs)
#include "gconf.h"
#undef oper_
0
};
const uint op_def_count = (countof(op_defs_all) - 1) * OP_DEFS_MAX_SIZE;
#define plugin_(proc) extern plugin_instantiation_proc(proc);
#include "gconf.h"
#undef plugin_
extern_i_plugin_table();
#define plugin_(proc) proc,
const i_plugin_instantiation_proc i_plugin_table[] = {
#include "gconf.h"
0
};
#undef plugin_