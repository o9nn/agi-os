#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gxcspace.h"
#include "estack.h"
#include "ialloc.h"
#include "idict.h"
#include "idparam.h"
#include "ifunc.h"
#include "ostack.h"
#include "store.h"
#include "gsfunc0.h"
#define MAX_DATA_SIZE 0x10000
#define MAX_NUM_INPUTS 16
#define MAX_NUM_OUTPUTS 128
struct gs_sampled_data_enum_s {
int indexes[MAX_NUM_INPUTS];
int o_stack_depth;
gs_function_t * pfn;
};
typedef struct gs_sampled_data_enum_s gs_sampled_data_enum;
gs_private_st_ptrs1(st_gs_sampled_data_enum, gs_sampled_data_enum,
"gs_sampled_data_enum", gs_sampled_data_enum_enum_ptrs,
gs_sampled_data_enum_reloc_ptrs, pfn);
private int cube_build_func0(const ref * pdict,
gs_function_Sd_params_t * params, gs_memory_t *mem);
private int sampled_data_setup(i_ctx_t *i_ctx_p, gs_function_t *pfn,
const ref * pproc, int (*finish_proc)(i_ctx_t *),
gs_memory_t * mem);
private int sampled_data_sample(i_ctx_t *i_ctx_p);
private int sampled_data_continue(i_ctx_t *i_ctx_p);
private int sampled_data_finish(i_ctx_t *i_ctx_p);
private gs_sampled_data_enum * gs_sampled_data_enum_alloc
(gs_memory_t * mem, client_name_t cname);
private int
zbuildsampledfunction(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
const ref * pdict = op;
ref * pfunc;
int code = 0;
gs_function_t *pfn;
gs_function_Sd_params_t params = {0};
check_type(*pdict, t_dictionary);
if (dict_find_string(pdict, "Function", &pfunc) <= 0)
return_error(e_rangecheck);
check_proc(*pfunc);
code = cube_build_func0(pdict, &params, imemory);
if (code < 0)
return code;
code = gs_function_Sd_init(&pfn, &params, imemory);
if (code < 0)
return code;
return sampled_data_setup(i_ctx_p, pfn, pfunc, sampled_data_finish, imemory);
}
#define bits2bytes(x) ((x) >> 3)
private bool
valid_cube_size(int num_inputs, int num_outputs, int sample_size, const int Size[])
{
int i, total_size = num_outputs * sample_size;
for (i = 0; i < num_inputs; i++) {
if (Size[i] <= 0 || Size[i] > MAX_DATA_SIZE / total_size)
return false;
total_size *= Size[i];
}
return true;
}
private int
determine_sampled_data_size(int num_inputs, int num_outputs,
int sample_size, int Size[])
{
static const int size_list[] = {512, 50, 20, 10, 7, 5, 4, 3};
int i, size;
if (num_inputs > 0 && num_inputs <= 8)
size = size_list[num_inputs - 1];
else
size = 2;
while (true) {
for (i = 0; i < num_inputs; i++)
Size[i] = size;
if (valid_cube_size(num_inputs, num_outputs, sample_size, Size))
return 0;
if (size == 2)
return_error(e_rangecheck);
size--;
}
}
private gs_sampled_data_enum *
gs_sampled_data_enum_alloc(gs_memory_t * mem, client_name_t cname)
{
return gs_alloc_struct(mem, gs_sampled_data_enum,
&st_gs_sampled_data_enum, cname);
}
private byte *
cube_ptr_from_index(gs_function_Sd_params_t * params, int indexes[])
{
int i, sum = indexes[params->m - 1];
for (i = params->m - 2; i >= 0; i--) {
sum *= params->Size[i];
sum += indexes[i];
}
return (byte *)(params->DataSource.data.str.data) +
sum * params->n * bits2bytes(params->BitsPerSample);
}
private bool
increment_cube_indexes(gs_function_Sd_params_t * params, int indexes[])
{
int i = 0;
while (true) {
indexes[i]++;
if (indexes[i] < params->Size[i])
return false;
indexes[i] = 0;
i++;
if (i == params->m)
return true;
}
}
private int
cube_build_func0(const ref * pdict, gs_function_Sd_params_t * params,
gs_memory_t *mem)
{
byte * bytes = 0;
int code, i;
int total_size;
if ((code = dict_int_param(pdict, "Order", 1, 3, 1, &params->Order)) < 0 ||
(code = dict_int_param(pdict, "BitsPerSample", 1, 32, 0,
&params->BitsPerSample)) < 0 ||
((code = params->m =
fn_build_float_array(pdict, "Domain", false, true,
&params->Domain, mem)) < 0 ) ||
((code = params->n =
fn_build_float_array(pdict, "Range", false, true,
&params->Range, mem)) < 0)
) {
goto fail;
}
params->m >>= 1;
params->n >>= 1;
if (params->m == 0 || params->n == 0 ||
params->m > MAX_NUM_INPUTS || params->n > MAX_NUM_OUTPUTS) {
code = gs_note_error(e_rangecheck);
goto fail;
}
{
int *ptr = (int *)
gs_alloc_byte_array(mem, params->m, sizeof(int), "Size");
if (ptr == NULL) {
code = gs_note_error(e_VMerror);
goto fail;
}
params->Size = ptr;
code = dict_ints_param(pdict, "Size", params->m, ptr);
if (code < 0)
goto fail;
if (code == 0) {
code = determine_sampled_data_size(params->m, params->n,
params->BitsPerSample, (int *)params->Size);
if (code < 0)
goto fail;
}
else {
if (code != params->m || !valid_cube_size(params->m, params->n,
params->BitsPerSample, params->Size))
code = gs_note_error(e_rangecheck);
goto fail;
}
}
total_size = params->n * bits2bytes(params->BitsPerSample);
for (i = 0; i < params->m; i++)
total_size *= params->Size[i];
bytes = gs_alloc_byte_array(mem, total_size, 1, "cube_build_func0(bytes)");
if (!bytes) {
code = gs_note_error(e_VMerror);
goto fail;
}
data_source_init_bytes(&params->DataSource,
(const unsigned char *)bytes, total_size);
return 0;
fail:
gs_function_Sd_free_params(params, mem);
return (code < 0 ? code : gs_note_error(e_rangecheck));
}
#define estack_storage 3
#define esp_finish_proc (*real_opproc(esp - 2))
#define sample_proc esp[-1]
#define senum r_ptr(esp, gs_sampled_data_enum)
#define O_STACK_PAD 3
private int
sampled_data_setup(i_ctx_t *i_ctx_p, gs_function_t *pfn,
const ref * pproc, int (*finish_proc)(i_ctx_t *), gs_memory_t * mem)
{
os_ptr op = osp;
gs_sampled_data_enum *penum;
int i;
gs_function_Sd_params_t * params = (gs_function_Sd_params_t *)&pfn->params;
check_estack(estack_storage + 1);
check_ostack(params->m + O_STACK_PAD);
check_ostack(params->n + O_STACK_PAD);
penum = gs_sampled_data_enum_alloc(imemory, "zbuildsampledfuntion(params)");
if (penum == NULL)
return_error(e_VMerror);
penum->pfn = pfn;
for(i=0; i< params->m; i++)
penum->indexes[i] = 0;
penum->o_stack_depth = ref_stack_count(&o_stack);
push(O_STACK_PAD);
for (i = 0; i < O_STACK_PAD; i++)
make_null(op - i);
esp += estack_storage;
make_op_estack(esp - 2, finish_proc);
sample_proc = *pproc;
make_istruct(esp, 0, penum);
push_op_estack(sampled_data_sample);
return o_push_estack;
}
private int
sampled_data_sample(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_sampled_data_enum *penum = senum;
ref proc;
gs_function_Sd_params_t * params =
(gs_function_Sd_params_t *)&penum->pfn->params;
int num_inputs = params->m;
int i;
push(num_inputs);
for (i = 0; i < num_inputs; i++) {
double dmin = params->Domain[2 * i];
double dmax = params->Domain[2 * i + 1];
make_real(op - num_inputs + i + 1, (float) (
penum->indexes[i] * (dmax - dmin)/(params->Size[i] - 1) + dmin));
}
proc = sample_proc;
push_op_estack(sampled_data_continue);
*++esp = proc;
return o_push_estack;
}
private int
sampled_data_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_sampled_data_enum *penum = senum;
gs_function_Sd_params_t * params =
(gs_function_Sd_params_t *)&penum->pfn->params;
int i, j, num_out = params->n;
int code = 0;
byte * data_ptr;
double sampled_data_value_max = (double)((1 << params->BitsPerSample) - 1);
int bps = bits2bytes(params->BitsPerSample);
if (num_out + O_STACK_PAD + penum->o_stack_depth != ref_stack_count(&o_stack)) {
int stack_depth_adjust = ref_stack_count(&o_stack) - penum->o_stack_depth;
if (stack_depth_adjust >= 0)
pop(stack_depth_adjust);
else {
push(-stack_depth_adjust);
}
ifree_object(penum->pfn, "sampled_data_continue(pfn)");
ifree_object(penum, "sampled_data_continue((enum)");
return_error(e_undefinedresult);
}
data_ptr = cube_ptr_from_index(params, penum->indexes);
for (i=0; i < num_out; i++) {
ulong cv;
double value;
double rmin = params->Range[2 * i];
double rmax = params->Range[2 * i + 1];
code = real_param(op + i - num_out + 1, &value);
if (code < 0)
return code;
if (value < rmin)
value = rmin;
else if (value > rmax)
value = rmax;
value = (value - rmin) / (rmax - rmin);
cv = (int) (value * sampled_data_value_max + 0.5);
for (j = 0; j < bps; j++)
data_ptr[bps * i + j] = (byte)(cv >> ((bps - 1 - j) * 8));
}
pop(num_out);
if (increment_cube_indexes(params, penum->indexes)) {
pop(O_STACK_PAD);
code = 0;
if (esp_finish_proc != 0)
code = esp_finish_proc(i_ctx_p);
return code;
}
return sampled_data_sample(i_ctx_p);
}
private int
sampled_data_finish(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_sampled_data_enum *penum = senum;
gs_function_Sd_params_t * params =
(gs_function_Sd_params_t *)&penum->pfn->params;
gs_function_t * pfn;
ref cref;
int code = gs_function_Sd_init(&pfn, params, imemory);
if (code < 0)
return code;
code = ialloc_ref_array(&cref, a_executable | a_execute, 2,
"sampled_data_finish(cref)");
if (code < 0)
return code;
make_istruct_new(cref.value.refs, a_executable | a_execute, pfn);
make_oper_new(cref.value.refs + 1, 0, zexecfunction);
ref_assign(op, &cref);
esp -= estack_storage;
ifree_object(penum->pfn, "sampled_data_finish(pfn)");
ifree_object(penum, "sampled_data_finish(enum)");
return o_pop_estack;
}
const op_def zfsample_op_defs[] =
{
op_def_begin_level2(),
{"1.buildsampledfunction", zbuildsampledfunction},
op_def_end(0)
};