#include "memory_.h"
#include "stdio_.h"
#include "string_.h"
#include "ghost.h"
#include "gp.h"
#include "gpcheck.h"
#include "oper.h"
#include "stream.h"
#include "istream.h"
#include "ialloc.h"
#include "iscan.h"
#include "ivmspace.h"
#include "gxiodev.h"
#include "files.h"
#include "scanchar.h"
#include "store.h"
#include "ierrors.h"
extern const char iodev_dtype_stdio[];
#define iodev_special(dname, init, open) {\
dname, iodev_dtype_stdio,\
{ init, open, iodev_no_open_file, iodev_no_fopen, iodev_no_fclose,\
iodev_no_delete_file, iodev_no_rename_file, iodev_no_file_status,\
iodev_no_enumerate_files, NULL, NULL,\
iodev_no_get_params, iodev_no_put_params\
}\
}
#define LINEEDIT_BUF_SIZE 20
const gx_io_device gs_iodev_lineedit =
iodev_special("%lineedit%", iodev_no_init, iodev_no_open_device);
#define STATEMENTEDIT_BUF_SIZE 50
const gx_io_device gs_iodev_statementedit =
iodev_special("%statementedit%", iodev_no_init, iodev_no_open_device);
private int
zgetiodevice(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gx_io_device *iodev;
const byte *dname;
check_type(*op, t_integer);
if (op->value.intval != (int)op->value.intval)
return_error(e_rangecheck);
iodev = gs_getiodevice((int)(op->value.intval));
if (iodev == 0)
return_error(e_rangecheck);
dname = (const byte *)iodev->dname;
if (dname == 0)
make_null(op);
else
make_const_string(op, a_readonly | avm_foreign,
strlen((const char *)dname), dname);
return 0;
}
int
zfilelineedit(i_ctx_t *i_ctx_p)
{
uint count = 0;
bool in_eol = false;
int code;
os_ptr op = osp;
bool statement;
stream *s;
stream *ins;
gs_string str;
uint initial_buf_size;
const char *filename;
gs_string *const buf = &str;
check_type(*op, t_string);
buf->data = op->value.bytes;
buf->size = op->tas.rsize;
check_type(*(op-1), t_integer);
count = (op-1)->value.intval;
check_type(*(op-2), t_boolean);
statement = (op-2)->value.boolval;
check_read_file(ins, op - 3);
initial_buf_size = statement ? STATEMENTEDIT_BUF_SIZE : LINEEDIT_BUF_SIZE;
if (initial_buf_size > max_string_size)
return_error(e_limitcheck);
if (!buf->data || (buf->size < initial_buf_size)) {
count = 0;
buf->data = gs_alloc_string(imemory, initial_buf_size,
"zfilelineedit(buffer)");
if (buf->data == 0)
return_error(e_VMerror);
op->value.bytes = buf->data;
op->tas.rsize = buf->size = initial_buf_size;
}
rd:
code = zreadline_from(ins, buf, imemory, &count, &in_eol);
if (buf->size > max_string_size) {
byte *nbuf = gs_resize_string(imemory, buf->data, buf->size,
max_string_size, "zfilelineedit(shrink buffer)");
if (nbuf == 0)
return_error(e_VMerror);
op->value.bytes = buf->data = nbuf;
op->tas.rsize = buf->size = max_string_size;
return_error(e_limitcheck);
}
op->value.bytes = buf->data;
op->tas.rsize = buf->size;
switch (code) {
case EOFC:
code = gs_note_error(e_undefinedfilename);
case 0:
break;
default:
code = gs_note_error(e_ioerror);
break;
case CALLC:
{
ref rfile;
(op-1)->value.intval = count;
make_file(&rfile, a_readonly | avm_system, ins->read_id, ins);
code = s_handle_read_exception(i_ctx_p, code, &rfile,
NULL, 0, zfilelineedit);
}
break;
case 1:
{
uint nsize = buf->size;
byte *nbuf;
if (nsize >= max_string_size) {
code = gs_note_error(e_limitcheck);
break;
}
else if (nsize >= max_string_size / 2)
nsize= max_string_size;
else
nsize = buf->size * 2;
nbuf = gs_resize_string(imemory, buf->data, buf->size, nsize,
"zfilelineedit(grow buffer)");
if (nbuf == 0) {
code = gs_note_error(e_VMerror);
break;
}
op->value.bytes = buf->data = nbuf;
op->tas.rsize = buf->size = nsize;
goto rd;
}
}
if (code != 0)
return code;
if (statement) {
stream st;
stream *ts = &st;
scanner_state state;
ref ignore_value;
uint depth = ref_stack_count(&o_stack);
int code;
if (count + 1 > buf->size) {
uint nsize;
byte *nbuf;
nsize = buf->size + 1;
if (nsize > max_string_size) {
return_error(gs_note_error(e_limitcheck));
}
else {
nbuf = gs_resize_string(imemory, buf->data, buf->size, nsize,
"zfilelineedit(grow buffer)");
if (nbuf == 0) {
code = gs_note_error(e_VMerror);
return_error(code);
}
op->value.bytes = buf->data = nbuf;
op->tas.rsize = buf->size = nsize;
}
}
buf->data[count++] = char_EOL;
s_init(ts, NULL);
sread_string(ts, buf->data, count);
sc:
scanner_state_init_check(&state, false, true);
code = scan_token(i_ctx_p, ts, &ignore_value, &state);
ref_stack_pop_to(&o_stack, depth);
if (code < 0)
code = scan_EOF;
switch (code) {
case 0:
case scan_BOS:
goto sc;
case scan_Refill:
goto rd;
case scan_EOF:
break;
default:
return code;
}
}
buf->data = gs_resize_string(imemory, buf->data, buf->size, count,
"zfilelineedit(resize buffer)");
if (buf->data == 0)
return_error(e_VMerror);
op->value.bytes = buf->data;
op->tas.rsize = buf->size;
s = file_alloc_stream(imemory, "zfilelineedit(stream)");
if (s == 0)
return_error(e_VMerror);
sread_string(s, buf->data, count);
s->save_close = s->procs.close;
s->procs.close = file_close_disable;
filename = statement ? gs_iodev_statementedit.dname
: gs_iodev_lineedit.dname;
code = ssetfilename(s, (const byte *)filename, strlen(filename)+1);
if (code < 0) {
sclose(s);
return_error(e_VMerror);
}
pop(3);
make_stream_file(osp, s, "r");
return code;
}
const op_def ziodev_op_defs[] =
{
{"1.getiodevice", zgetiodevice},
op_def_end(0)
};