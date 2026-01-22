#include "memory_.h"
#include "ghost.h"
#include "gp.h"
#include "oper.h"
#include "stream.h"
#include "files.h"
#include "store.h"
#include "strimpl.h"
#include "ifilter.h"
#include "interp.h"
#include "gsmatrix.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "estack.h"
private int write_string(ref *, stream *);
private int handle_read_status(i_ctx_t *, int, const ref *, const uint *,
op_proc_t);
private int handle_write_status(i_ctx_t *, int, const ref *, const uint *,
op_proc_t);
int
zclosefile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
check_type(*op, t_file);
if (file_is_valid(s, op)) {
int status = sclose(s);
if (status != 0 && status != EOFC) {
if (s_is_writing(s))
return handle_write_status(i_ctx_p, status, op, NULL,
zclosefile);
else
return handle_read_status(i_ctx_p, status, op, NULL,
zclosefile);
}
}
pop(1);
return 0;
}
private int
zread(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
int ch;
check_read_file(s, op);
push(1);
ch = sgetc(s);
if (ch >= 0) {
make_int(op - 1, ch);
make_bool(op, 1);
} else {
pop(1);
op--;
if (ch == EOFC)
make_bool(op, 0);
else
return handle_read_status(i_ctx_p, ch, op, NULL, zread);
}
return 0;
}
int
zwrite(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
byte ch;
int status;
check_write_file(s, op - 1);
check_type(*op, t_integer);
ch = (byte) op->value.intval;
status = sputc(s, (byte) ch);
if (status >= 0) {
pop(2);
return 0;
}
return handle_write_status(i_ctx_p, status, op - 1, NULL, zwrite);
}
private int zreadhexstring_continue(i_ctx_t *);
private int
zreadhexstring_at(i_ctx_t *i_ctx_p, os_ptr op, uint start)
{
stream *s;
uint len, nread;
byte *str;
int odd;
stream_cursor_write cw;
int status;
check_read_file(s, op - 1);
str = op->value.bytes;
len = r_size(op);
if (start < len) {
odd = str[start];
if (odd > 0xf)
odd = -1;
} else
odd = -1;
cw.ptr = str + start - 1;
cw.limit = str + len - 1;
for (;;) {
status = s_hex_process(&s->cursor.r, &cw, &odd,
hex_ignore_garbage);
if (status == 1) {
ref_assign_inline(op - 1, op);
make_true(op);
return 0;
} else if (status != 0)
break;
status = spgetc(s);
if (status < 0)
break;
sputback(s);
}
nread = cw.ptr + 1 - str;
if (status != EOFC) {
if (nread < len)
str[nread] = (odd < 0 ? 0x10 : odd);
return handle_read_status(i_ctx_p, status, op - 1, &nread,
zreadhexstring_continue);
}
ref_assign_inline(op - 1, op);
r_set_size(op - 1, nread);
make_false(op);
return 0;
}
private int
zreadhexstring(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_write_type(*op, t_string);
if (r_size(op) > 0)
*op->value.bytes = 0x10;
return zreadhexstring_at(i_ctx_p, op, 0);
}
private int
zreadhexstring_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code;
check_type(*op, t_integer);
if (op->value.intval < 0 || op->value.intval > r_size(op - 1))
return_error(e_rangecheck);
check_write_type(op[-1], t_string);
code = zreadhexstring_at(i_ctx_p, op - 1, (uint) op->value.intval);
if (code >= 0)
pop(1);
return code;
}
private int zwritehexstring_continue(i_ctx_t *);
private int
zwritehexstring_at(i_ctx_t *i_ctx_p, os_ptr op, uint odd)
{
register stream *s;
register byte ch;
register const byte *p;
register const char *const hex_digits = "0123456789abcdef";
register uint len;
int status;
#define MAX_HEX 128
byte buf[MAX_HEX];
check_write_file(s, op - 1);
check_read_type(*op, t_string);
p = op->value.bytes;
len = r_size(op);
while (len) {
uint len1 = min(len, MAX_HEX / 2);
register byte *q = buf;
uint count = len1;
ref rbuf;
do {
ch = *p++;
*q++ = hex_digits[ch >> 4];
*q++ = hex_digits[ch & 0xf];
}
while (--count);
r_set_size(&rbuf, (len1 << 1) - odd);
rbuf.value.bytes = buf + odd;
status = write_string(&rbuf, s);
switch (status) {
default:
return_error(e_ioerror);
case 0:
len -= len1;
odd = 0;
continue;
case INTC:
case CALLC:
count = rbuf.value.bytes - buf;
op->value.bytes += count >> 1;
r_set_size(op, len - (count >> 1));
count &= 1;
return handle_write_status(i_ctx_p, status, op - 1, &count,
zwritehexstring_continue);
}
}
pop(2);
return 0;
#undef MAX_HEX
}
private int
zwritehexstring(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
return zwritehexstring_at(i_ctx_p, op, 0);
}
private int
zwritehexstring_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code;
check_type(*op, t_integer);
if ((op->value.intval & ~1) != 0)
return_error(e_rangecheck);
code = zwritehexstring_at(i_ctx_p, op - 1, (uint) op->value.intval);
if (code >= 0)
pop(1);
return code;
}
private int zreadstring_continue(i_ctx_t *);
private int
zreadstring_at(i_ctx_t *i_ctx_p, os_ptr op, uint start)
{
stream *s;
uint len, rlen;
int status;
check_read_file(s, op - 1);
check_write_type(*op, t_string);
len = r_size(op);
status = sgets(s, op->value.bytes + start, len - start, &rlen);
rlen += start;
switch (status) {
case EOFC:
case 0:
break;
default:
return handle_read_status(i_ctx_p, status, op - 1, &rlen,
zreadstring_continue);
}
if (len == 0)
return_error(e_rangecheck);
r_set_size(op, rlen);
op[-1] = *op;
make_bool(op, (rlen == len ? 1 : 0));
return 0;
}
private int
zreadstring(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
return zreadstring_at(i_ctx_p, op, 0);
}
private int
zreadstring_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code;
check_type(*op, t_integer);
if (op->value.intval < 0 || op->value.intval > r_size(op - 1))
return_error(e_rangecheck);
code = zreadstring_at(i_ctx_p, op - 1, (uint) op->value.intval);
if (code >= 0)
pop(1);
return code;
}
private int
zwritestring(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
int status;
check_write_file(s, op - 1);
check_read_type(*op, t_string);
status = write_string(op, s);
if (status >= 0) {
pop(2);
return 0;
}
return handle_write_status(i_ctx_p, status, op - 1, NULL, zwritestring);
}
private int zreadline(i_ctx_t *);
private int zreadline_continue(i_ctx_t *);
private int
zreadline_at(i_ctx_t *i_ctx_p, os_ptr op, uint count, bool in_eol)
{
stream *s;
int status;
gs_string str;
check_read_file(s, op - 1);
check_write_type(*op, t_string);
str.data = op->value.bytes;
str.size = r_size(op);
status = zreadline_from(s, &str, NULL, &count, &in_eol);
switch (status) {
case 0:
case EOFC:
break;
case 1:
return_error(e_rangecheck);
default:
if (count == 0 && !in_eol)
return handle_read_status(i_ctx_p, status, op - 1, NULL,
zreadline);
else {
if (in_eol) {
r_set_size(op, count);
count = 0;
}
return handle_read_status(i_ctx_p, status, op - 1, &count,
zreadline_continue);
}
}
r_set_size(op, count);
op[-1] = *op;
make_bool(op, status == 0);
return 0;
}
private int
zreadline(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
return zreadline_at(i_ctx_p, op, 0, false);
}
private int
zreadline_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint size = r_size(op - 1);
uint start;
int code;
check_type(*op, t_integer);
if (op->value.intval < 0 || op->value.intval > size)
return_error(e_rangecheck);
start = (uint) op->value.intval;
code = (start == 0 ? zreadline_at(i_ctx_p, op - 1, size, true) :
zreadline_at(i_ctx_p, op - 1, start, false));
if (code >= 0)
pop(1);
return code;
}
int
zreadline_from(stream *s, gs_string *buf, gs_memory_t *bufmem,
uint *pcount, bool *pin_eol)
{
sreadline_proc((*readline));
if (zis_stdin(s))
readline = gp_readline;
else
readline = sreadline;
return readline(s, NULL, NULL , NULL, buf, bufmem,
pcount, pin_eol, zis_stdin);
}
private int
zbytesavailable(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
long avail;
check_read_file(s, op);
switch (savailable(s, &avail)) {
default:
return_error(e_ioerror);
case EOFC:
avail = -1;
case 0:
;
}
make_int(op, avail);
return 0;
}
int
zflush(i_ctx_t *i_ctx_p)
{
stream *s;
int status;
ref rstdout;
int code = zget_stdout(i_ctx_p, &s);
if (code < 0)
return code;
make_stream_file(&rstdout, s, "w");
status = sflush(s);
if (status == 0 || status == EOFC) {
return 0;
}
return
(s_is_writing(s) ?
handle_write_status(i_ctx_p, status, &rstdout, NULL, zflush) :
handle_read_status(i_ctx_p, status, &rstdout, NULL, zflush));
}
private int
zflushfile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
int status;
check_type(*op, t_file);
if (file_is_invalid(s, op)) {
if (r_has_attr(op, a_write))
return_error(e_invalidaccess);
pop(1);
return 0;
}
status = sflush(s);
if (status == 0 || status == EOFC) {
pop(1);
return 0;
}
return
(s_is_writing(s) ?
handle_write_status(i_ctx_p, status, op, NULL, zflushfile) :
handle_read_status(i_ctx_p, status, op, NULL, zflushfile));
}
private int
zresetfile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
check_type(*op, t_file);
if (file_is_valid(s, op))
sreset(s);
pop(1);
return 0;
}
private int
zprint(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
int status;
ref rstdout;
int code;
check_read_type(*op, t_string);
code = zget_stdout(i_ctx_p, &s);
if (code < 0)
return code;
status = write_string(op, s);
if (status >= 0) {
pop(1);
return 0;
}
make_stream_file(&rstdout, s, "w");
code = handle_write_status(i_ctx_p, status, &rstdout, NULL,
zwritestring);
if (code != o_push_estack)
return code;
push(1);
*op = op[-1];
op[-1] = rstdout;
return code;
}
private int
zecho(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_boolean);
pop(1);
return 0;
}
private int
zfileposition(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
check_file(s, op);
if (!s_can_seek(s))
return_error(e_ioerror);
make_int(op, stell(s));
return 0;
}
private int
zxfileposition(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
check_file(s, op);
make_int(op, stell(s));
return 0;
}
private int
zsetfileposition(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
check_file(s, op - 1);
check_type(*op, t_integer);
if (sseek(s, op->value.intval) < 0)
return_error(e_ioerror);
pop(2);
return 0;
}
private int
zfilename(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
gs_const_string fname;
byte *str;
check_file(s, op);
if (sfilename(s, &fname) < 0) {
make_false(op);
return 0;
}
check_ostack(1);
str = ialloc_string(fname.size, "filename");
if (str == 0)
return_error(e_VMerror);
memcpy(str, fname.data, fname.size);
push(1);
make_const_string( op - 1 ,
a_all | imemory_space((const struct gs_ref_memory_s*) imemory),
fname.size,
str);
make_true(op);
return 0;
}
private int
zisprocfilter(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
check_file(s, op);
while (s->strm != 0)
s = s->strm;
make_bool(op, s_is_proc(s));
return 0;
}
private int
zpeekstring(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
uint len, rlen;
check_read_file(s, op - 1);
check_write_type(*op, t_string);
len = r_size(op);
while ((rlen = sbufavailable(s)) < len) {
int status = s->end_status;
switch (status) {
case EOFC:
break;
case 0:
if (len >= s->bsize)
return_error(e_rangecheck);
s_process_read_buf(s);
continue;
default:
return handle_read_status(i_ctx_p, status, op - 1, NULL,
zpeekstring);
}
break;
}
if (rlen > len)
rlen = len;
memcpy(op->value.bytes, sbufptr(s), rlen);
r_set_size(op, rlen);
op[-1] = *op;
make_bool(op, (rlen == len ? 1 : 0));
return 0;
}
private int
zunread(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
ulong ch;
check_read_file(s, op - 1);
check_type(*op, t_integer);
ch = op->value.intval;
if (ch > 0xff)
return_error(e_rangecheck);
if (sungetc(s, (byte) ch) < 0)
return_error(e_ioerror);
pop(2);
return 0;
}
private int zwritecvp_continue(i_ctx_t *);
private int
zwritecvp_at(i_ctx_t *i_ctx_p, os_ptr op, uint start, bool first)
{
stream *s;
byte str[100];
ref rstr;
const byte *data = str;
uint len;
int code, status;
check_write_file(s, op - 2);
check_type(*op, t_integer);
code = obj_cvp(op - 1, str, sizeof(str), &len, (int)op->value.intval,
start, imemory);
if (code == e_rangecheck) {
code = obj_string_data(imemory, op - 1, &data, &len);
if (len < start)
return_error(e_rangecheck);
data += start;
len -= start;
}
if (code < 0)
return code;
r_set_size(&rstr, len);
rstr.value.const_bytes = data;
status = write_string(&rstr, s);
switch (status) {
default:
return_error(e_ioerror);
case 0:
break;
case INTC:
case CALLC:
len = start + len - r_size(&rstr);
if (!first)
--osp;
return handle_write_status(i_ctx_p, status, op - 2, &len,
zwritecvp_continue);
}
if (code == 1) {
if (first)
check_ostack(1);
push_op_estack(zwritecvp_continue);
if (first)
push(1);
make_int(osp, start + len);
return o_push_estack;
}
if (first)
pop(3);
else
pop(4);
return 0;
}
private int
zwritecvp(i_ctx_t *i_ctx_p)
{
return zwritecvp_at(i_ctx_p, osp, 0, true);
}
private int
zwritecvp_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
if (op->value.intval != (uint) op->value.intval)
return_error(e_rangecheck);
return zwritecvp_at(i_ctx_p, op - 1, (uint) op->value.intval, false);
}
int
zneedstdin(i_ctx_t *i_ctx_p)
{
return e_NeedStdin;
}
int
zneedstdout(i_ctx_t *i_ctx_p)
{
return e_NeedStdout;
}
int
zneedstderr(i_ctx_t *i_ctx_p)
{
return e_NeedStderr;
}
const op_def zfileio1_op_defs[] = {
{"1bytesavailable", zbytesavailable},
{"1closefile", zclosefile},
{"1echo", zecho},
{"1.filename", zfilename},
{"1.fileposition", zxfileposition},
{"1fileposition", zfileposition},
{"0flush", zflush},
{"1flushfile", zflushfile},
{"1.isprocfilter", zisprocfilter},
{"2.peekstring", zpeekstring},
{"1print", zprint},
{"1read", zread},
{"2readhexstring", zreadhexstring},
{"2readline", zreadline},
{"2readstring", zreadstring},
op_def_end(0)
};
const op_def zfileio2_op_defs[] = {
{"1resetfile", zresetfile},
{"2setfileposition", zsetfileposition},
{"2.unread", zunread},
{"2write", zwrite},
{"3.writecvp", zwritecvp},
{"2writehexstring", zwritehexstring},
{"2writestring", zwritestring},
{"3%zreadhexstring_continue", zreadhexstring_continue},
{"3%zreadline_continue", zreadline_continue},
{"3%zreadstring_continue", zreadstring_continue},
{"4%zwritecvp_continue", zwritecvp_continue},
{"3%zwritehexstring_continue", zwritehexstring_continue},
{"0.needstdin", zneedstdin},
{"0.needstdout", zneedstdout},
{"0.needstderr", zneedstderr},
op_def_end(0)
};
int
file_switch_to_read(const ref * op)
{
stream *s = fptr(op);
if (s->write_id != r_size(op) || s->file == 0)
return_error(e_invalidaccess);
if (sswitch(s, false) < 0)
return_error(e_ioerror);
s->read_id = s->write_id;
s->write_id = 0;
return 0;
}
int
file_switch_to_write(const ref * op)
{
stream *s = fptr(op);
if (s->read_id != r_size(op) || s->file == 0)
return_error(e_invalidaccess);
if (sswitch(s, true) < 0)
return_error(e_ioerror);
s->write_id = s->read_id;
s->read_id = 0;
return 0;
}
private int
write_string(ref * op, stream * s)
{
const byte *data = op->value.const_bytes;
uint len = r_size(op);
uint wlen;
int status = sputs(s, data, len, &wlen);
switch (status) {
case INTC:
case CALLC:
op->value.const_bytes = data + wlen;
r_set_size(op, len - wlen);
default:
return status;
}
}
private int
copy_error_string(i_ctx_t *i_ctx_p, const ref *fop)
{
stream *s;
for (s = fptr(fop); s->strm != 0 && s->state->error_string[0] == 0;)
s = s->strm;
if (s->state->error_string[0]) {
int code = gs_errorinfo_put_string(i_ctx_p, s->state->error_string);
if (code < 0)
return code;
s->state->error_string[0] = 0;
}
return_error(e_ioerror);
}
private int
handle_read_status(i_ctx_t *i_ctx_p, int ch, const ref * fop,
const uint * pindex, op_proc_t cont)
{
switch (ch) {
default:
return copy_error_string(i_ctx_p, fop);
case EOFC:
return 1;
case INTC:
case CALLC:
if (pindex) {
ref index;
make_int(&index, *pindex);
return s_handle_read_exception(i_ctx_p, ch, fop, &index, 1,
cont);
} else
return s_handle_read_exception(i_ctx_p, ch, fop, NULL, 0,
cont);
}
}
private int
handle_write_status(i_ctx_t *i_ctx_p, int ch, const ref * fop,
const uint * pindex, op_proc_t cont)
{
switch (ch) {
default:
return copy_error_string(i_ctx_p, fop);
case EOFC:
return 1;
case INTC:
case CALLC:
if (pindex) {
ref index;
make_int(&index, *pindex);
return s_handle_write_exception(i_ctx_p, ch, fop, &index, 1,
cont);
} else
return s_handle_write_exception(i_ctx_p, ch, fop, NULL, 0,
cont);
}
}