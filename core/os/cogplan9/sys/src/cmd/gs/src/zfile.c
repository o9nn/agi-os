#include "memory_.h"
#include "string_.h"
#include "unistd_.h"
#include "ghost.h"
#include "gscdefs.h"
#include "gsutil.h"
#include "gp.h"
#include "gpmisc.h"
#include "gsfname.h"
#include "gsstruct.h"
#include "gxalloc.h"
#include "oper.h"
#include "dstack.h"
#include "estack.h"
#include "ialloc.h"
#include "ilevel.h"
#include "interp.h"
#include "iname.h"
#include "isave.h"
#include "idict.h"
#include "iutil.h"
#include "stream.h"
#include "strimpl.h"
#include "sfilter.h"
#include "gxiodev.h"
#include "files.h"
#include "main.h"
#include "store.h"
extern_gx_io_device_table();
extern const char iodev_dtype_stdio[];
private int parse_file_name(const ref * op, gs_parsed_file_name_t * pfn, bool safemode);
private int parse_real_file_name(const ref * op,
gs_parsed_file_name_t * pfn,
gs_memory_t *mem, client_name_t cname);
private int parse_file_access_string(const ref *op, char file_access[4]);
private int execfile_finish(i_ctx_t *);
private int execfile_cleanup(i_ctx_t *);
private int zopen_file(i_ctx_t *, const gs_parsed_file_name_t *pfn,
const char *file_access, stream **ps,
gs_memory_t *mem);
private iodev_proc_open_file(iodev_os_open_file);
private void file_init_stream(stream *s, FILE *file, const char *fmode,
byte *buffer, uint buffer_size);
stream_proc_report_error(filter_report_error);
#define DEFAULT_BUFFER_SIZE 2048
const uint file_default_buffer_size = DEFAULT_BUFFER_SIZE;
private stream invalid_file_stream;
stream *const invalid_file_entry = &invalid_file_stream;
private int
zfile_init(i_ctx_t *i_ctx_p)
{
stream *const s = &invalid_file_stream;
s_init(s, NULL);
sread_string(s, NULL, 0);
s->next = s->prev = 0;
s_init_no_id(s);
return 0;
}
void
make_invalid_file(ref * fp)
{
make_file(fp, avm_invalid_file_entry, ~0, invalid_file_entry);
}
private int
check_file_permissions_reduced(i_ctx_t *i_ctx_p, const char *fname, int len,
const char *permitgroup)
{
long i;
ref *permitlist = NULL;
const char *win_sep2 = "\\";
bool use_windows_pathsep = (gs_file_name_check_separator(win_sep2, 1, win_sep2) == 1);
uint plen = gp_file_name_parents(fname, len);
if (dict_find_string(&(i_ctx_p->userparams), permitgroup, &permitlist) <= 0)
return 0;
for (i=0; i<r_size(permitlist); i++) {
ref permitstring;
const string_match_params win_filename_params = {
'*', '?', '\\', true, true
};
const byte *permstr;
uint permlen;
int cwd_len = 0;
if (array_get(imemory, permitlist, i, &permitstring) < 0 ||
r_type(&permitstring) != t_string
)
break;
permstr = permitstring.value.bytes;
permlen = r_size(&permitstring);
if (permlen == 1 && permstr[0] == '*')
return 0;
if (plen != 0 && plen != gp_file_name_parents((const char *)permstr, permlen))
continue;
cwd_len = gp_file_name_cwds((const char *)permstr, permlen);
if (cwd_len > 0 && gp_file_name_is_absolute(fname, len))
continue;
if (string_match( (const unsigned char*) fname, len,
permstr + cwd_len, permlen - cwd_len,
use_windows_pathsep ? &win_filename_params : NULL))
return 0;
}
return e_invalidfileaccess;
}
private int
check_file_permissions(i_ctx_t *i_ctx_p, const char *fname, int len,
const char *permitgroup)
{
char fname_reduced[gp_file_name_sizeof];
uint rlen = sizeof(fname_reduced);
if (gp_file_name_reduce(fname, len, fname_reduced, &rlen) != gp_combine_success)
return e_invalidaccess;
return check_file_permissions_reduced(i_ctx_p, fname_reduced, rlen, permitgroup);
}
private int
zfile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
char file_access[4];
gs_parsed_file_name_t pname;
int code = parse_file_access_string(op, file_access);
stream *s;
if (code < 0)
return code;
code = parse_file_name(op - 1, &pname, i_ctx_p->LockFilePermissions);
if (code < 0)
return code;
if (pname.iodev && pname.iodev->dtype == iodev_dtype_stdio) {
bool statement = (strcmp(pname.iodev->dname, "%statementedit%") == 0);
bool lineedit = (strcmp(pname.iodev->dname, "%lineedit%") == 0);
if (pname.fname)
return_error(e_invalidfileaccess);
if (statement || lineedit) {
gx_io_device *indev = gs_findiodevice((const byte *)"%stdin", 6);
stream *ins;
if (strcmp(file_access, "r"))
return_error(e_invalidfileaccess);
indev->state = i_ctx_p;
code = (indev->procs.open_device)(indev, file_access, &ins, imemory);
indev->state = 0;
if (code < 0)
return code;
check_ostack(2);
push(2);
make_stream_file(op - 3, ins, file_access);
make_bool(op-2, statement);
make_int(op-1, 0);
make_string(op, icurrent_space, 0, NULL);
return zfilelineedit(i_ctx_p);
}
pname.iodev->state = i_ctx_p;
code = (*pname.iodev->procs.open_device)(pname.iodev,
file_access, &s, imemory);
pname.iodev->state = NULL;
} else {
if (pname.iodev == NULL)
pname.iodev = iodev_default;
code = zopen_file(i_ctx_p, &pname, file_access, &s, imemory);
}
if (code < 0)
return code;
code = ssetfilename(s, op[-1].value.const_bytes, r_size(op - 1));
if (code < 0) {
sclose(s);
return_error(e_VMerror);
}
make_stream_file(op - 1, s, file_access);
pop(1);
return code;
}
private bool
file_is_tempfile(i_ctx_t *i_ctx_p, const ref *op)
{
ref *SAFETY;
ref *tempfiles;
ref kname;
if (dict_find_string(systemdict, "SAFETY", &SAFETY) <= 0 ||
dict_find_string(SAFETY, "tempfiles", &tempfiles) <= 0)
return false;
if (name_ref(imemory, op->value.bytes, r_size(op), &kname, -1) < 0 ||
dict_find(tempfiles, &kname, &SAFETY) <= 0)
return false;
return true;
}
private int
zdeletefile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_parsed_file_name_t pname;
int code = parse_real_file_name(op, &pname, imemory, "deletefile");
if (code < 0)
return code;
if (pname.iodev == iodev_default) {
if ((code = check_file_permissions(i_ctx_p, pname.fname, pname.len,
"PermitFileControl")) < 0 &&
!file_is_tempfile(i_ctx_p, op)) {
return code;
}
}
code = (*pname.iodev->procs.delete_file)(pname.iodev, pname.fname);
gs_free_file_name(&pname, "deletefile");
if (code < 0)
return code;
pop(1);
return 0;
}
private int file_continue(i_ctx_t *);
private int file_cleanup(i_ctx_t *);
private int
zfilenameforall(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
file_enum *pfen;
gx_io_device *iodev = NULL;
gs_parsed_file_name_t pname;
int code = 0;
check_write_type(*op, t_string);
check_proc(op[-1]);
check_read_type(op[-2], t_string);
check_estack(7);
code = parse_file_name(op - 2, &pname, i_ctx_p->LockFilePermissions);
if (code < 0)
return code;
iodev = (pname.iodev == NULL) ? iodev_default : pname.iodev;
if (pname.len == 0 || iodev->procs.enumerate_files == iodev_no_enumerate_files) {
pop(3);
return 0;
}
pfen = iodev->procs.enumerate_files(iodev, (const char *)pname.fname,
pname.len, imemory);
if (pfen == 0)
return_error(e_VMerror);
push_mark_estack(es_for, file_cleanup);
++esp;
make_istruct(esp, 0, iodev);
++esp;
make_int(esp, r_size(op-2) - pname.len);
*++esp = *op;
++esp;
make_istruct(esp, 0, pfen);
*++esp = op[-1];
pop(3);
code = file_continue(i_ctx_p);
return (code == o_pop_estack ? o_push_estack : code);
}
private int
file_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr pscratch = esp - 2;
file_enum *pfen = r_ptr(esp - 1, file_enum);
long devlen = esp[-3].value.intval;
gx_io_device *iodev = r_ptr(esp - 4, gx_io_device);
uint len = r_size(pscratch);
uint code;
if (len < devlen)
return_error(e_rangecheck);
memcpy((char *)pscratch->value.bytes, iodev->dname, devlen);
code = iodev->procs.enumerate_next(pfen, (char *)pscratch->value.bytes + devlen,
len - devlen);
if (code == ~(uint) 0) {
esp -= 5;
return o_pop_estack;
} else if (code > len)
return_error(e_rangecheck);
else {
push(1);
ref_assign(op, pscratch);
r_set_size(op, code + devlen);
push_op_estack(file_continue);
*++esp = pscratch[2];
return o_push_estack;
}
}
private int
file_cleanup(i_ctx_t *i_ctx_p)
{
gx_io_device *iodev = r_ptr(esp + 2, gx_io_device);
iodev->procs.enumerate_close(r_ptr(esp + 5, file_enum));
return 0;
}
private int
zrenamefile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_parsed_file_name_t pname1, pname2;
int code = parse_real_file_name(op - 1, &pname1, imemory,
"renamefile(from)");
if (code < 0)
return code;
pname2.fname = 0;
code = parse_real_file_name(op, &pname2, imemory, "renamefile(to)");
if (code >= 0) {
if (pname1.iodev != pname2.iodev ) {
if (pname1.iodev == iodev_default)
pname1.iodev = pname2.iodev;
if (pname2.iodev == iodev_default)
pname2.iodev = pname1.iodev;
}
if (pname1.iodev != pname2.iodev ||
(pname1.iodev == iodev_default &&
((check_file_permissions(i_ctx_p, pname1.fname, pname1.len,
"PermitFileControl") < 0 &&
!file_is_tempfile(i_ctx_p, op - 1)) ||
(check_file_permissions(i_ctx_p, pname2.fname, pname2.len,
"PermitFileControl") < 0 ||
check_file_permissions(i_ctx_p, pname2.fname, pname2.len,
"PermitFileWriting") < 0 )))) {
code = gs_note_error(e_invalidfileaccess);
} else {
code = (*pname1.iodev->procs.rename_file)(pname1.iodev,
pname1.fname, pname2.fname);
}
}
gs_free_file_name(&pname2, "renamefile(to)");
gs_free_file_name(&pname1, "renamefile(from)");
if (code < 0)
return code;
pop(2);
return 0;
}
private int
zstatus(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
case t_file:
{
stream *s;
make_bool(op, (file_is_valid(s, op) ? 1 : 0));
}
return 0;
case t_string:
{
gs_parsed_file_name_t pname;
struct stat fstat;
int code = parse_file_name(op, &pname, i_ctx_p->LockFilePermissions);
if (code < 0)
return code;
code = gs_terminate_file_name(&pname, imemory, "status");
if (code < 0)
return code;
code = (*pname.iodev->procs.file_status)(pname.iodev,
pname.fname, &fstat);
switch (code) {
case 0:
check_ostack(4);
push(4);
make_int(op - 4, stat_blocks(&fstat));
make_int(op - 3, fstat.st_size);
if ((double)op[-4].value.intval !=
(double)stat_blocks(&fstat) ||
(double)op[-3].value.intval !=
(double)fstat.st_size
)
return_error(e_limitcheck);
make_int(op - 2, fstat.st_mtime);
make_int(op - 1, fstat.st_ctime);
make_bool(op, 1);
break;
case e_undefinedfilename:
make_bool(op, 0);
code = 0;
}
gs_free_file_name(&pname, "status");
return code;
}
default:
return_op_typecheck(op);
}
}
private int
zexecfile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type_access(*op, t_file, a_executable | a_read | a_execute);
check_estack(4);
push_mark_estack(es_other, execfile_cleanup);
*++esp = *op;
push_op_estack(execfile_finish);
return zexec(i_ctx_p);
}
private int
execfile_finish(i_ctx_t *i_ctx_p)
{
check_ostack(1);
esp -= 2;
execfile_cleanup(i_ctx_p);
return o_pop_estack;
}
private int
execfile_cleanup(i_ctx_t *i_ctx_p)
{
check_ostack(1);
*++osp = esp[2];
return zclosefile(i_ctx_p);
}
private int
zfilenamelistseparator(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
make_const_string(op, avm_foreign | a_readonly, 1,
(const byte *)&gp_file_name_list_separator);
return 0;
}
private int
zfilenamesplit(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_read_type(*op, t_string);
return_error(e_undefined);
}
private int
zlibfile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code;
byte cname[gp_file_name_sizeof];
uint clen;
gs_parsed_file_name_t pname;
stream *s;
check_ostack(2);
code = parse_file_name(op, &pname, i_ctx_p->LockFilePermissions);
if (code < 0)
return code;
if (pname.iodev == NULL)
pname.iodev = iodev_default;
if (pname.iodev != iodev_default) {
code = zopen_file(i_ctx_p, &pname, "r", &s, imemory);
if (code >= 0) {
code = ssetfilename(s, op->value.const_bytes, r_size(op));
if (code < 0) {
sclose(s);
return_error(e_VMerror);
}
}
if (code < 0) {
push(1);
make_false(op);
return 0;
}
make_stream_file(op, s, "r");
} else {
ref fref;
code = lib_file_open(i_ctx_p->lib_path, i_ctx_p, pname.fname, pname.len, cname, sizeof(cname),
&clen, &fref, imemory);
if (code >= 0) {
s = fptr(&fref);
code = ssetfilename(s, cname, clen);
if (code < 0) {
sclose(s);
return_error(e_VMerror);
}
}
if (code < 0) {
if (code == e_VMerror || code == e_invalidfileaccess)
return code;
push(1);
make_false(op);
return 0;
}
ref_assign(op, &fref);
}
push(1);
make_true(op);
return 0;
}
private bool
prefix_is_simple(const char *pstr)
{
int i;
char c;
for (i = 0; (c = pstr[i]) != 0; i++) {
if (!(c == '-' || c == '_' || (c >= '0' && c <= '9') ||
(c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')))
return false;
}
return true;
}
private int
ztempfile(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
const char *pstr;
char fmode[4];
int code = parse_file_access_string(op, fmode);
char prefix[gp_file_name_sizeof];
char fname[gp_file_name_sizeof];
uint fnlen;
FILE *sfile;
stream *s;
byte *buf;
if (code < 0)
return code;
strcat(fmode, gp_fmode_binary_suffix);
if (r_has_type(op - 1, t_null))
pstr = gp_scratch_file_name_prefix;
else {
uint psize;
check_read_type(op[-1], t_string);
psize = r_size(op - 1);
if (psize >= gp_file_name_sizeof)
return_error(e_rangecheck);
memcpy(prefix, op[-1].value.const_bytes, psize);
prefix[psize] = 0;
pstr = prefix;
}
if (gp_file_name_is_absolute(pstr, strlen(pstr))) {
if (check_file_permissions(i_ctx_p, pstr, strlen(pstr),
"PermitFileWriting") < 0) {
return_error(e_invalidfileaccess);
}
} else if (!prefix_is_simple(pstr)) {
return_error(e_invalidfileaccess);
}
s = file_alloc_stream(imemory, "ztempfile(stream)");
if (s == 0)
return_error(e_VMerror);
buf = gs_alloc_bytes(imemory, file_default_buffer_size,
"ztempfile(buffer)");
if (buf == 0)
return_error(e_VMerror);
sfile = gp_open_scratch_file(pstr, fname, fmode);
if (sfile == 0) {
gs_free_object(imemory, buf, "ztempfile(buffer)");
return_error(e_invalidfileaccess);
}
fnlen = strlen(fname);
file_init_stream(s, sfile, fmode, buf, file_default_buffer_size);
code = ssetfilename(s, (const unsigned char*) fname, fnlen);
if (code < 0) {
sclose(s);
iodev_default->procs.delete_file(iodev_default, fname);
return_error(e_VMerror);
}
make_const_string(op - 1, a_readonly | icurrent_space, fnlen,
s->file_name.data);
make_stream_file(op, s, fmode);
return code;
}
const op_def zfile_op_defs[] =
{
{"1deletefile", zdeletefile},
{"1.execfile", zexecfile},
{"2file", zfile},
{"3filenameforall", zfilenameforall},
{"0.filenamelistseparator", zfilenamelistseparator},
{"1.filenamesplit", zfilenamesplit},
{"1.libfile", zlibfile},
{"2renamefile", zrenamefile},
{"1status", zstatus},
{"2.tempfile", ztempfile},
{"0%file_continue", file_continue},
{"0%execfile_finish", execfile_finish},
op_def_end(zfile_init)
};
private int
parse_file_name(const ref * op, gs_parsed_file_name_t * pfn, bool safemode)
{
int code;
check_read_type(*op, t_string);
code = gs_parse_file_name(pfn, (const char *)op->value.const_bytes,
r_size(op));
if (code < 0)
return code;
if (pfn->iodev && safemode && strcmp(pfn->iodev->dname, "%pipe%") == 0)
return e_invalidfileaccess;
return code;
}
private int
parse_real_file_name(const ref *op, gs_parsed_file_name_t *pfn,
gs_memory_t *mem, client_name_t cname)
{
check_read_type(*op, t_string);
return gs_parse_real_file_name(pfn, (const char *)op->value.const_bytes,
r_size(op), mem, cname);
}
private int
parse_file_access_string(const ref *op, char file_access[4])
{
const byte *astr;
check_read_type(*op, t_string);
astr = op->value.const_bytes;
switch (r_size(op)) {
case 2:
if (astr[1] != '+')
return_error(e_invalidfileaccess);
file_access[1] = '+';
file_access[2] = 0;
break;
case 1:
file_access[1] = 0;
break;
default:
return_error(e_invalidfileaccess);
}
switch (astr[0]) {
case 'r':
case 'w':
case 'a':
break;
default:
return_error(e_invalidfileaccess);
}
file_access[0] = astr[0];
return 0;
}
private int
zopen_file(i_ctx_t *i_ctx_p, const gs_parsed_file_name_t *pfn,
const char *file_access, stream **ps, gs_memory_t *mem)
{
gx_io_device *const iodev = pfn->iodev;
if (pfn->fname == NULL)
return iodev->procs.open_device(iodev, file_access, ps, mem);
else {
iodev_proc_open_file((*open_file)) = iodev->procs.open_file;
if (open_file == 0)
open_file = iodev_os_open_file;
if (open_file == iodev_os_open_file) {
int code = check_file_permissions(i_ctx_p, pfn->fname, pfn->len,
file_access[0] == 'r' ? "PermitFileReading" : "PermitFileWriting");
if (code < 0)
return code;
}
return open_file(iodev, pfn->fname, pfn->len, file_access, ps, mem);
}
}
private int
iodev_os_open_file(gx_io_device * iodev, const char *fname, uint len,
const char *file_access, stream ** ps, gs_memory_t * mem)
{
return file_open_stream(fname, len, file_access,
file_default_buffer_size, ps,
iodev, iodev->procs.fopen, mem);
}
void
make_stream_file(ref * pfile, stream * s, const char *access)
{
uint attrs =
(access[1] == '+' ? a_write + a_read + a_execute : 0) |
imemory_space((gs_ref_memory_t *) s->memory);
if (access[0] == 'r') {
make_file(pfile, attrs | (a_read | a_execute), s->read_id, s);
s->write_id = 0;
} else {
make_file(pfile, attrs | a_write, s->write_id, s);
s->read_id = 0;
}
}
private gp_file_name_combine_result
gp_file_name_combine_patch(const char *prefix, uint plen, const char *fname, uint flen,
bool no_sibling, char *buffer, uint *blen)
{
return gp_file_name_combine(prefix, plen, fname, flen, no_sibling, buffer, blen);
}
private int
file_prepare_stream(const char *fname, uint len, const char *file_access,
uint buffer_size, stream ** ps, char fmode[4],
gx_io_device *iodev, gs_memory_t *mem)
{
byte *buffer;
register stream *s;
strcpy(fmode, file_access);
strcat(fmode, gp_fmode_binary_suffix);
if (buffer_size == 0)
buffer_size = file_default_buffer_size;
if (len >= buffer_size)
return_error(e_limitcheck);
s = file_alloc_stream(mem, "file_prepare_stream");
if (s == 0)
return_error(e_VMerror);
buffer = gs_alloc_bytes(mem, buffer_size, "file_prepare_stream(buffer)");
if (buffer == 0)
return_error(e_VMerror);
if (fname != 0) {
memcpy(buffer, fname, len);
buffer[len] = 0;
} else
buffer[0] = 0;
s->cbuf = buffer;
s->bsize = s->cbsize = buffer_size;
*ps = s;
return 0;
}
private int
check_file_permissions_aux(i_ctx_t *i_ctx_p, char *fname, uint flen)
{
if (i_ctx_p == NULL)
return 0;
if (check_file_permissions_reduced(i_ctx_p, fname, flen, "PermitFileReading") < 0)
return_error(e_invalidfileaccess);
return 0;
}
private int
lib_fopen_with_libpath(gs_file_path_ptr lib_path,
const gs_memory_t *mem,
i_ctx_t *i_ctx_p,
gx_io_device *iodev,
const char *fname, uint flen, char fmode[4], char *buffer, int blen,
FILE **file)
{
bool starting_arg_file = false;
bool search_with_no_combine = false;
bool search_with_combine = false;
if (i_ctx_p != NULL) {
starting_arg_file = i_ctx_p->starting_arg_file;
i_ctx_p->starting_arg_file = false;
} else
starting_arg_file = true;
if (gp_file_name_is_absolute(fname, flen)) {
search_with_no_combine = true;
search_with_combine = false;
} else {
search_with_no_combine = starting_arg_file;
search_with_combine = true;
}
if (search_with_no_combine) {
uint blen1 = blen;
if (gp_file_name_reduce(fname, flen, buffer, &blen1) != gp_combine_success)
goto skip;
if (iodev->procs.fopen(iodev, buffer, fmode, file,
buffer, blen) == 0) {
if (starting_arg_file ||
check_file_permissions_aux(i_ctx_p, buffer, blen1) >= 0)
return 0;
iodev->procs.fclose(iodev, *file);
*file = NULL;
return_error(e_invalidfileaccess);
} else
*file = NULL;
skip:;
}
if (search_with_combine) {
const gs_file_path *pfpath = lib_path;
uint pi;
for (pi = 0; pi < r_size(&pfpath->list); ++pi) {
const ref *prdir = pfpath->list.value.refs + pi;
const char *pstr = (const char *)prdir->value.const_bytes;
uint plen = r_size(prdir), blen1 = blen;
gp_file_name_combine_result r = gp_file_name_combine_patch(pstr, plen,
fname, flen, false, buffer, &blen1);
if (r != gp_combine_success)
continue;
if (iodev->procs.fopen(iodev, buffer, fmode, file,
buffer, blen) == 0) {
if (starting_arg_file ||
check_file_permissions_aux(i_ctx_p, buffer, blen1) >= 0)
return 0;
iodev->procs.fclose(iodev, *file);
*file = NULL;
return_error(e_invalidfileaccess);
}
*file = NULL;
}
}
return_error(e_undefinedfilename);
}
FILE *
lib_fopen(const gs_file_path_ptr pfpath, const gs_memory_t *mem, const char *fname)
{
char buffer[gp_file_name_sizeof];
gx_io_device iodev_default_copy = *gx_io_device_table[0];
char fmode[3] = "r";
FILE *file = NULL;
strcat(fmode, gp_fmode_binary_suffix);
lib_fopen_with_libpath(pfpath, mem, NULL, &iodev_default_copy, fname, strlen(fname),
fmode, buffer, sizeof(buffer), &file);
return file;
}
int
lib_file_open(const gs_file_path_ptr pfpath,
i_ctx_t *i_ctx_p, const char *fname, uint len, byte * cname, uint max_clen,
uint * pclen, ref * pfile, gs_memory_t *mem)
{
stream *s;
int code;
char fmode[4];
char *buffer;
uint blen;
gx_io_device *iodev = iodev_default;
FILE *file;
code = file_prepare_stream(fname, len, "r", file_default_buffer_size,
&s, fmode, iodev, mem);
if (code < 0)
return code;
if (fname == 0)
return 0;
buffer = (char *)s->cbuf;
code = lib_fopen_with_libpath(pfpath, mem, i_ctx_p,
iodev, fname, len, fmode, buffer, s->bsize, &file);
if (code < 0) {
s->cbuf = NULL;
s->bsize = s->cbsize = 0;
gs_free_object(mem, buffer, "lib_file_open");
return code;
}
file_init_stream(s, file, fmode, (byte *)buffer, s->bsize);
blen = strlen(buffer);
if (blen > max_clen) {
sclose(s);
return_error(e_limitcheck);
}
memcpy(cname, buffer, blen);
*pclen = blen;
make_stream_file(pfile, s, "r");
return 0;
}
int
file_read_string(const byte *str, uint len, ref *pfile, gs_ref_memory_t *imem)
{
stream *s = file_alloc_stream((gs_memory_t *)imem, "file_read_string");
if (s == 0)
return_error(e_VMerror);
sread_string(s, str, len);
s->foreign = 1;
s->write_id = 0;
make_file(pfile, a_readonly | imemory_space(imem), s->read_id, s);
s->save_close = s->procs.close;
s->procs.close = file_close_disable;
return 0;
}
private void
file_init_stream(stream *s, FILE *file, const char *fmode, byte *buffer,
uint buffer_size)
{
switch (fmode[0]) {
case 'a':
sappend_file(s, file, buffer, buffer_size);
break;
case 'r':
{
struct stat rstat;
fstat(fileno(file), &rstat);
sread_file(s, file, buffer,
(S_ISCHR(rstat.st_mode) ? 1 : buffer_size));
}
break;
case 'w':
swrite_file(s, file, buffer, buffer_size);
}
if (fmode[1] == '+')
s->file_modes |= s_mode_read | s_mode_write;
s->save_close = s->procs.close;
s->procs.close = file_close_file;
}
int
file_open_stream(const char *fname, uint len, const char *file_access,
uint buffer_size, stream ** ps, gx_io_device *iodev,
iodev_proc_fopen_t fopen_proc, gs_memory_t *mem)
{
int code;
FILE *file;
char fmode[4];
if (!iodev)
iodev = iodev_default;
code = file_prepare_stream(fname, len, file_access, buffer_size, ps, fmode,
(!iodev ? iodev_default : iodev), mem);
if (code < 0)
return code;
if (fname == 0)
return 0;
code = (*fopen_proc)(iodev, (char *)(*ps)->cbuf, fmode, &file,
(char *)(*ps)->cbuf, (*ps)->bsize);
if (code < 0)
return code;
file_init_stream(*ps, file, fmode, (*ps)->cbuf, (*ps)->bsize);
return 0;
}
int
filter_report_error(stream_state * st, const char *str)
{
if_debug1('s', "[s]stream error: %s\n", str);
strncpy(st->error_string, str, STREAM_MAX_ERROR_STRING);
st->error_string[STREAM_MAX_ERROR_STRING] = 0;
return 0;
}
int
filter_open(const char *file_access, uint buffer_size, ref * pfile,
const stream_procs * procs, const stream_template * template,
const stream_state * st, gs_memory_t *mem)
{
stream *s;
uint ssize = gs_struct_type_size(template->stype);
stream_state *sst = 0;
int code;
if (template->stype != &st_stream_state) {
sst = s_alloc_state(mem, template->stype, "filter_open(stream_state)");
if (sst == 0)
return_error(e_VMerror);
}
code = file_open_stream((char *)0, 0, file_access, buffer_size, &s,
(gx_io_device *)0, (iodev_proc_fopen_t)0, mem);
if (code < 0) {
gs_free_object(mem, sst, "filter_open(stream_state)");
return code;
}
s_std_init(s, s->cbuf, s->bsize, procs,
(*file_access == 'r' ? s_mode_read : s_mode_write));
s->procs.process = template->process;
s->save_close = s->procs.close;
s->procs.close = file_close_file;
if (sst == 0) {
sst = (stream_state *) s;
} else if (st != 0)
memcpy(sst, st, ssize);
s->state = sst;
s_init_state(sst, template, mem);
sst->report_error = filter_report_error;
if (template->init != 0) {
code = (*template->init)(sst);
if (code < 0) {
gs_free_object(mem, sst, "filter_open(stream_state)");
gs_free_object(mem, s->cbuf, "filter_open(buffer)");
return code;
}
}
make_stream_file(pfile, s, file_access);
return 0;
}
stream *
file_alloc_stream(gs_memory_t * mem, client_name_t cname)
{
stream *s;
gs_ref_memory_t *imem = 0;
if (mem->procs.free_object == gs_ref_memory_procs.free_object)
imem = (gs_ref_memory_t *) mem;
if (imem) {
s = imem->streams;
while (s != 0) {
if (!s_is_valid(s) && s->read_id != 0 ) {
s->is_temp = 0;
return s;
}
s = s->next;
}
}
s = s_alloc(mem, cname);
if (s == 0)
return 0;
s_init_ids(s);
s->is_temp = 0;
s_disable(s);
if (imem) {
if (imem->streams != 0)
imem->streams->prev = s;
s->next = imem->streams;
imem->streams = s;
} else {
s->next = 0;
}
s->prev = 0;
return s;
}
int
file_close_finish(stream * s)
{
return 0;
}
int
file_close_disable(stream * s)
{
int code = (*s->save_close)(s);
if (code)
return code;
s->read_id = s->write_id = (s->read_id | s->write_id) + 1;
return file_close_finish(s);
}
int
file_close_file(stream * s)
{
stream *stemp = s->strm;
gs_memory_t *mem;
int code = file_close_disable(s);
if (code)
return code;
while (stemp != 0 && stemp->is_temp != 0) {
stream *snext = stemp->strm;
mem = stemp->memory;
if (stemp->is_temp > 1)
gs_free_object(mem, stemp->cbuf,
"file_close(temp stream buffer)");
s_disable(stemp);
stemp = snext;
}
mem = s->memory;
gs_free_object(mem, s->cbuf, "file_close(buffer)");
if (s->close_strm && stemp != 0)
return sclose(stemp);
return 0;
}
int
file_close(ref * pfile)
{
stream *s;
if (file_is_valid(s, pfile)) {
if (sclose(s))
return_error(e_ioerror);
}
return 0;
}