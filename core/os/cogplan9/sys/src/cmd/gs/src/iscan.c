#include "ghost.h"
#include "memory_.h"
#include "stream.h"
#include "ierrors.h"
#include "btoken.h"
#include "files.h"
#include "ialloc.h"
#include "idict.h"
#include "dstack.h"
#include "ilevel.h"
#include "iname.h"
#include "ipacked.h"
#include "iparray.h"
#include "strimpl.h"
#include "sa85d.h"
#include "sfilter.h"
#include "ostack.h"
#include "iscan.h"
#include "iscanbin.h"
#include "iscannum.h"
#include "istream.h"
#include "istruct.h"
#include "iutil.h"
#include "ivmspace.h"
#include "store.h"
#include "scanchar.h"
#define recognize_btokens()\
(ref_binary_object_format.value.intval != 0 && level2_enabled)
int (*scan_dsc_proc) (const byte *, uint) = NULL;
int (*scan_comment_proc) (const byte *, uint) = NULL;
#define scan_enable_level2 level2_enabled
inline private void
dynamic_init(da_ptr pda, gs_memory_t *mem)
{
pda->is_dynamic = false;
pda->limit = pda->buf + da_buf_size;
pda->next = pda->base = pda->buf;
pda->memory = mem;
}
private void
dynamic_free(da_ptr pda)
{
if (pda->is_dynamic)
gs_free_string(pda->memory, pda->base, da_size(pda), "scanner");
}
private int
dynamic_resize(da_ptr pda, uint new_size)
{
uint old_size = da_size(pda);
uint pos = pda->next - pda->base;
gs_memory_t *mem = pda->memory;
byte *base;
if (pda->is_dynamic) {
base = gs_resize_string(mem, pda->base, old_size,
new_size, "scanner");
if (base == 0)
return_error(e_VMerror);
} else {
base = gs_alloc_string(mem, new_size, "scanner");
if (base == 0)
return_error(e_VMerror);
memcpy(base, pda->base, min(old_size, new_size));
pda->is_dynamic = true;
}
pda->base = base;
pda->next = base + pos;
pda->limit = base + new_size;
return 0;
}
private int
dynamic_grow(da_ptr pda, byte * next, uint max_size)
{
uint old_size = da_size(pda);
uint new_size = (old_size < 10 ? 20 :
old_size >= (max_size >> 1) ? max_size :
old_size << 1);
int code;
pda->next = next;
if (old_size == max_size)
return_error(e_limitcheck);
while ((code = dynamic_resize(pda, new_size)) < 0 &&
new_size > old_size
) {
new_size -= (new_size - old_size + 1) >> 1;
}
return code;
}
private void
dynamic_save(da_ptr pda)
{
if (!pda->is_dynamic && pda->base != pda->buf) {
memcpy(pda->buf, pda->base, da_size(pda));
pda->next = pda->buf + da_size(pda);
pda->base = pda->buf;
}
}
private int
dynamic_make_string(i_ctx_t *i_ctx_p, ref * pref, da_ptr pda, byte * next)
{
uint size = (pda->next = next) - pda->base;
int code = dynamic_resize(pda, size);
if (code < 0)
return code;
make_tasv_new(pref, t_string,
a_all | imemory_space((gs_ref_memory_t *) pda->memory),
size, bytes, pda->base);
return 0;
}
#define ssarray ssptr->s_ss.binary.bin_array
private
CLEAR_MARKS_PROC(scanner_clear_marks)
{
scanner_state *const ssptr = vptr;
r_clear_attrs(&ssarray, l_mark);
}
private
ENUM_PTRS_WITH(scanner_enum_ptrs, scanner_state *ssptr) return 0;
case 0:
if (ssptr->s_scan_type == scanning_none ||
!ssptr->s_da.is_dynamic
)
ENUM_RETURN(0);
return ENUM_STRING2(ssptr->s_da.base, da_size(&ssptr->s_da));
case 1:
if (ssptr->s_scan_type != scanning_binary)
return 0;
ENUM_RETURN_REF(&ssarray);
ENUM_PTRS_END
private RELOC_PTRS_WITH(scanner_reloc_ptrs, scanner_state *ssptr)
{
if (ssptr->s_scan_type != scanning_none && ssptr->s_da.is_dynamic) {
gs_string sda;
sda.data = ssptr->s_da.base;
sda.size = da_size(&ssptr->s_da);
RELOC_STRING_VAR(sda);
ssptr->s_da.limit = sda.data + sda.size;
ssptr->s_da.next = sda.data + (ssptr->s_da.next - ssptr->s_da.base);
ssptr->s_da.base = sda.data;
}
if (ssptr->s_scan_type == scanning_binary) {
RELOC_REF_VAR(ssarray);
r_clear_attrs(&ssarray, l_mark);
}
}
RELOC_PTRS_END
public_st_scanner_state();
void
scanner_state_init_options(scanner_state *sstate, int options)
{
sstate->s_scan_type = scanning_none;
sstate->s_pstack = 0;
sstate->s_options = options;
}
int
scan_handle_refill(i_ctx_t *i_ctx_p, const ref * fop, scanner_state * sstate,
bool save, bool push_file, op_proc_t cont)
{
stream *s = fptr(fop);
uint avail = sbufavailable(s);
int status;
if (s->end_status == EOFC) {
return_error(e_syntaxerror);
}
status = s_process_read_buf(s);
if (sbufavailable(s) > avail)
return 0;
if (status == 0)
status = s->end_status;
switch (status) {
case EOFC:
return 0;
case ERRC:
return_error(e_ioerror);
case INTC:
case CALLC:
{
ref rstate[2];
scanner_state *pstate;
int nstate = (push_file ? 2 : 1);
if (save) {
pstate =
ialloc_struct(scanner_state, &st_scanner_state,
"scan_handle_refill");
if (pstate == 0)
return_error(e_VMerror);
*pstate = *sstate;
} else
pstate = sstate;
make_istruct(&rstate[0], 0, pstate);
rstate[1] = *fop;
r_clear_attrs(&rstate[1], a_executable);
return s_handle_read_exception(i_ctx_p, status, fop,
rstate, nstate, cont);
}
}
lprintf("Can't refill scanner input buffer!");
return_error(e_Fatal);
}
private int
scan_comment(i_ctx_t *i_ctx_p, ref *pref, scanner_state *pstate,
const byte * base, const byte * end, bool saved)
{
uint len = (uint) (end - base);
int code;
#ifdef DEBUG
const char *sstr = (saved ? ">" : "");
#endif
if (len > 1 && (base[1] == '%' || base[1] == '!')) {
#ifdef DEBUG
if (gs_debug_c('%')) {
dlprintf2("[%%%%%s%c]", sstr, (len >= 3 ? '+' : '-'));
debug_print_string(base, len);
dputs("\n");
}
#endif
if (scan_dsc_proc != NULL) {
code = scan_dsc_proc(base, len);
return (code < 0 ? code : 0);
}
if (pstate->s_options & SCAN_PROCESS_DSC_COMMENTS) {
code = scan_DSC_Comment;
goto comment;
}
}
#ifdef DEBUG
else {
if (gs_debug_c('%')) {
dlprintf2("[%% %s%c]", sstr, (len >= 2 ? '+' : '-'));
debug_print_string(base, len);
dputs("\n");
}
}
#endif
if (scan_comment_proc != NULL) {
code = scan_comment_proc(base, len);
return (code < 0 ? code : 0);
}
if (pstate->s_options & SCAN_PROCESS_COMMENTS) {
code = scan_Comment;
goto comment;
}
return 0;
comment:
{
byte *cstr = ialloc_string(len, "scan_comment");
if (cstr == 0)
return_error(e_VMerror);
memcpy(cstr, base, len);
make_string(pref, a_all | icurrent_space, len, cstr);
}
return code;
}
int
scan_string_token_options(i_ctx_t *i_ctx_p, ref * pstr, ref * pref,
int options)
{
stream st;
stream *s = &st;
scanner_state state;
int code;
if (!r_has_attr(pstr, a_read))
return_error(e_invalidaccess);
s_init(s, NULL);
sread_string(s, pstr->value.bytes, r_size(pstr));
scanner_state_init_options(&state, options | SCAN_FROM_STRING);
switch (code = scan_token(i_ctx_p, s, pref, &state)) {
default:
if (code < 0)
break;
case 0:
case scan_BOS:
{
uint pos = stell(s);
pstr->value.bytes += pos;
r_dec_size(pstr, pos);
}
break;
case scan_Refill:
code = gs_note_error(e_syntaxerror);
case scan_EOF:
break;
}
return code;
}
int
scan_token(i_ctx_t *i_ctx_p, stream * s, ref * pref, scanner_state * pstate)
{
ref *myref = pref;
int retcode = 0;
int c;
s_declare_inline(s, sptr, endptr);
#define scan_begin_inline() s_begin_inline(s, sptr, endptr)
#define scan_getc() sgetc_inline(s, sptr, endptr)
#define scan_putback() sputback_inline(s, sptr, endptr)
#define scan_end_inline() s_end_inline(s, sptr, endptr)
const byte *newptr;
byte *daptr;
#define sreturn(code)\
{ retcode = gs_note_error(code); goto sret; }
#define sreturn_no_error(code)\
{ scan_end_inline(); return(code); }
#define if_not_spush1()\
if ( osp < ostop ) osp++;\
else if ( (retcode = ref_stack_push(&o_stack, 1)) >= 0 )\
;\
else
#define spop1()\
if ( osp >= osbot ) osp--;\
else ref_stack_pop(&o_stack, 1)
int max_name_ctype =
(recognize_btokens()? ctype_name : ctype_btoken);
#define scan_sign(sign, ptr)\
switch ( *ptr ) {\
case '-': sign = -1; ptr++; break;\
case '+': sign = 1; ptr++; break;\
default: sign = 0;\
}
#define ensure2_back(styp,nback)\
if ( sptr >= endptr ) { sptr -= nback; scan_type = styp; goto pause; }
#define ensure2(styp) ensure2_back(styp, 1)
byte s1[2];
const byte *const decoder = scan_char_decoder;
int status;
int sign;
const bool check_only = (pstate->s_options & SCAN_CHECK_ONLY) != 0;
const bool PDFScanRules = (i_ctx_p->scanner_options & SCAN_PDF_RULES) != 0;
const bool PDFScanInvNum = (i_ctx_p->scanner_options & SCAN_PDF_INV_NUM) != 0;
scanner_state sstate;
#define pstack sstate.s_pstack
#define pdepth sstate.s_pdepth
#define scan_type sstate.s_scan_type
#define da sstate.s_da
#define name_type sstate.s_ss.s_name.s_name_type
#define try_number sstate.s_ss.s_name.s_try_number
sptr = endptr = NULL;
if (pstate->s_pstack != 0) {
if_not_spush1()
return retcode;
myref = osp;
}
if (pstate->s_scan_type != scanning_none) {
sstate = *pstate;
if (!da.is_dynamic && da.base != da.buf) {
uint next = da.next - da.base;
uint limit = da.limit - da.base;
da.base = da.buf;
da.next = da.buf + next;
da.limit = da.buf + limit;
}
daptr = da.next;
switch (scan_type) {
case scanning_binary:
retcode = (*sstate.s_ss.binary.cont)
(i_ctx_p, s, myref, &sstate);
scan_begin_inline();
if (retcode == scan_Refill)
goto pause;
goto sret;
case scanning_comment:
scan_begin_inline();
goto cont_comment;
case scanning_name:
goto cont_name;
case scanning_string:
goto cont_string;
default:
return_error(e_Fatal);
}
}
pstack = pstate->s_pstack;
pdepth = pstate->s_pdepth;
sstate.s_options = pstate->s_options;
scan_begin_inline();
top:c = scan_getc();
if_debug1('S', (c >= 32 && c <= 126 ? "`%c'" : c >= 0 ? "`\\%03o'" : "`%d'"), c);
switch (c) {
case ' ':
case '\f':
case '\t':
case char_CR:
case char_EOL:
case char_NULL:
goto top;
case 0x4:
case '[':
case ']':
s1[0] = (byte) c;
retcode = name_ref(imemory, s1, 1, myref, 1);
r_set_attrs(myref, a_executable);
break;
case '<':
if (scan_enable_level2) {
ensure2(scanning_none);
c = scan_getc();
switch (c) {
case '<':
scan_putback();
name_type = 0;
try_number = false;
goto try_funny_name;
case '~':
s_A85D_init_inline(&sstate.s_ss.a85d);
sstate.s_ss.st.template = &s_A85D_template;
goto str;
}
scan_putback();
}
s_AXD_init_inline(&sstate.s_ss.axd);
sstate.s_ss.st.template = &s_AXD_template;
str:scan_end_inline();
dynamic_init(&da, imemory);
cont_string:for (;;) {
stream_cursor_write w;
w.ptr = da.next - 1;
w.limit = da.limit - 1;
status = (*sstate.s_ss.st.template->process)
(&sstate.s_ss.st, &s->cursor.r, &w,
s->end_status == EOFC);
if (!check_only)
da.next = w.ptr + 1;
switch (status) {
case 0:
status = s->end_status;
if (status < 0) {
if (status == EOFC) {
if (check_only) {
retcode = scan_Refill;
scan_type = scanning_string;
goto suspend;
} else
sreturn(e_syntaxerror);
}
break;
}
s_process_read_buf(s);
continue;
case 1:
if (!check_only) {
retcode = dynamic_grow(&da, da.next, max_string_size);
if (retcode == e_VMerror) {
scan_type = scanning_string;
goto suspend;
} else if (retcode < 0)
sreturn(retcode);
}
continue;
}
break;
}
scan_begin_inline();
switch (status) {
default:
sreturn(e_syntaxerror);
case INTC:
case CALLC:
scan_type = scanning_string;
goto pause;
case EOFC:
;
}
retcode = dynamic_make_string(i_ctx_p, myref, &da, da.next);
if (retcode < 0) {
sputback(s);
scan_type = scanning_string;
goto suspend;
}
break;
case '(':
sstate.s_ss.pssd.from_string =
((pstate->s_options & SCAN_FROM_STRING) != 0) &&
!scan_enable_level2;
s_PSSD_partially_init_inline(&sstate.s_ss.pssd);
sstate.s_ss.st.template = &s_PSSD_template;
goto str;
case '{':
if (pstack == 0) {
if_not_spush1() {
scan_putback();
scan_type = scanning_none;
goto pause_ret;
}
pdepth = ref_stack_count_inline(&o_stack);
}
make_int(osp, pstack);
pstack = ref_stack_count_inline(&o_stack);
if_debug3('S', "[S{]d=%d, s=%d->%d\n",
pdepth, (int)osp->value.intval, pstack);
goto snext;
case '>':
if (scan_enable_level2) {
ensure2(scanning_none);
name_type = 0;
try_number = false;
goto try_funny_name;
}
case ')':
sreturn(e_syntaxerror);
case '}':
if (pstack == 0)
sreturn(e_syntaxerror);
osp--;
{
uint size = ref_stack_count_inline(&o_stack) - pstack;
ref arr;
if_debug4('S', "[S}]d=%d, s=%d->%ld, c=%d\n",
pdepth, pstack,
(pstack == pdepth ? 0 :
ref_stack_index(&o_stack, size)->value.intval),
size + pstack);
myref = (pstack == pdepth ? pref : &arr);
if (check_only) {
make_empty_array(myref, 0);
ref_stack_pop(&o_stack, size);
} else if (ref_array_packing.value.boolval) {
retcode = make_packed_array(myref, &o_stack, size,
idmemory, "scanner(packed)");
if (retcode < 0) {
osp++;
scan_putback();
scan_type = scanning_none;
goto pause_ret;
}
r_set_attrs(myref, a_executable);
} else {
retcode = ialloc_ref_array(myref,
a_executable + a_all, size,
"scanner(proc)");
if (retcode < 0) {
osp++;
scan_putback();
scan_type = scanning_none;
goto pause_ret;
}
retcode = ref_stack_store(&o_stack, myref, size, 0, 1,
false, idmemory, "scanner");
if (retcode < 0) {
ifree_ref_array(myref, "scanner(proc)");
sreturn(retcode);
}
ref_stack_pop(&o_stack, size);
}
if (pstack == pdepth) {
spop1();
pstack = 0;
} else {
if (osp < osbot)
ref_stack_pop_block(&o_stack);
pstack = osp->value.intval;
*osp = arr;
goto snext;
}
}
break;
case '/':
ensure2(scanning_none);
c = scan_getc();
if (!PDFScanRules && (c == '/')) {
name_type = 2;
c = scan_getc();
} else
name_type = 1;
try_number = false;
switch (decoder[c]) {
case ctype_name:
default:
goto do_name;
case ctype_btoken:
if (!recognize_btokens())
goto do_name;
case ctype_exception:
case ctype_space:
case ctype_other:
da.base = da.limit = daptr = 0;
da.is_dynamic = false;
goto nx;
}
case '%':
{
const byte *base = sptr;
const byte *end;
while (++sptr < endptr)
switch (*sptr) {
case char_CR:
end = sptr;
if (sptr[1] == char_EOL)
sptr++;
cend:
retcode = scan_comment(i_ctx_p, myref, &sstate,
base, end, false);
if (retcode != 0)
goto comment;
goto top;
case char_EOL:
case '\f':
end = sptr;
goto cend;
}
#define comment_line da.buf
--sptr;
comment_line[1] = 0;
{
uint len = sptr + 1 - base;
if (len > sizeof(comment_line))
len = sizeof(comment_line);
memcpy(comment_line, base, len);
daptr = comment_line + len;
}
da.base = comment_line;
da.is_dynamic = false;
}
cont_comment:for (;;) {
switch ((c = scan_getc())) {
default:
if (c < 0)
switch (c) {
case INTC:
case CALLC:
da.next = daptr;
scan_type = scanning_comment;
goto pause;
case EOFC:
goto end_comment;
default:
sreturn(e_syntaxerror);
}
if (daptr < comment_line + max_comment_line)
*daptr++ = c;
continue;
case char_CR:
case char_EOL:
case '\f':
end_comment:
retcode = scan_comment(i_ctx_p, myref, &sstate,
comment_line, daptr, true);
if (retcode != 0)
goto comment;
goto top;
}
}
#undef comment_line
case EOFC:
if (pstack != 0) {
if (check_only)
goto pause;
sreturn(e_syntaxerror);
}
retcode = scan_EOF;
break;
case ERRC:
sreturn(e_ioerror);
try_funny_name:
{
int c1 = scan_getc();
if (c1 == c) {
s1[0] = s1[1] = c;
name_ref(imemory, s1, 2, myref, 1);
goto have_name;
}
scan_putback();
}
sreturn(e_syntaxerror);
case '0':
case '1':
case '2':
case '3':
case '4':
case '5':
case '6':
case '7':
case '8':
case '9':
case '.':
sign = 0;
nr:
retcode = scan_number(sptr + (sign & 1),
endptr  ,
sign, myref, &newptr, PDFScanInvNum);
if (retcode == 1 && decoder[newptr[-1]] == ctype_space) {
sptr = newptr - 1;
if (*sptr == char_CR && sptr[1] == char_EOL)
sptr++;
retcode = 0;
ref_mark_new(myref);
break;
}
name_type = 0;
try_number = true;
goto do_name;
case '+':
sign = 1;
goto nr;
case '-':
sign = -1;
goto nr;
#define case4(c) case c: case c+1: case c+2: case c+3
case4(128): case4(132): case4(136): case4(140):
case4(144): case4(148): case4(152): case4(156):
#undef case4
if (recognize_btokens()) {
scan_end_inline();
retcode = scan_binary_token(i_ctx_p, s, myref, &sstate);
scan_begin_inline();
if (retcode == scan_Refill)
goto pause;
break;
}
default:
if (c < 0) {
dynamic_init(&da, name_memory(imemory));
scan_type = scanning_none;
goto pause;
}
case '!':
case '"':
case '#':
case '$':
case '&':
case '\'':
case '*':
case ',':
case '=':
case ':':
case ';':
case '?':
case '@':
case 'A':
case 'B':
case 'C':
case 'D':
case 'E':
case 'F':
case 'G':
case 'H':
case 'I':
case 'J':
case 'K':
case 'L':
case 'M':
case 'N':
case 'O':
case 'P':
case 'Q':
case 'R':
case 'S':
case 'T':
case 'U':
case 'V':
case 'W':
case 'X':
case 'Y':
case 'Z':
case '\\':
case '^':
case '_':
case '`':
case 'a':
case 'b':
case 'c':
case 'd':
case 'e':
case 'f':
case 'g':
case 'h':
case 'i':
case 'j':
case 'k':
case 'l':
case 'm':
case 'n':
case 'o':
case 'p':
case 'q':
case 'r':
case 's':
case 't':
case 'u':
case 'v':
case 'w':
case 'x':
case 'y':
case 'z':
case '|':
case '~':
name_type = 0;
try_number = false;
do_name:
da.base = (byte *) sptr;
da.is_dynamic = false;
{
const byte *endp1 = endptr - 1;
do {
if (sptr >= endp1)
goto dyn_name;
}
while (decoder[*++sptr] <= max_name_ctype);
}
daptr = (byte *) sptr;
c = *sptr;
goto nx;
dyn_name:
scan_end_inline();
da.limit = (byte *)++ sptr;
da.memory = name_memory(imemory);
retcode = dynamic_grow(&da, da.limit, name_max_string);
if (retcode < 0) {
dynamic_save(&da);
if (retcode != e_VMerror)
sreturn(retcode);
scan_type = scanning_name;
goto pause_ret;
}
daptr = da.next;
cont_name:scan_begin_inline();
while (decoder[c = scan_getc()] <= max_name_ctype) {
if (daptr == da.limit) {
retcode = dynamic_grow(&da, daptr,
name_max_string);
if (retcode < 0) {
dynamic_save(&da);
if (retcode != e_VMerror)
sreturn(retcode);
scan_putback();
scan_type = scanning_name;
goto pause_ret;
}
daptr = da.next;
}
*daptr++ = c;
}
nx:switch (decoder[c]) {
case ctype_btoken:
case ctype_other:
scan_putback();
break;
case ctype_space:
if (c == char_CR) {
if (sptr >= endptr) {
if (s->end_status != EOFC) {
sptr--;
goto pause_name;
}
} else if (sptr[1] == char_EOL)
sptr++;
}
break;
case ctype_exception:
switch (c) {
case INTC:
case CALLC:
goto pause_name;
case ERRC:
sreturn(e_ioerror);
case EOFC:
break;
}
}
if (try_number) {
const byte *base = da.base;
scan_sign(sign, base);
retcode = scan_number(base, daptr, sign, myref, &newptr, PDFScanInvNum);
if (retcode == 1) {
ref_mark_new(myref);
retcode = 0;
} else if (retcode != e_syntaxerror) {
dynamic_free(&da);
if (name_type == 2)
sreturn(e_syntaxerror);
break;
}
}
if (da.is_dynamic) {
uint size = daptr - da.base;
retcode = name_ref(imemory, da.base, size, myref, -1);
if (retcode >= 0) {
dynamic_free(&da);
} else {
retcode = dynamic_resize(&da, size);
if (retcode < 0) {
if (c != EOFC)
scan_putback();
scan_type = scanning_name;
goto pause_ret;
}
retcode = name_ref(imemory, da.base, size, myref, 2);
}
} else {
retcode = name_ref(imemory, da.base, (uint) (daptr - da.base),
myref, !s->foreign);
}
if (retcode < 0) {
if (retcode != e_VMerror)
sreturn(retcode);
if (!da.is_dynamic) {
da.next = daptr;
dynamic_save(&da);
}
if (c != EOFC)
scan_putback();
scan_type = scanning_name;
goto pause_ret;
}
have_name:switch (name_type) {
case 0:
if (r_has_type(myref, t_name))
r_set_attrs(myref, a_executable);
case 1:
break;
case 2:
{
ref *pvalue;
if (!r_has_type(myref, t_name))
sreturn(e_undefined);
if ((pvalue = dict_find_name(myref)) == 0)
sreturn(e_undefined);
if (pstack != 0 &&
r_space(pvalue) > ialloc_space(idmemory)
)
sreturn(e_invalidaccess);
ref_assign_new(myref, pvalue);
}
}
}
sret:if (retcode < 0) {
scan_end_inline();
if (pstack != 0) {
if (retcode == e_undefined)
*pref = *osp;
ref_stack_pop(&o_stack,
ref_stack_count(&o_stack) - (pdepth - 1));
}
return retcode;
}
if (pstack == 0) {
scan_end_inline();
return retcode;
}
snext:if_not_spush1() {
scan_end_inline();
scan_type = scanning_none;
goto save;
}
myref = osp;
goto top;
pause_name:
da.next = daptr;
dynamic_save(&da);
scan_type = scanning_name;
pause:
retcode = scan_Refill;
pause_ret:
scan_end_inline();
suspend:
if (pstack != 0)
osp--;
save:
*pstate = sstate;
return retcode;
comment:
if (retcode < 0)
goto sret;
scan_end_inline();
scan_type = scanning_none;
goto save;
}