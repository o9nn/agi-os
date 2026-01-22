#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsutil.h"
#include "gdevpdfx.h"
#include "gdevpdfo.h"
#include "scanchar.h"
#include "strimpl.h"
#include "sstring.h"
#ifndef gs_error_syntaxerror
#  define gs_error_syntaxerror gs_error_rangecheck
#endif
bool
pdf_objname_is_valid(const byte *data, uint size)
{
return (size >= 2 && data[0] == '{' &&
(const byte *)memchr(data, '}', size) == data + size - 1);
}
int
pdf_find_named(gx_device_pdf * pdev, const gs_param_string * pname,
cos_object_t **ppco)
{
const cos_value_t *pvalue;
if (!pdf_objname_is_valid(pname->data, pname->size))
return_error(gs_error_rangecheck);
if ((pvalue = cos_dict_find(pdev->local_named_objects, pname->data,
pname->size)) != 0 ||
(pvalue = cos_dict_find(pdev->global_named_objects, pname->data,
pname->size)) != 0
) {
*ppco = pvalue->contents.object;
return 0;
}
return_error(gs_error_undefined);
}
int
pdf_create_named(gx_device_pdf *pdev, const gs_param_string *pname,
cos_type_t cotype, cos_object_t **ppco, long id)
{
cos_object_t *pco;
cos_value_t value;
*ppco = pco = cos_object_alloc(pdev, "pdf_create_named");
if (pco == 0)
return_error(gs_error_VMerror);
pco->id =
(id == -1 ? 0L : id == 0 ? pdf_obj_ref(pdev) : id);
if (pname) {
int code = cos_dict_put(pdev->local_named_objects, pname->data,
pname->size, cos_object_value(&value, pco));
if (code < 0)
return code;
}
if (cotype != cos_type_generic)
cos_become(pco, cotype);
*ppco = pco;
return 0;
}
int
pdf_create_named_dict(gx_device_pdf *pdev, const gs_param_string *pname,
cos_dict_t **ppcd, long id)
{
cos_object_t *pco;
int code = pdf_create_named(pdev, pname, cos_type_dict, &pco, id);
*ppcd = (cos_dict_t *)pco;
return code;
}
int
pdf_refer_named(gx_device_pdf * pdev, const gs_param_string * pname_orig,
cos_object_t **ppco)
{
const gs_param_string *pname = pname_orig;
int code = pdf_find_named(pdev, pname, ppco);
char page_name_chars[6 + 10 + 2];
gs_param_string pnstr;
int page_number;
if (code != gs_error_undefined)
return code;
if (pname->size >= 7 &&
sscanf((const char *)pname->data, "{Page%d}", &page_number) == 1
)
goto cpage;
if (pdf_key_eq(pname, "{ThisPage}"))
page_number = pdev->next_page + 1;
else if (pdf_key_eq(pname, "{NextPage}"))
page_number = pdev->next_page + 2;
else if (pdf_key_eq(pname, "{PrevPage}"))
page_number = pdev->next_page;
else {
code = pdf_create_named(pdev, pname, cos_type_generic, ppco, 0L);
return (code < 0 ? code : 1);
}
if (page_number <= 0)
return code;
sprintf(page_name_chars, "{Page%d}", page_number);
param_string_from_string(pnstr, page_name_chars);
pname = &pnstr;
code = pdf_find_named(pdev, pname, ppco);
if (code != gs_error_undefined)
return code;
cpage:
if (pdf_page_id(pdev, page_number) <= 0)
return_error(gs_error_rangecheck);
*ppco = COS_OBJECT(pdev->pages[page_number - 1].Page);
return 0;
}
int
pdf_make_named(gx_device_pdf * pdev, const gs_param_string * pname,
cos_type_t cotype, cos_object_t **ppco, bool assign_id)
{
if (pname) {
int code = pdf_refer_named(pdev, pname, ppco);
cos_object_t *pco = *ppco;
if (code < 0)
return code;
if (cos_type(pco) != cos_type_generic)
return_error(gs_error_rangecheck);
if (assign_id && pco->id == 0)
pco->id = pdf_obj_ref(pdev);
cos_become(pco, cotype);
return code;
} else {
int code = pdf_create_named(pdev, pname, cotype, ppco,
(assign_id ? 0L : -1L));
return (code < 0 ? code : 1);
}
}
int
pdf_make_named_dict(gx_device_pdf * pdev, const gs_param_string * pname,
cos_dict_t **ppcd, bool assign_id)
{
cos_object_t *pco;
int code = pdf_make_named(pdev, pname, cos_type_dict, &pco, assign_id);
*ppcd = (cos_dict_t *)pco;
return code;
}
int
pdf_get_named(gx_device_pdf * pdev, const gs_param_string * pname,
cos_type_t cotype, cos_object_t **ppco)
{
int code = pdf_refer_named(pdev, pname, ppco);
if (code < 0)
return code;
if (cos_type(*ppco) != cotype)
return_error(gs_error_typecheck);
return code;
}
int
pdf_push_namespace(gx_device_pdf *pdev)
{
int code = cos_array_add_object(pdev->Namespace_stack,
COS_OBJECT(pdev->local_named_objects));
cos_dict_t *pcd =
cos_dict_alloc(pdev, "pdf_push_namespace(local_named_objects)");
cos_array_t *pca =
cos_array_alloc(pdev, "pdf_push_namespace(NI_stack)");
if (code < 0 ||
(code = cos_array_add_object(pdev->Namespace_stack,
COS_OBJECT(pdev->NI_stack))) < 0
)
return code;
if (pcd == 0 || pca == 0)
return_error(gs_error_VMerror);
pdev->local_named_objects = pcd;
pdev->NI_stack = pca;
return 0;
}
int
pdf_pop_namespace(gx_device_pdf *pdev)
{
cos_value_t nis_value, lno_value;
int code = cos_array_unadd(pdev->Namespace_stack, &nis_value);
if (code < 0 ||
(code = cos_array_unadd(pdev->Namespace_stack, &lno_value)) < 0
)
return code;
COS_FREE(pdev->local_named_objects,
"pdf_pop_namespace(local_named_objects)");
pdev->local_named_objects = (cos_dict_t *)lno_value.contents.object;
COS_FREE(pdev->NI_stack, "pdf_pop_namespace(NI_stack)");
pdev->NI_stack = (cos_array_t *)nis_value.contents.object;
return 0;
}
int
pdf_scan_token(const byte **pscan, const byte * end, const byte **ptoken)
{
const byte *p = *pscan;
while (p < end && scan_char_decoder[*p] == ctype_space) {
++p;
if (p[-1] == 0 && p + 1 < end && *p == 0 && p[1] == '/') {
*ptoken = ++p;
while (*p != 0)
if (++p >= end)
return_error(gs_error_syntaxerror);
*pscan = p;
return 1;
}
}
*ptoken = p;
if (p >= end) {
*pscan = p;
return 0;
}
switch (*p) {
case '%':
case ')':
return_error(gs_error_syntaxerror);
case '(': {
byte buf[50];
stream_cursor_read r;
stream_cursor_write w;
stream_PSSD_state ss;
int status;
s_PSSD_init((stream_state *)&ss);
r.ptr = p;
r.limit = end - 1;
w.limit = buf + sizeof(buf) - 1;
do {
w.ptr = buf;  w.ptr--;
status = (*s_PSSD_template.process)
((stream_state *) & ss, &r, &w, true);
}
while (status == 1);
*pscan = r.ptr + 1;
return 1;
}
case '<':
if (end - p < 2)
return_error(gs_error_syntaxerror);
if (p[1] != '<') {
p = (const byte *)memchr(p + 1, '>', end - p - 1);
if (p == 0)
return_error(gs_error_syntaxerror);
}
goto m2;
case '>':
if (end - p < 2 || p[1] != '>')
return_error(gs_error_syntaxerror);
m2:	*pscan = p + 2;
return 1;
case '[': case ']': case '{': case '}':
*pscan = p + 1;
return 1;
case '/':
++p;
default:
break;
}
while (p < end && scan_char_decoder[*p] <= ctype_name)
++p;
*pscan = p;
if (p == *ptoken)
return_error(gs_error_syntaxerror);
return 1;
}
int
pdf_scan_token_composite(const byte **pscan, const byte * end,
const byte **ptoken_orig)
{
int level = 0;
const byte *ignore_token;
const byte **ptoken = ptoken_orig;
int code;
do {
code = pdf_scan_token(pscan, end, ptoken);
if (code <= 0)
return (code < 0 || level == 0 ? code :
gs_note_error(gs_error_syntaxerror));
switch (**ptoken) {
case '<': case '[': case '{':
++level; break;
case '>': case ']': case '}':
if (level == 0)
return_error(gs_error_syntaxerror);
--level; break;
}
ptoken = &ignore_token;
} while (level);
return code;
}
private const byte *
pdfmark_next_object(const byte * scan, const byte * end, const byte **pname,
cos_object_t **ppco, gx_device_pdf * pdev)
{
int code;
while ((code = pdf_scan_token(&scan, end, pname)) != 0) {
gs_param_string sname;
if (code < 0) {
++scan;
continue;
}
if (**pname != '{')
continue;
scan = *pname;
code = pdf_scan_token_composite(&scan, end, pname);
if (code < 0) {
++scan;
continue;
}
sname.data = *pname;
sname.size = scan - sname.data;
code = pdf_refer_named(pdev, &sname, ppco);
if (code < 0)
continue;
return scan;
}
*ppco = 0;
return end;
}
int
pdf_replace_names(gx_device_pdf * pdev, const gs_param_string * from,
gs_param_string * to)
{
const byte *start = from->data;
const byte *end = start + from->size;
const byte *scan;
uint size = 0;
cos_object_t *pco;
bool any = false;
byte *sto;
char ref[1 + 10 + 5 + 1];
for (scan = start; scan < end;) {
const byte *sname;
const byte *next =
pdfmark_next_object(scan, end, &sname, &pco, pdev);
size += sname - scan;
if (pco) {
sprintf(ref, " %ld 0 R ", pco->id);
size += strlen(ref);
}
scan = next;
any |= next != sname;
}
to->persistent = true;
if (!any) {
to->data = start;
to->size = size;
return 0;
}
sto = gs_alloc_bytes(pdev->pdf_memory, size, "pdf_replace_names");
if (sto == 0)
return_error(gs_error_VMerror);
to->data = sto;
to->size = size;
for (scan = start; scan < end;) {
const byte *sname;
const byte *next =
pdfmark_next_object(scan, end, &sname, &pco, pdev);
uint copy = sname - scan;
int rlen;
memcpy(sto, scan, copy);
sto += copy;
if (pco) {
sprintf(ref, " %ld 0 R ", pco->id);
rlen = strlen(ref);
memcpy(sto, ref, rlen);
sto += rlen;
}
scan = next;
}
return 0;
}