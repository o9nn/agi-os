#include "math_.h"
#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsutil.h"
#include "gdevpdfx.h"
#include "gdevpdfo.h"
#include "scanchar.h"
#include "szlibx.h"
#include "slzwx.h"
private_st_pdf_article();
#define PDFMARK_NAMEABLE 1
#define PDFMARK_ODD_OK 2
#define PDFMARK_KEEP_NAME 4
#define PDFMARK_NO_REFS 8
#define PDFMARK_TRUECTM 16
typedef struct pdfmark_name_s {
const char *mname;
pdfmark_proc((*proc));
byte options;
} pdfmark_name;
bool
pdf_key_eq(const gs_param_string * pcs, const char *str)
{
return (strlen(str) == pcs->size &&
!strncmp(str, (const char *)pcs->data, pcs->size));
}
int
pdfmark_scan_int(const gs_param_string * pstr, int *pvalue)
{
#define MAX_INT_STR 20
uint size = pstr->size;
char str[MAX_INT_STR + 1];
if (size > MAX_INT_STR)
return_error(gs_error_limitcheck);
memcpy(str, pstr->data, size);
str[size] = 0;
return (sscanf(str, "%d", pvalue) == 1 ? 0 :
gs_note_error(gs_error_rangecheck));
#undef MAX_INT_STR
}
private bool
pdfmark_find_key(const char *key, const gs_param_string * pairs, uint count,
gs_param_string * pstr)
{
uint i;
for (i = 0; i < count; i += 2)
if (pdf_key_eq(&pairs[i], key)) {
*pstr = pairs[i + 1];
return true;
}
pstr->data = 0;
pstr->size = 0;
return false;
}
private int
pdfmark_page_number(gx_device_pdf * pdev, const gs_param_string * pnstr)
{
int page = pdev->next_page + 1;
if (pnstr->data == 0);
else if (pdf_key_eq(pnstr, "/Next"))
++page;
else if (pdf_key_eq(pnstr, "/Prev"))
--page;
else if (pdfmark_scan_int(pnstr, &page) < 0)
page = 0;
if (pdev->max_referred_page < page)
pdev->max_referred_page = page;
return page;
}
private int
pdfmark_make_dest(char dstr[MAX_DEST_STRING], gx_device_pdf * pdev,
const char *Page_key, const char *View_key,
const gs_param_string * pairs, uint count)
{
gs_param_string page_string, view_string;
int present =
pdfmark_find_key(Page_key, pairs, count, &page_string) +
pdfmark_find_key(View_key, pairs, count, &view_string);
int page = pdfmark_page_number(pdev, &page_string);
gs_param_string action;
int len;
if (view_string.size == 0)
param_string_from_string(view_string, "[/XYZ null null null]");
if (page == 0)
strcpy(dstr, "[null ");
else if (pdfmark_find_key("/Action", pairs, count, &action) &&
pdf_key_eq(&action, "/GoToR")
)
sprintf(dstr, "[%d ", page - 1);
else
sprintf(dstr, "[%ld 0 R ", pdf_page_id(pdev, page));
len = strlen(dstr);
if (len + view_string.size > MAX_DEST_STRING)
return_error(gs_error_limitcheck);
if (view_string.data[0] != '[' ||
view_string.data[view_string.size - 1] != ']'
)
return_error(gs_error_rangecheck);
memcpy(dstr + len, view_string.data + 1, view_string.size - 1);
dstr[len + view_string.size - 1] = 0;
return present;
}
private int
pdfmark_coerce_dest(gs_param_string *dstr, char dest[MAX_DEST_STRING])
{
const byte *data = dstr->data;
uint size = dstr->size;
if (size == 0 || data[0] != '(')
return 0;
memcpy(dest, data, size - 1);
dest[0] = '/';
dest[size - 1] = 0;
dstr->data = (byte *)dest;
dstr->size = size - 1;
return 1;
}
private int
pdfmark_put_c_pair(cos_dict_t *pcd, const char *key,
const gs_param_string * pvalue)
{
return cos_dict_put_c_key_string(pcd, key, pvalue->data, pvalue->size);
}
private int
pdfmark_put_pair(cos_dict_t *pcd, const gs_param_string * pair)
{
return cos_dict_put_string(pcd, pair->data, pair->size,
pair[1].data, pair[1].size);
}
private int
pdfmark_scan_rect(gs_rect * prect, const gs_param_string * str,
const gs_matrix * pctm)
{
uint size = str->size;
double v[4];
#define MAX_RECT_STRING 100
char chars[MAX_RECT_STRING + 3];
int end_check;
if (str->size > MAX_RECT_STRING)
return_error(gs_error_limitcheck);
memcpy(chars, str->data, size);
strcpy(chars + size, " 0");
if (sscanf(chars, "[%lg %lg %lg %lg]%d",
&v[0], &v[1], &v[2], &v[3], &end_check) != 5
)
return_error(gs_error_rangecheck);
gs_point_transform(v[0], v[1], pctm, &prect->p);
gs_point_transform(v[2], v[3], pctm, &prect->q);
return 0;
}
private void
pdfmark_make_rect(char str[MAX_RECT_STRING], const gs_rect * prect)
{
stream s;
s_init(&s, NULL);
swrite_string(&s, (byte *)str, MAX_RECT_STRING - 1);
pprintg4(&s, "[%g %g %g %g]",
prect->p.x, prect->p.y, prect->q.x, prect->q.y);
str[stell(&s)] = 0;
}
private int
pdfmark_write_border(stream *s, const gs_param_string *str,
const gs_matrix *pctm)
{
uint size = str->size;
#define MAX_BORDER_STRING 100
char chars[MAX_BORDER_STRING + 1];
double bx, by, c;
gs_point bpt, cpt;
const char *next;
if (str->size > MAX_BORDER_STRING)
return_error(gs_error_limitcheck);
memcpy(chars, str->data, size);
chars[size] = 0;
if (sscanf(chars, "[%lg %lg %lg", &bx, &by, &c) != 3)
return_error(gs_error_rangecheck);
gs_distance_transform(bx, by, pctm, &bpt);
gs_distance_transform(0.0, c, pctm, &cpt);
pprintg3(s, "[%g %g %g", fabs(bpt.x), fabs(bpt.y), fabs(cpt.x + cpt.y));
next = strchr(chars + 1, ']');
if (next == 0)
return_error(gs_error_rangecheck);
if (next[1] != 0) {
double v;
stream_putc(s, '[');
while (next != 0 && sscanf(++next, "%lg", &v) == 1) {
gs_point vpt;
gs_distance_transform(0.0, v, pctm, &vpt);
pprintg1(s, "%g ", fabs(vpt.x + vpt.y));
next = strchr(next, ' ');
}
stream_putc(s, ']');
}
stream_putc(s, ']');
return 0;
}
private int
cos_stream_put_c_strings(cos_stream_t *pcs, const char *key, const char *value)
{
return cos_dict_put_c_strings(cos_stream_dict(pcs), key, value);
}
private int
setup_pdfmark_stream_compression(gx_device_psdf *pdev0,
cos_stream_t *pco)
{
gx_device_pdf *pdev = (gx_device_pdf *)pdev0;
gs_memory_t *mem = pdev->pdf_memory;
static const pdf_filter_names_t fnames = {
PDF_FILTER_NAMES
};
const stream_template *template =
(pdev->params.UseFlateCompression &&
pdev->version >= psdf_version_ll3 ?
&s_zlibE_template : &s_LZWE_template);
stream_state *st;
pco->input_strm = cos_write_stream_alloc(pco, pdev,
"setup_pdfmark_stream_compression");
if (pco->input_strm == 0)
return_error(gs_error_VMerror);
if (!pdev->binary_ok) {
stream_state *ss = s_alloc_state(mem, s_A85E_template.stype,
"setup_pdfmark_stream_compression");
if (ss == 0)
return_error(gs_error_VMerror);
if (s_add_filter(&pco->input_strm, &s_A85E_template, ss, mem) == 0) {
gs_free_object(mem, ss, "setup_image_compression");
return_error(gs_error_VMerror);
}
}
st = s_alloc_state(mem, template->stype,
"setup_pdfmark_stream_compression");
if (st == 0)
return_error(gs_error_VMerror);
if (template->set_defaults)
(*template->set_defaults) (st);
if (s_add_filter(&pco->input_strm, template, st, mem) == 0) {
gs_free_object(mem, st, "setup_image_compression");
return_error(gs_error_VMerror);
}
return pdf_put_filters(cos_stream_dict(pco), pdev, pco->input_strm, &fnames);
}
typedef struct ao_params_s {
gx_device_pdf *pdev;
const char *subtype;
long src_pg;
} ao_params_t;
private int
pdfmark_put_ao_pairs(gx_device_pdf * pdev, cos_dict_t *pcd,
const gs_param_string * pairs, uint count,
const gs_matrix * pctm, ao_params_t * params,
bool for_outline)
{
const gs_param_string *Action = 0;
const gs_param_string *File = 0;
gs_param_string Dest;
gs_param_string Subtype;
uint i;
int code;
char dest[MAX_DEST_STRING];
bool coerce_dest = false;
Dest.data = 0;
if (params->subtype)
param_string_from_string(Subtype, params->subtype);
else
Subtype.data = 0;
for (i = 0; i < count; i += 2) {
const gs_param_string *pair = &pairs[i];
long src_pg;
if (pdf_key_eq(pair, "/SrcPg") &&
sscanf((const char *)pair[1].data, "%ld", &src_pg) == 1
)
params->src_pg = src_pg - 1;
else if (!for_outline && pdf_key_eq(pair, "/Color"))
pdfmark_put_c_pair(pcd, "/C", pair + 1);
else if (!for_outline && pdf_key_eq(pair, "/Title"))
pdfmark_put_c_pair(pcd, "/T", pair + 1);
else if (pdf_key_eq(pair, "/Action") || pdf_key_eq(pair, "/A"))
Action = pair;
else if (pdf_key_eq(pair, "/File") || pdf_key_eq(pair, "/F"))
File = pair;
else if (pdf_key_eq(pair, "/Dest")) {
Dest = pair[1];
coerce_dest = true;
}
else if (pdf_key_eq(pair, "/Page") || pdf_key_eq(pair, "/View")) {
if (Dest.data == 0) {
code = pdfmark_make_dest(dest, params->pdev, "/Page", "/View",
pairs, count);
if (code < 0)
return code;
param_string_from_string(Dest, dest);
if (for_outline)
coerce_dest = false;
}
} else if (pdf_key_eq(pair, "/Subtype"))
Subtype = pair[1];
else if (pdf_key_eq(pair, "/Contents")) {
byte *cstr;
uint csize = pair[1].size;
cos_value_t *pcv;
uint i, j;
pdfmark_put_pair(pcd, pair);
pcv = (cos_value_t *)cos_dict_find_c_key(pcd, "/Contents");
cstr = pcv->contents.chars.data;
for (i = j = 0; i < csize;)
if (csize - i >= 2 && !memcmp(cstr + i, "\\n", 2) &&
(i == 0 || cstr[i - 1] != '\\')
) {
cstr[j] = '\\', cstr[j + 1] = 'r';
i += 2, j += 2;
} else if (csize - i >= 4 && !memcmp(cstr + i, "\\012", 4) &&
(i == 0 || cstr[i - 1] != '\\')
) {
cstr[j] = '\\', cstr[j + 1] = 'r';
i += 4, j += 2;
} else
cstr[j++] = cstr[i++];
if (j != i)
pcv->contents.chars.data =
gs_resize_string(pdev->pdf_memory, cstr, csize, j,
"pdfmark_put_ao_pairs");
} else if (pdf_key_eq(pair, "/Rect")) {
gs_rect rect;
char rstr[MAX_RECT_STRING];
int code = pdfmark_scan_rect(&rect, pair + 1, pctm);
if (code < 0)
return code;
pdfmark_make_rect(rstr, &rect);
cos_dict_put_c_key_string(pcd, "/Rect", (byte *)rstr,
strlen(rstr));
} else if (pdf_key_eq(pair, "/Border")) {
stream s;
char bstr[MAX_BORDER_STRING + 1];
int code;
s_init(&s, NULL);
swrite_string(&s, (byte *)bstr, MAX_BORDER_STRING + 1);
code = pdfmark_write_border(&s, pair + 1, pctm);
if (code < 0)
return code;
if (stell(&s) > MAX_BORDER_STRING)
return_error(gs_error_limitcheck);
bstr[stell(&s)] = 0;
cos_dict_put_c_key_string(pcd, "/Border", (byte *)bstr,
strlen(bstr));
} else if (for_outline && pdf_key_eq(pair, "/Count"))
DO_NOTHING;
else
pdfmark_put_pair(pcd, pair);
}
if (!for_outline && pdf_key_eq(&Subtype, "/Link")) {
if (Action) {
if (pdf_key_eq(Action + 1, "/GoTo") ||
(File && pdf_key_eq(Action + 1, "/GoToR"))
)
DO_NOTHING;
else
Dest.data = 0;
}
}
if (Action) {
const byte *astr = Action[1].data;
const uint asize = Action[1].size;
if ((File != 0 || Dest.data != 0) &&
(pdf_key_eq(Action + 1, "/Launch") ||
(pdf_key_eq(Action + 1, "/GoToR") && File) ||
pdf_key_eq(Action + 1, "/Article"))
) {
cos_dict_t *adict = cos_dict_alloc(pdev, "action dict");
cos_value_t avalue;
if (adict == 0)
return_error(gs_error_VMerror);
if (!for_outline) {
cos_dict_put_c_strings(adict, "/Type", "/Action");
}
if (pdf_key_eq(Action + 1, "/Article")) {
cos_dict_put_c_strings(adict, "/S", "/Thread");
coerce_dest = false;
}
else
pdfmark_put_c_pair(adict, "/S", Action + 1);
if (Dest.data) {
if (coerce_dest)
pdfmark_coerce_dest(&Dest, dest);
pdfmark_put_c_pair(adict, "/D", &Dest);
Dest.data = 0;
}
if (File) {
pdfmark_put_c_pair(adict, "/F", File + 1);
File = 0;
}
cos_dict_put(pcd, (const byte *)"/A", 2,
COS_OBJECT_VALUE(&avalue, adict));
} else if (asize >= 4 && !memcmp(astr, "<<", 2)) {
const byte *scan = astr + 2;
const byte *end = astr + asize;
gs_param_string key, value;
cos_dict_t *adict = cos_dict_alloc(pdev, "action dict");
cos_value_t avalue;
int code;
if (adict == 0)
return_error(gs_error_VMerror);
while ((code = pdf_scan_token(&scan, end, &key.data)) > 0) {
key.size = scan - key.data;
if (key.data[0] != '/' ||
(code = pdf_scan_token_composite(&scan, end, &value.data)) != 1)
break;
value.size = scan - value.data;
if (pdf_key_eq(&key, "/Dest") || pdf_key_eq(&key, "/D")) {
param_string_from_string(key, "/D");
if (value.data[0] == '(') {
pdfmark_coerce_dest(&value, dest);
}
} else if (pdf_key_eq(&key, "/File"))
param_string_from_string(key, "/F");
else if (pdf_key_eq(&key, "/Subtype"))
param_string_from_string(key, "/S");
cos_dict_put_string(adict, key.data, key.size,
value.data, value.size);
}
if (code <= 0 || !pdf_key_eq(&key, ">>"))
return_error(gs_error_rangecheck);
cos_dict_put(pcd, (const byte *)"/A", 2,
COS_OBJECT_VALUE(&avalue, adict));
} else if (pdf_key_eq(Action + 1, "/GoTo"))
pdfmark_put_pair(pcd, Action);
else if (Action[1].size < 30) {
char buf[30];
int d0, d1;
memcpy(buf, Action[1].data, Action[1].size);
buf[Action[1].size] = 0;
if (sscanf(buf, "%d %d R", &d0, &d1) == 2)
pdfmark_put_pair(pcd, Action);
}
}
if (Dest.data) {
if (coerce_dest)
pdfmark_coerce_dest(&Dest, dest);
pdfmark_put_c_pair(pcd, "/Dest", &Dest);
} else if (for_outline && !Action) {
char dstr[1 + (sizeof(long) * 8 / 3 + 1) + 25 + 1];
long page_id = pdf_page_id(pdev, pdev->next_page + 1);
sprintf(dstr, "[%ld 0 R /XYZ null null null]", page_id);
cos_dict_put_c_key_string(pcd, "/Dest", (const unsigned char*) dstr,
strlen(dstr));
}
if (File)
pdfmark_put_pair(pcd, File);
if (Subtype.data)
pdfmark_put_c_pair(pcd, "/Subtype", &Subtype);
return 0;
}
private int
pdfmark_annot(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string *objname,
const char *subtype)
{
ao_params_t params;
cos_dict_t *pcd;
int page_index = pdev->next_page;
cos_array_t *annots;
cos_value_t value;
int code;
params.pdev = pdev;
params.subtype = subtype;
params.src_pg = -1;
code = pdf_make_named_dict(pdev, objname, &pcd, true);
if (code < 0)
return code;
code = cos_dict_put_c_strings(pcd, "/Type", "/Annot");
if (code < 0)
return code;
code = pdfmark_put_ao_pairs(pdev, pcd, pairs, count, pctm, &params, false);
if (code < 0)
return code;
if (params.src_pg >= 0)
page_index = params.src_pg;
if (pdf_page_id(pdev, page_index + 1) <= 0)
return_error(gs_error_rangecheck);
annots = pdev->pages[page_index].Annots;
if (annots == 0) {
annots = cos_array_alloc(pdev, "pdfmark_annot");
if (annots == 0)
return_error(gs_error_VMerror);
pdev->pages[page_index].Annots = annots;
}
if (!objname) {
COS_WRITE_OBJECT(pcd, pdev);
COS_RELEASE(pcd, "pdfmark_annot");
}
return cos_array_add(annots,
cos_object_value(&value, COS_OBJECT(pcd)));
}
private int
pdfmark_ANN(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * objname)
{
return pdfmark_annot(pdev, pairs, count, pctm, objname, "/Text");
}
private int
pdfmark_LNK(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * objname)
{
return pdfmark_annot(pdev, pairs, count, pctm, objname, "/Link");
}
private int
pdfmark_write_outline(gx_device_pdf * pdev, pdf_outline_node_t * pnode,
long next_id)
{
stream *s;
pdf_open_separate(pdev, pnode->id);
pnode->action->id = pnode->id;
s = pdev->strm;
stream_puts(s, "<< ");
cos_dict_elements_write(pnode->action, pdev);
if (pnode->count)
pprintd1(s, "/Count %d ", pnode->count);
pprintld1(s, "/Parent %ld 0 R\n", pnode->parent_id);
if (pnode->prev_id)
pprintld1(s, "/Prev %ld 0 R\n", pnode->prev_id);
if (next_id)
pprintld1(s, "/Next %ld 0 R\n", next_id);
if (pnode->first_id)
pprintld2(s, "/First %ld 0 R /Last %ld 0 R\n",
pnode->first_id, pnode->last_id);
stream_puts(s, ">>\n");
pdf_end_separate(pdev);
COS_FREE(pnode->action, "pdfmark_write_outline");
pnode->action = 0;
return 0;
}
private void
pdfmark_adjust_parent_count(pdf_outline_level_t * plevel)
{
pdf_outline_level_t *parent = plevel - 1;
int count = plevel->last.count;
if (count > 0) {
if (parent->last.count < 0)
parent->last.count -= count;
else
parent->last.count += count;
}
}
int
pdfmark_close_outline(gx_device_pdf * pdev)
{
int depth = pdev->outline_depth;
pdf_outline_level_t *plevel = &pdev->outline_levels[depth];
int code;
if (plevel->last.id) {
code = pdfmark_write_outline(pdev, &plevel->last, 0);
if (code < 0)
return code;
}
if (depth > 0) {
plevel[-1].last.last_id = plevel->last.id;
pdfmark_adjust_parent_count(plevel);
--plevel;
if (plevel->last.count < 0)
pdev->closed_outline_depth--;
pdev->outline_depth--;
}
return 0;
}
private int
pdfmark_OUT(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
int depth = pdev->outline_depth;
pdf_outline_level_t *plevel = &pdev->outline_levels[depth];
int sub_count = 0;
uint i;
pdf_outline_node_t node;
ao_params_t ao;
int code;
for (i = 0; i < count; i += 2) {
const gs_param_string *pair = &pairs[i];
if (pdf_key_eq(pair, "/Count"))
pdfmark_scan_int(pair + 1, &sub_count);
}
if (sub_count != 0 && depth == MAX_OUTLINE_DEPTH - 1)
return_error(gs_error_limitcheck);
node.action = cos_dict_alloc(pdev, "pdfmark_OUT");
if (node.action == 0)
return_error(gs_error_VMerror);
ao.pdev = pdev;
ao.subtype = 0;
ao.src_pg = -1;
code = pdfmark_put_ao_pairs(pdev, node.action, pairs, count, pctm, &ao,
true);
if (code < 0)
return code;
if (pdev->outlines_id == 0)
pdev->outlines_id = pdf_obj_ref(pdev);
node.id = pdf_obj_ref(pdev);
node.parent_id =
(depth == 0 ? pdev->outlines_id : plevel[-1].last.id);
node.prev_id = plevel->last.id;
node.first_id = node.last_id = 0;
node.count = sub_count;
if (plevel->first.id == 0) {
if (depth > 0)
plevel[-1].last.first_id = node.id;
node.prev_id = 0;
plevel->first = node;
plevel->first.action = 0;
} else {
if (depth > 0)
pdfmark_adjust_parent_count(plevel);
pdfmark_write_outline(pdev, &plevel->last, node.id);
}
plevel->last = node;
plevel->left--;
if (!pdev->closed_outline_depth)
pdev->outlines_open++;
if (sub_count != 0) {
pdev->outline_depth++;
++plevel;
plevel->left = (sub_count > 0 ? sub_count : -sub_count);
plevel->first.id = 0;
plevel->first.action = plevel->last.action = 0;
if (sub_count < 0)
pdev->closed_outline_depth++;
} else {
while ((depth = pdev->outline_depth) > 0 &&
pdev->outline_levels[depth].left == 0
)
pdfmark_close_outline(pdev);
}
return 0;
}
private int
pdfmark_write_bead(gx_device_pdf * pdev, const pdf_bead_t * pbead)
{
stream *s;
char rstr[MAX_RECT_STRING];
pdf_open_separate(pdev, pbead->id);
s = pdev->strm;
pprintld3(s, "<</T %ld 0 R/V %ld 0 R/N %ld 0 R",
pbead->article_id, pbead->prev_id, pbead->next_id);
if (pbead->page_id != 0)
pprintld1(s, "/P %ld 0 R", pbead->page_id);
pdfmark_make_rect(rstr, &pbead->rect);
pprints1(s, "/R%s>>\n", rstr);
return pdf_end_separate(pdev);
}
int
pdfmark_write_article(gx_device_pdf * pdev, const pdf_article_t * part)
{
pdf_article_t art;
stream *s;
art = *part;
if (art.last.id == 0) {
art.first.prev_id = art.first.next_id = art.first.id;
} else {
art.first.prev_id = art.last.id;
art.last.next_id = art.first.id;
pdfmark_write_bead(pdev, &art.last);
}
pdfmark_write_bead(pdev, &art.first);
pdf_open_separate(pdev, art.contents->id);
s = pdev->strm;
pprintld1(s, "<</F %ld 0 R/I<<", art.first.id);
cos_dict_elements_write(art.contents, pdev);
stream_puts(s, ">> >>\n");
return pdf_end_separate(pdev);
}
private int
pdfmark_ARTICLE(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
gs_memory_t *mem = pdev->pdf_memory;
gs_param_string title;
gs_param_string rectstr;
gs_rect rect;
long bead_id;
pdf_article_t *part;
int code;
if (!pdfmark_find_key("/Title", pairs, count, &title) ||
!pdfmark_find_key("/Rect", pairs, count, &rectstr)
)
return_error(gs_error_rangecheck);
if ((code = pdfmark_scan_rect(&rect, &rectstr, pctm)) < 0)
return code;
bead_id = pdf_obj_ref(pdev);
for (part = pdev->articles; part != 0; part = part->next) {
const cos_value_t *a_title =
cos_dict_find_c_key(part->contents, "/Title");
if (a_title != 0 && !COS_VALUE_IS_OBJECT(a_title) &&
!bytes_compare(a_title->contents.chars.data,
a_title->contents.chars.size,
title.data, title.size))
break;
}
if (part == 0) {
cos_dict_t *contents =
cos_dict_alloc(pdev, "pdfmark_ARTICLE(contents)");
if (contents == 0)
return_error(gs_error_VMerror);
part = gs_alloc_struct(mem, pdf_article_t, &st_pdf_article,
"pdfmark_ARTICLE(article)");
if (part == 0 || contents == 0) {
gs_free_object(mem, part, "pdfmark_ARTICLE(article)");
if (contents)
COS_FREE(contents, "pdfmark_ARTICLE(contents)");
return_error(gs_error_VMerror);
}
contents->id = pdf_obj_ref(pdev);
part->next = pdev->articles;
pdev->articles = part;
cos_dict_put_string(contents, (const byte *)"/Title", 6,
title.data, title.size);
part->first.id = part->last.id = 0;
part->contents = contents;
}
if (part->last.id == 0) {
part->first.next_id = bead_id;
part->last.id = part->first.id;
} else {
part->last.next_id = bead_id;
pdfmark_write_bead(pdev, &part->last);
}
part->last.prev_id = part->last.id;
part->last.id = bead_id;
part->last.article_id = part->contents->id;
part->last.next_id = 0;
part->last.rect = rect;
{
gs_param_string page_string;
int page = 0;
uint i;
pdfmark_find_key("/Page", pairs, count, &page_string);
page = pdfmark_page_number(pdev, &page_string);
part->last.page_id = pdf_page_id(pdev, page);
for (i = 0; i < count; i += 2) {
if (pdf_key_eq(&pairs[i], "/Rect") || pdf_key_eq(&pairs[i], "/Page"))
continue;
pdfmark_put_pair(part->contents, &pairs[i]);
}
}
if (part->first.id == 0) {
part->first = part->last;
part->last.id = 0;
}
return 0;
}
private int
pdfmark_DEST(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * objname)
{
int present;
char dest[MAX_DEST_STRING];
gs_param_string key;
cos_value_t value;
if (!pdfmark_find_key("/Dest", pairs, count, &key) ||
(present =
pdfmark_make_dest(dest, pdev, "/Page", "/View", pairs, count)) < 0
)
return_error(gs_error_rangecheck);
cos_string_value(&value, (byte *)dest, strlen(dest));
if (!pdev->Dests) {
pdev->Dests = cos_dict_alloc(pdev, "pdfmark_DEST(Dests)");
if (pdev->Dests == 0)
return_error(gs_error_VMerror);
pdev->Dests->id = pdf_obj_ref(pdev);
}
if (objname || count > (present + 1) * 2) {
cos_dict_t *ddict;
int i, code;
code = pdf_make_named_dict(pdev, objname, &ddict, false);
if (code < 0)
return code;
code = cos_dict_put_c_key_string(ddict, "/D", (byte *)dest,
strlen(dest));
for (i = 0; code >= 0 && i < count; i += 2)
if (!pdf_key_eq(&pairs[i], "/Dest") &&
!pdf_key_eq(&pairs[i], "/Page") &&
!pdf_key_eq(&pairs[i], "/View")
)
code = pdfmark_put_pair(ddict, &pairs[i]);
if (code < 0)
return code;
COS_OBJECT_VALUE(&value, ddict);
}
return cos_dict_put(pdev->Dests, key.data, key.size, &value);
}
private bool
ps_source_ok(const gs_param_string * psource)
{
if (psource->size >= 2 && psource->data[0] == '(' &&
psource->data[psource->size - 1] == ')'
)
return true;
else {
int i;
lprintf("bad PS passthrough: ");
for (i=0; i<psource->size; i++)
errprintf("%c", psource->data[i]);
errprintf("\n");
return false;
}
}
private uint
pdfmark_write_ps(stream *s, const gs_param_string * psource)
{
uint size = psource->size - 2;
stream_write(s, psource->data + 1, size);
stream_putc(s, '\n');
return size + 1;
}
private int
start_XObject(gx_device_pdf * pdev, bool compress, cos_stream_t **ppcs)
{ pdf_resource_t *pres;
cos_stream_t *pcs;
int code;
code = pdf_open_page(pdev, PDF_IN_STREAM);
if (code < 0)
return code;
code = pdf_enter_substream(pdev, resourceXObject, gs_no_id, &pres, false,
pdev->CompressFonts );
if (code < 0)
return code;
pdev->accumulating_a_global_object = true;
pcs = (cos_stream_t *)pres->object;
pdev->substream_Resources = cos_dict_alloc(pdev, "start_XObject");
if (!pdev->substream_Resources)
return_error(gs_error_VMerror);
if (pdev->ForOPDFRead) {
code = cos_dict_put_c_key_bool((cos_dict_t *)pres->object, "/.Global", true);
if (code < 0)
return code;
}
pres->named = true;
pres->where_used = 0;
pcs->pres = pres;
*ppcs = pcs;
return 0;
}
#define MAX_PS_INLINE 100
private int
pdfmark_PS(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * objname)
{
gs_param_string source;
gs_param_string level1;
if (!pdfmark_find_key("/DataSource", pairs, count, &source) ||
!ps_source_ok(&source) ||
(pdfmark_find_key("/Level1", pairs, count, &level1) &&
!ps_source_ok(&level1))
)
return_error(gs_error_rangecheck);
if (level1.data == 0 && source.size <= MAX_PS_INLINE && objname == 0) {
int code = pdf_open_contents(pdev, PDF_IN_STREAM);
stream *s;
if (code < 0)
return code;
s = pdev->strm;
stream_write(s, source.data, source.size);
stream_puts(s, " PS\n");
} else {
cos_stream_t *pcs;
int code;
gs_id level1_id = gs_no_id;
pdf_resource_t *pres;
cos_value_t value;
if (level1.data != 0) {
pdf_resource_t *pres;
code = pdf_enter_substream(pdev,
resourceXObject,
gs_no_id, &pres, true,
pdev->CompressFonts );
if (code < 0)
return code;
pcs = (cos_stream_t *)pres->object;
if (pdev->ForOPDFRead && objname != 0) {
code = cos_dict_put_c_key_bool((cos_dict_t *)pres->object, "/.Global", true);
if (code < 0)
return code;
}
pres->named = (objname != 0);
pres->where_used = 0;
pcs->pres = pres;
DISCARD(pdfmark_write_ps(pdev->strm, &level1));
code = pdf_exit_substream(pdev);
if (code < 0)
return code;
code = cos_write_object(pres->object, pdev);
if (code < 0)
return code;
level1_id = pres->object->id;
}
code = start_XObject(pdev, pdev->params.CompressPages, &pcs);
if (code < 0)
return code;
pres = pdev->accumulating_substream_resource;
code = cos_stream_put_c_strings(pcs, "/Type", "/XObject");
if (code < 0)
return code;
code = cos_stream_put_c_strings(pcs, "/Subtype", "/PS");
if (code < 0)
return code;
if (level1_id != gs_no_id) {
char r[MAX_DEST_STRING];
sprintf(r, "%ld 0 R", level1_id);
code = cos_dict_put_c_key_string(cos_stream_dict(pcs), "/Level1",
(byte *)r, strlen(r));
if (code < 0)
return code;
}
DISCARD(pdfmark_write_ps(pdev->strm, &source));
code = pdf_exit_substream(pdev);
if (code < 0)
return code;
code = pdf_substitute_resource(pdev, &pres, resourceXObject, NULL, false);
if (code < 0)
return code;
if (objname != 0) {
code = cos_dict_put(pdev->local_named_objects, objname->data,
objname->size, cos_object_value(&value, (cos_object_t *)pcs));
if (code < 0)
return code;
}
code = pdf_open_contents(pdev, PDF_IN_STREAM);
if (code < 0)
return code;
pcs->pres->where_used |= pdev->used_mask;
pprintld1(pdev->strm, "/R%ld Do\n", pcs->id);
}
return 0;
}
private int
pdfmark_put_pairs(cos_dict_t *pcd, gs_param_string * pairs, uint count)
{
int code = 0, i;
if (count & 1)
return_error(gs_error_rangecheck);
for (i = 0; code >= 0 && i < count; i += 2)
code = pdfmark_put_pair(pcd, pairs + i);
return code;
}
private int
pdfmark_PAGES(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
return pdfmark_put_pairs(pdev->Pages, pairs, count);
}
private int
pdfmark_PAGE(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
return pdfmark_put_pairs(pdf_current_page_dict(pdev), pairs, count);
}
private int
pdfmark_add_pagelabel(gx_device_pdf * pdev, const gs_param_string *label)
{
cos_value_t value;
cos_dict_t *dict = 0;
int code = 0;
if (label != 0) {
if (!pdev->PageLabels) {
pdev->PageLabels = cos_array_alloc(pdev,
"pdfmark_add_pagelabel(PageLabels)");
if (pdev->PageLabels == 0)
return_error(gs_error_VMerror);
pdev->PageLabels->id = pdf_obj_ref(pdev);
pdev->PageLabels_current_page = 0;
pdev->PageLabels_current_label = cos_dict_alloc(pdev,
"pdfmark_add_pagelabel(first)");
if (pdev->PageLabels_current_label == 0)
return_error(gs_error_VMerror);
}
dict = cos_dict_alloc(pdev, "pdfmark_add_pagelabel(dict)");
if (dict == 0)
return_error(gs_error_VMerror);
code = cos_dict_put_c_key(dict, "/P", cos_string_value(&value,
label->data, label->size));
if (code < 0) {
COS_FREE(dict, "pdfmark_add_pagelabel(dict)");
return code;
}
}
if (label == 0 || pdev->next_page != pdev->PageLabels_current_page) {
if (pdev->PageLabels_current_label) {
if (code >= 0) {
code = cos_array_add_int(pdev->PageLabels,
pdev->PageLabels_current_page);
if (code >= 0)
code = cos_array_add(pdev->PageLabels,
COS_OBJECT_VALUE(&value,
pdev->PageLabels_current_label));
}
pdev->PageLabels_current_label = 0;
}
if (pdev->PageLabels) {
if (pdev->next_page - pdev->PageLabels_current_page > 1) {
cos_dict_t *tmp = cos_dict_alloc(pdev,
"pdfmark_add_pagelabel(tmp)");
if (tmp == 0)
return_error(gs_error_VMerror);
code = cos_array_add_int(pdev->PageLabels,
pdev->PageLabels_current_page + 1);
if (code >= 0)
code = cos_array_add(pdev->PageLabels,
COS_OBJECT_VALUE(&value, tmp));
}
}
}
if (pdev->PageLabels_current_label)
COS_FREE(pdev->PageLabels_current_label,
"pdfmark_add_pagelabel(current_label)");
pdev->PageLabels_current_label = dict;
pdev->PageLabels_current_page = pdev->next_page;
return code;
}
int
pdfmark_end_pagelabels(gx_device_pdf * pdev)
{
return pdfmark_add_pagelabel(pdev, 0);
}
private int
pdfmark_PAGELABEL(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
gs_param_string key;
if (pdev->CompatibilityLevel >= 1.3) {
if (pdfmark_find_key("/Label", pairs, count, &key)) {
return pdfmark_add_pagelabel(pdev, &key);
}
}
return 0;
}
private int
pdfmark_DOCINFO(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
cos_dict_t *const pcd = pdev->Info;
int code = 0, i;
gs_memory_t *mem = pdev->pdf_memory;
if (count & 1)
return_error(gs_error_rangecheck);
for (i = 0; code >= 0 && i < count; i += 2) {
const gs_param_string *pair = pairs + i;
gs_param_string alt_pair[2];
const byte *vdata;
uint vsize;
byte *str = 0;
vsize = 0x0badf00d;
if (pdf_key_eq(pairs + i, "/Producer")) {
string_match_params params;
memcpy(alt_pair, pairs + i, sizeof(alt_pair));
vdata = alt_pair[1].data;
vsize = alt_pair[1].size;
params = string_match_params_default;
params.ignore_case = true;
if (string_match(vdata, vsize, (const byte *)"*Distiller*",
11, &params) ||
string_match(vdata, vsize,
(const byte *)"*\000D\000i\000s\000t\000i\000l\000l\000e\000r*",
20, &params)
) {
uint j;
char buf[PDF_MAX_PRODUCER];
int len;
for (j = vsize; j > 0 && vdata[--j] != '+'; )
DO_NOTHING;
if (vsize - j > 2 && vdata[j] == '+') {
++j;
while (j < vsize && vdata[j] == ' ')
++j;
}
pdf_store_default_Producer(buf);
len = strlen(buf) - 1;
str = gs_alloc_string(mem, j + len, "Producer");
if (str == 0)
return_error(gs_error_VMerror);
memcpy(str, vdata, j);
memcpy(str + j, buf + 1, len);
alt_pair[1].data = vdata = str;
alt_pair[1].size = vsize = j + len;
pair = alt_pair;
}
}
code = pdfmark_put_pair(pcd, pair);
if (str)
gs_free_string(mem, str, vsize, "Producer");
}
return code;
}
private int
pdfmark_DOCVIEW(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
char dest[MAX_DEST_STRING];
int code = 0;
if (count & 1)
return_error(gs_error_rangecheck);
if (pdfmark_make_dest(dest, pdev, "/Page", "/View", pairs, count)) {
int i;
code = cos_dict_put_c_key_string(pdev->Catalog, "/OpenAction",
(byte *)dest, strlen(dest));
for (i = 0; code >= 0 && i < count; i += 2)
if (!(pdf_key_eq(&pairs[i], "/Page") ||
pdf_key_eq(&pairs[i], "/View"))
)
code = pdfmark_put_pair(pdev->Catalog, pairs + i);
return code;
} else
return pdfmark_put_pairs(pdev->Catalog, pairs, count);
}
private int
pdfmark_BP(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * objname)
{
gs_rect bbox;
cos_stream_t *pcs;
int code;
gs_matrix ictm;
byte bbox_str[6 + 6 * 15], matrix_str[6 + 6 * 15];
int bbox_str_len, matrix_str_len;
stream s;
if (objname == 0 || count != 2 || !pdf_key_eq(&pairs[0], "/BBox"))
return_error(gs_error_rangecheck);
code = gs_matrix_invert(pctm, &ictm);
if (code < 0)
return code;
if (sscanf((const char *)pairs[1].data, "[%lg %lg %lg %lg]",
&bbox.p.x, &bbox.p.y, &bbox.q.x, &bbox.q.y) != 4)
return_error(gs_error_rangecheck);
if ((pdev->used_mask << 1) == 0)
return_error(gs_error_limitcheck);
code = start_XObject(pdev, pdev->params.CompressPages, &pcs);
if (code < 0)
return code;
{ byte *s = gs_alloc_string(pdev->memory, objname->size, "pdfmark_PS");
if (s == NULL)
return_error(gs_error_VMerror);
memcpy(s, objname->data, objname->size);
pdev->objname.data = s;
pdev->objname.size = objname->size;
}
pcs->is_graphics = true;
gs_bbox_transform(&bbox, pctm, &bbox);
s_init(&s, NULL);
swrite_string(&s, bbox_str, sizeof(bbox_str));
pprintg4(&s, "[%g %g %g %g]",
bbox.p.x, bbox.p.y, bbox.q.x, bbox.q.y);
bbox_str_len = stell(&s);
swrite_string(&s, matrix_str, sizeof(bbox_str));
pprintg6(&s, "[%g %g %g %g %g %g]",
ictm.xx, ictm.xy, ictm.yx, ictm.yy, ictm.tx, ictm.ty);
matrix_str_len = stell(&s);
if ((code = cos_stream_put_c_strings(pcs, "/Type", "/XObject")) < 0 ||
(code = cos_stream_put_c_strings(pcs, "/Subtype", "/Form")) < 0 ||
(code = cos_stream_put_c_strings(pcs, "/FormType", "1")) < 0 ||
(code = cos_dict_put_c_key_string(cos_stream_dict(pcs), "/BBox",
bbox_str, bbox_str_len)) < 0 ||
(code = cos_dict_put_c_key_string(cos_stream_dict(pcs), "/Matrix",
matrix_str, matrix_str_len)) < 0 ||
(code = cos_dict_put_c_key_object(cos_stream_dict(pcs), "/Resources",
COS_OBJECT(pdev->substream_Resources))) < 0
)
return code;
return 0;
}
private int
pdfmark_EP(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
int code;
cos_value_t value;
pdf_resource_t *pres = pdev->accumulating_substream_resource;
gs_const_string objname = pdev->objname;
code = pdf_add_procsets(pdev->substream_Resources, pdev->procsets);
if (code < 0)
return code;
code = pdf_exit_substream(pdev);
if (code < 0)
return code;
code = pdf_substitute_resource(pdev, &pres, resourceXObject, NULL, true);
if (code < 0)
return code;
if (objname.size) {
code = cos_dict_put(pdev->local_named_objects, objname.data,
objname.size, cos_object_value(&value, (cos_object_t *)pres->object));
if (code < 0)
return code;
}
gs_free_const_string(pdev->memory, objname.data, objname.size, "pdfmark_EP");
if (code < 0)
return code;
return 0;
}
private int
pdfmark_SP(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
cos_object_t *pco;
int code;
if (count != 1)
return_error(gs_error_rangecheck);
if ((code = pdf_get_named(pdev, &pairs[0], cos_type_stream, &pco)) < 0)
return code;
if (pco->is_open || !pco->is_graphics)
return_error(gs_error_rangecheck);
code = pdf_open_contents(pdev, PDF_IN_STREAM);
if (code < 0)
return code;
pdf_put_matrix(pdev, "q ", pctm, "cm");
pprintld1(pdev->strm, "/R%ld Do Q\n", pco->id);
pco->pres->where_used |= pdev->used_mask;
return 0;
}
private int
pdfmark_OBJ(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * objname)
{
cos_type_t cotype;
cos_object_t *pco;
bool stream = false;
int code;
if (objname == 0 || count != 2 || !pdf_key_eq(&pairs[0], "/type"))
return_error(gs_error_rangecheck);
if (pdf_key_eq(&pairs[1], "/array"))
cotype = cos_type_array;
else if (pdf_key_eq(&pairs[1], "/dict"))
cotype = cos_type_dict;
else if ((stream = pdf_key_eq(&pairs[1], "/stream")))
cotype = cos_type_stream;
else
return_error(gs_error_rangecheck);
if ((code = pdf_make_named(pdev, objname, cotype, &pco, true)) < 0) {
if (code == gs_error_rangecheck &&
pdf_refer_named(pdev, objname, &pco) >= 0 &&
cos_type(pco) == cotype
)
return 0;
return code;
}
if (stream)
return setup_pdfmark_stream_compression((gx_device_psdf *)pdev,
(cos_stream_t *)pco);
return 0;
}
private int
pdfmark_PUT(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
cos_object_t *pco;
cos_value_t value;
int code, index;
if (count != 3)
return_error(gs_error_rangecheck);
if ((code = pdf_get_named(pdev, &pairs[0], cos_type_array, &pco)) < 0)
return code;
if ((code = pdfmark_scan_int(&pairs[1], &index)) < 0)
return code;
if (index < 0)
return_error(gs_error_rangecheck);
return cos_array_put((cos_array_t *)pco, index,
cos_string_value(&value, pairs[2].data, pairs[2].size));
}
private int
pdfmark_PUTDICT(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
cos_object_t *pco;
int code;
if ((code = pdf_refer_named(pdev, &pairs[0], &pco)) < 0)
return code;
if (cos_type(pco) != cos_type_dict && cos_type(pco) != cos_type_stream)
return_error(gs_error_typecheck);
return pdfmark_put_pairs((cos_dict_t *)pco, pairs + 1, count - 1);
}
private int
pdfmark_PUTSTREAM(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
cos_object_t *pco;
int code, i;
uint l;
if (count < 2)
return_error(gs_error_rangecheck);
if ((code = pdf_get_named(pdev, &pairs[0], cos_type_stream, &pco)) < 0)
return code;
if (!pco->is_open)
return_error(gs_error_rangecheck);
for (i = 1; i < count; ++i)
if (sputs(pco->input_strm, pairs[i].data, pairs[i].size, &l) != 0)
return_error(gs_error_ioerror);
return code;
}
private int
pdfmark_APPEND(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * objname)
{
cos_object_t *pco;
cos_value_t value;
int code;
if (count != 2)
return_error(gs_error_rangecheck);
if ((code = pdf_get_named(pdev, &pairs[0], cos_type_array, &pco)) < 0)
return code;
return cos_array_add((cos_array_t *)pco,
cos_string_value(&value, pairs[1].data, pairs[1].size));
}
private int
pdfmark_PUTINTERVAL(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
cos_object_t *pco;
cos_value_t value;
int code, index, i;
if (count < 2)
return_error(gs_error_rangecheck);
if ((code = pdf_get_named(pdev, &pairs[0], cos_type_array, &pco)) < 0)
return code;
if ((code = pdfmark_scan_int(&pairs[1], &index)) < 0)
return code;
if (index < 0)
return_error(gs_error_rangecheck);
for (i = 2; code >= 0 && i < count; ++i)
code = cos_array_put((cos_array_t *)pco, index + i - 2,
cos_string_value(&value, pairs[i].data, pairs[i].size));
return code;
}
private int
pdfmark_CLOSE(gx_device_pdf * pdev, gs_param_string * pairs, uint count,
const gs_matrix * pctm, const gs_param_string * no_objname)
{
cos_object_t *pco;
int code;
if (count != 1)
return_error(gs_error_rangecheck);
if ((code = pdf_get_named(pdev, &pairs[0], cos_type_stream, &pco)) < 0)
return code;
if (!pco->is_open)
return_error(gs_error_rangecheck);
pco->is_open = false;
return 0;
}
private int
pdfmark_NamespacePush(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
if (count != 0)
return_error(gs_error_rangecheck);
return pdf_push_namespace(pdev);
}
private int
pdfmark_NamespacePop(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
if (count != 0)
return_error(gs_error_rangecheck);
cos_dict_objects_write(pdev->local_named_objects, pdev);
return pdf_pop_namespace(pdev);
}
private int
pdfmark_NI(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
cos_object_t *pco;
int code;
if (objname == 0 || count != 0)
return_error(gs_error_rangecheck);
code = pdf_make_named(pdev, objname, cos_type_dict, &pco, true);
if (code < 0)
return code;
return cos_array_add_object(pdev->NI_stack, pco);
}
private int
pdfmark_MP(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_DP(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_BMC(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_BDC(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_EMC(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StRoleMap(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StClassMap(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StPNE(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StBookmarkRoot(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StPush(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StPop(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StPopAll(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StBMC(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StBDC(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StOBJ(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StAttr(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StStore(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private int
pdfmark_StRetrieve(gx_device_pdf *pdev, gs_param_string *pairs, uint count,
const gs_matrix *pctm, const gs_param_string *objname)
{
return 0;
}
private const pdfmark_name mark_names[] =
{
{"ANN", pdfmark_ANN, PDFMARK_NAMEABLE},
{"LNK", pdfmark_LNK, PDFMARK_NAMEABLE},
{"OUT", pdfmark_OUT, 0},
{"ARTICLE", pdfmark_ARTICLE, 0},
{"DEST", pdfmark_DEST, PDFMARK_NAMEABLE},
{"PS", pdfmark_PS, PDFMARK_NAMEABLE},
{"PAGES", pdfmark_PAGES, 0},
{"PAGE", pdfmark_PAGE, 0},
{"PAGELABEL", pdfmark_PAGELABEL, 0},
{"DOCINFO", pdfmark_DOCINFO, 0},
{"DOCVIEW", pdfmark_DOCVIEW, 0},
{"BP", pdfmark_BP, PDFMARK_NAMEABLE | PDFMARK_TRUECTM},
{"EP", pdfmark_EP, 0},
{"SP", pdfmark_SP, PDFMARK_ODD_OK | PDFMARK_KEEP_NAME | PDFMARK_TRUECTM},
{"OBJ", pdfmark_OBJ, PDFMARK_NAMEABLE},
{"PUT", pdfmark_PUT, PDFMARK_ODD_OK | PDFMARK_KEEP_NAME},
{".PUTDICT", pdfmark_PUTDICT, PDFMARK_ODD_OK | PDFMARK_KEEP_NAME},
{".PUTINTERVAL", pdfmark_PUTINTERVAL, PDFMARK_ODD_OK | PDFMARK_KEEP_NAME},
{".PUTSTREAM", pdfmark_PUTSTREAM, PDFMARK_ODD_OK | PDFMARK_KEEP_NAME |
PDFMARK_NO_REFS},
{"APPEND", pdfmark_APPEND, PDFMARK_KEEP_NAME},
{"CLOSE", pdfmark_CLOSE, PDFMARK_ODD_OK | PDFMARK_KEEP_NAME},
{"NamespacePush", pdfmark_NamespacePush, 0},
{"NamespacePop", pdfmark_NamespacePop, 0},
{"NI", pdfmark_NI, PDFMARK_NAMEABLE},
{"MP", pdfmark_MP, PDFMARK_ODD_OK},
{"DP", pdfmark_DP, 0},
{"BMC", pdfmark_BMC, PDFMARK_ODD_OK},
{"BDC", pdfmark_BDC, 0},
{"EMC", pdfmark_EMC, 0},
{"StRoleMap", pdfmark_StRoleMap, 0},
{"StClassMap", pdfmark_StClassMap, 0},
{"StPNE", pdfmark_StPNE, PDFMARK_NAMEABLE},
{"StBookmarkRoot", pdfmark_StBookmarkRoot, 0},
{"StPush", pdfmark_StPush, 0},
{"StPop", pdfmark_StPop, 0},
{"StPopAll", pdfmark_StPopAll, 0},
{"StBMC", pdfmark_StBMC, 0},
{"StBDC", pdfmark_StBDC, 0},
{"StOBJ", pdfmark_StOBJ, 0},
{"StAttr", pdfmark_StAttr, 0},
{"StStore", pdfmark_StStore, 0},
{"StRetrieve", pdfmark_StRetrieve, 0},
{0, 0}
};
int
pdfmark_process(gx_device_pdf * pdev, const gs_param_string_array * pma)
{
const gs_param_string *data = pma->data;
uint size = pma->size;
const gs_param_string *pts = &data[size - 1];
const gs_param_string *objname = 0;
gs_matrix ctm;
const pdfmark_name *pmn;
int code = 0;
if (size < 2 ||
sscanf((const char *)pts[-1].data, "[%g %g %g %g %g %g]",
&ctm.xx, &ctm.xy, &ctm.yx, &ctm.yy, &ctm.tx, &ctm.ty) != 6
)
return_error(gs_error_rangecheck);
size -= 2;
for (pmn = mark_names; pmn->mname != 0; ++pmn)
if (pdf_key_eq(pts, pmn->mname)) {
gs_memory_t *mem = pdev->pdf_memory;
int odd_ok = (pmn->options & PDFMARK_ODD_OK) != 0;
gs_param_string *pairs;
int j;
if (pmn->options & PDFMARK_TRUECTM)
DO_NOTHING;
else {
double xscale = 72.0 / pdev->HWResolution[0],
yscale = 72.0 / pdev->HWResolution[1];
ctm.xx *= xscale, ctm.xy *= yscale;
ctm.yx *= xscale, ctm.yy *= yscale;
ctm.tx *= xscale, ctm.ty *= yscale;
}
if (size & !odd_ok)
return_error(gs_error_rangecheck);
if (pmn->options & PDFMARK_NAMEABLE) {
for (j = 0; j < size; j += 2) {
if (pdf_key_eq(&data[j], "/_objdef")) {
objname = &data[j + 1];
if (!pdf_objname_is_valid(objname->data,
objname->size)
)
return_error(gs_error_rangecheck);
size -= 2;
pairs = (gs_param_string *)
gs_alloc_byte_array(mem, size,
sizeof(gs_param_string),
"pdfmark_process(pairs)");
if (!pairs)
return_error(gs_error_VMerror);
memcpy(pairs, data, j * sizeof(*data));
memcpy(pairs + j, data + j + 2,
(size - j) * sizeof(*data));
goto copied;
}
}
}
pairs = (gs_param_string *)
gs_alloc_byte_array(mem, size, sizeof(gs_param_string),
"pdfmark_process(pairs)");
if (!pairs)
return_error(gs_error_VMerror);
memcpy(pairs, data, size * sizeof(*data));
copied:
if (!(pmn->options & PDFMARK_NO_REFS)) {
for (j = (pmn->options & PDFMARK_KEEP_NAME ? 1 : 1 - odd_ok);
j < size; j += 2 - odd_ok
) {
code = pdf_replace_names(pdev, &pairs[j], &pairs[j]);
if (code < 0) {
gs_free_object(mem, pairs, "pdfmark_process(pairs)");
return code;
}
}
}
code = (*pmn->proc) (pdev, pairs, size, &ctm, objname);
gs_free_object(mem, pairs, "pdfmark_process(pairs)");
break;
}
return code;
}