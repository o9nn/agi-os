#include "ghost.h"
#include "string_.h"
#include "memory_.h"
#include "gsstruct.h"
#include "ialloc.h"
#include "iname.h"
#include "istack.h"
#include "iparam.h"
#include "ivmspace.h"
#include "oper.h"
#include "estack.h"
#include "store.h"
#include "idict.h"
#include "iddict.h"
#include "dscparse.h"
typedef struct dsc_data_s {
CDSC *dsc_data_ptr;
} dsc_data_t;
private void dsc_finalize(void *vptr);
gs_private_st_simple_final(st_dsc_data_t, dsc_data_t, "dsc_data_struct", dsc_finalize);
private const char * const dsc_dict_name = "DSC_struct";
private int
dsc_error_handler(void *caller_data, CDSC *dsc, unsigned int explanation,
const char *line, unsigned int line_len)
{
return CDSC_OK;
}
private int
zinitialize_dsc_parser(i_ctx_t *i_ctx_p)
{
ref local_ref;
int code;
os_ptr const op = osp;
dict * const pdict = op->value.pdict;
gs_memory_t * const mem = (gs_memory_t *)dict_memory(pdict);
dsc_data_t * const data =
gs_alloc_struct(mem, dsc_data_t, &st_dsc_data_t,
"DSC parser init");
data->dsc_data_ptr = dsc_init((void *) "Ghostscript DSC parsing");
if (!data->dsc_data_ptr)
return_error(e_VMerror);
dsc_set_error_function(data->dsc_data_ptr, dsc_error_handler);
make_astruct(&local_ref, a_readonly | r_space(op), (byte *) data);
code = idict_put_string(op, dsc_dict_name, &local_ref);
if (code >= 0)
pop(1);
return code;
}
private void
dsc_finalize(void *vptr)
{
dsc_data_t * const st = vptr;
if (st->dsc_data_ptr)
dsc_free(st->dsc_data_ptr);
st->dsc_data_ptr = NULL;
}
private int
dsc_put_int(gs_param_list *plist, const char *keyname, int value)
{
return param_write_int(plist, keyname, &value);
}
private int
dsc_put_string(gs_param_list *plist, const char *keyname,
const char *string)
{
gs_param_string str;
param_string_from_transient_string(str, string);
return param_write_string(plist, keyname, &str);
}
private int
dsc_put_bounding_box(gs_param_list *plist, const char *keyname,
const CDSCBBOX *pbbox)
{
int values[4];
gs_param_int_array va;
if (!pbbox)
return 0;
values[0] = pbbox->llx;
values[1] = pbbox->lly;
values[2] = pbbox->urx;
values[3] = pbbox->ury;
va.data = values;
va.size = 4;
va.persistent = false;
return param_write_int_array(plist, keyname, &va);
}
private int
dsc_adobe_header(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_int(plist, "EPSF", (int)(pData->epsf? 1: 0));
}
private int
dsc_creator(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_string(plist, "Creator", pData->dsc_creator );
}
private int
dsc_creation_date(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_string(plist, "CreationDate", pData->dsc_date );
}
private int
dsc_title(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_string(plist, "Title", pData->dsc_title );
}
private int
dsc_for(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_string(plist, "For", pData->dsc_for);
}
private int
dsc_bounding_box(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_bounding_box(plist, "BoundingBox", pData->bbox);
}
private int
dsc_page(gs_param_list *plist, const CDSC *pData)
{
int page_num = pData->page_count;
if (page_num)
return dsc_put_int(plist, "PageNum",
pData->page[page_num - 1].ordinal );
else
return dsc_put_int(plist, "PageNum", 0 );
}
private int
dsc_pages(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_int(plist, "NumPages", pData->page_pages);
}
private int
dsc_page_bounding_box(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_bounding_box(plist, "PageBoundingBox", pData->page_bbox);
}
private int
convert_orient(CDSC_ORIENTATION_ENUM orient)
{
switch (orient) {
case CDSC_PORTRAIT: return 0;
case CDSC_LANDSCAPE: return 1;
case CDSC_UPSIDEDOWN: return 2;
case CDSC_SEASCAPE: return 3;
default: return -1;
}
}
private int
dsc_page_orientation(gs_param_list *plist, const CDSC *pData)
{
int page_num = pData->page_count;
if (page_num && pData->page[page_num - 1].orientation != CDSC_ORIENT_UNKNOWN)
return dsc_put_int(plist, "PageOrientation",
convert_orient(pData->page[page_num - 1].orientation));
else
return dsc_put_int(plist, "Orientation",
convert_orient(pData->page_orientation));
}
private int
dsc_orientation(gs_param_list *plist, const CDSC *pData)
{
return dsc_put_int(plist, "Orientation",
convert_orient(pData->page_orientation));
}
private int
dsc_viewing_orientation(gs_param_list *plist, const CDSC *pData)
{
int page_num = pData->page_count;
const char *key;
const CDSCCTM *pctm;
float values[4];
gs_param_float_array va;
if (page_num && pData->page[page_num - 1].viewing_orientation != NULL) {
key = "PageViewingOrientation";
pctm = pData->page[page_num - 1].viewing_orientation;
} else {
key = "ViewingOrientation";
pctm = pData->viewing_orientation;
}
values[0] = pctm->xx;
values[1] = pctm->xy;
values[2] = pctm->yx;
values[3] = pctm->yy;
va.data = values;
va.size = 4;
va.persistent = false;
return param_write_float_array(plist, key, &va);
}
typedef struct cmd_list_s {
int code;
const char *comment_name;
int (*dsc_proc) (gs_param_list *, const CDSC *);
} cmdlist_t;
private const cmdlist_t DSCcmdlist[] = {
{ CDSC_PSADOBE,	    "Header",		dsc_adobe_header },
{ CDSC_CREATOR,	    "Creator",		dsc_creator },
{ CDSC_CREATIONDATE,    "CreationDate",	dsc_creation_date },
{ CDSC_TITLE,	    "Title",		dsc_title },
{ CDSC_FOR,		    "For",		dsc_for },
{ CDSC_BOUNDINGBOX,     "BoundingBox",	dsc_bounding_box },
{ CDSC_ORIENTATION,	    "Orientation",	dsc_orientation },
{ CDSC_BEGINDEFAULTS,   "BeginDefaults",	NULL },
{ CDSC_ENDDEFAULTS,     "EndDefaults",	NULL },
{ CDSC_PAGE,	    "Page",		dsc_page },
{ CDSC_PAGES,	    "Pages",		dsc_pages },
{ CDSC_PAGEORIENTATION, "PageOrientation",  dsc_page_orientation },
{ CDSC_PAGEBOUNDINGBOX, "PageBoundingBox",	dsc_page_bounding_box },
{ CDSC_VIEWINGORIENTATION, "ViewingOrientation", dsc_viewing_orientation },
{ CDSC_EOF,		    "EOF",		NULL },
{ 0,		    "NOP",		NULL }
};
private const char * const BadCmdlist[] = {
"%%BeginData:",
"%%EndData",
"%%BeginBinary:",
"%%EndBinary",
NULL
};
private int
zparse_dsc_comments(i_ctx_t *i_ctx_p)
{
#define MAX_DSC_MSG_SIZE (DSC_LINE_LENGTH + 4)
os_ptr const opString = osp;
os_ptr const opDict = opString - 1;
uint ssize;
int comment_code, code;
char dsc_buffer[MAX_DSC_MSG_SIZE + 2];
const cmdlist_t *pCmdList = DSCcmdlist;
const char * const *pBadList = BadCmdlist;
ref * pvalue;
CDSC * dsc_data = NULL;
dict_param_list list;
check_type(*opString, t_string);
check_dict_write(*opDict);
ssize = r_size(opString);
if (ssize > MAX_DSC_MSG_SIZE)
ssize = MAX_DSC_MSG_SIZE;
memcpy(dsc_buffer, opString->value.bytes, ssize);
dsc_buffer[ssize] = 0x0d;
dsc_buffer[ssize + 1] = 0;
while (*pBadList && strncmp(*pBadList, dsc_buffer, strlen(*pBadList)))
pBadList++;
if (*pBadList) {
comment_code = 0;
}
else {
code = dict_find_string(opDict, dsc_dict_name, &pvalue);
dsc_data = r_ptr(pvalue, dsc_data_t)->dsc_data_ptr;
if (code < 0)
return code;
comment_code = dsc_scan_data(dsc_data, dsc_buffer, ssize + 1);
if_debug1('%', "[%%].parse_dsc_comments: code = %d\n", comment_code);
if (comment_code < 0)
comment_code = 0;
}
while (pCmdList->code && pCmdList->code != comment_code )
pCmdList++;
if (pCmdList->dsc_proc) {
code = dict_param_list_write(&list, opDict, NULL, iimemory);
if (code < 0)
return code;
code = (pCmdList->dsc_proc)((gs_param_list *)&list, dsc_data);
iparam_list_release(&list);
if (code < 0)
return code;
}
return name_enter_string(imemory, pCmdList->comment_name, opString);
}
const op_def zdscpars_op_defs[] = {
{"1.initialize_dsc_parser", zinitialize_dsc_parser},
{"2.parse_dsc_comments", zparse_dsc_comments},
op_def_end(0)
};