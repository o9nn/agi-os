#include <ctype.h>
#include "dc_context.h"
#include "dc_aheader.h"
#include "dc_apeerstate.h"
#include "dc_mimeparser.h"
void dc_aheader_empty(dc_aheader_t* aheader)
{
if (aheader==NULL) {
return;
}
aheader->prefer_encrypt = 0;
free(aheader->addr);
aheader->addr = NULL;
if (aheader->public_key->binary) {
dc_key_unref(aheader->public_key);
aheader->public_key = dc_key_new();
}
}
char* dc_aheader_render(const dc_aheader_t* aheader)
{
int             success = 0;
char*           keybase64_wrapped = NULL;
dc_strbuilder_t ret;
dc_strbuilder_init(&ret, 0);
if (aheader==NULL || aheader->addr==NULL || aheader->public_key->binary==NULL || aheader->public_key->type!=DC_KEY_PUBLIC) {
goto cleanup;
}
dc_strbuilder_cat(&ret, "addr=");
dc_strbuilder_cat(&ret, aheader->addr);
dc_strbuilder_cat(&ret, "; ");
if (aheader->prefer_encrypt==DC_PE_MUTUAL) {
dc_strbuilder_cat(&ret, "prefer-encrypt=mutual; ");
}
dc_strbuilder_cat(&ret, "keydata= ");
if ((keybase64_wrapped = dc_key_render_base64(aheader->public_key, 78, " ", 0))==NULL) {
goto cleanup;
}
dc_strbuilder_cat(&ret, keybase64_wrapped);
success = 1;
cleanup:
if (!success) { free(ret.buf); ret.buf = NULL; }
free(keybase64_wrapped);
return ret.buf;
}
static int add_attribute(dc_aheader_t* aheader, const char* name, const char* value )
{
if (strcasecmp(name, "addr")==0)
{
if (value==NULL
|| !dc_may_be_valid_addr(value)
|| aheader->addr ) {
return 0;
}
aheader->addr = dc_addr_normalize(value);
return 1;
}
#if 0
else if (strcasecmp(name, "type")==0)
{
if (value==NULL) {
return 0;
}
if (strcasecmp(value, "1")==0 || strcasecmp(value, "0" )==0 || strcasecmp(value, "p" )==0) {
return 1;
}
return 0;
}
#endif
else if (strcasecmp(name, "prefer-encrypt")==0)
{
if (value && strcasecmp(value, "mutual")==0) {
aheader->prefer_encrypt = DC_PE_MUTUAL;
return 1;
}
return 1;
}
else if (strcasecmp(name, "keydata")==0)
{
if (value==NULL
|| aheader->public_key->binary || aheader->public_key->bytes) {
return 0;
}
return dc_key_set_from_base64(aheader->public_key, value, DC_KEY_PUBLIC);
}
else if (name[0]=='_')
{
return 1;
}
return 0;
}
int dc_aheader_set_from_string(dc_aheader_t* aheader, const char* header_str__)
{
#define AHEADER_WS "\t\r\n "
char*   header_str = NULL;
char*   p = NULL;
char*   beg_attr_name = NULL;
char*   after_attr_name = NULL;
char*   beg_attr_value = NULL;
int     success = 0;
dc_aheader_empty(aheader);
if (aheader==NULL || header_str__==NULL) {
goto cleanup;
}
aheader->prefer_encrypt = DC_PE_NOPREFERENCE;
header_str = dc_strdup(header_str__);
p = header_str;
while (*p)
{
p += strspn(p, AHEADER_WS "=;");
beg_attr_name = p;
beg_attr_value = NULL;
p += strcspn(p, AHEADER_WS "=;");
if (p!=beg_attr_name)
{
after_attr_name = p;
p += strspn(p, AHEADER_WS);
if (*p=='=')
{
p += strspn(p, AHEADER_WS "=");
beg_attr_value = p;
p += strcspn(p, ";");
if (*p!='\0') {
*p = '\0';
p++;
}
dc_trim(beg_attr_value);
}
else
{
p += strspn(p, AHEADER_WS ";");
}
*after_attr_name = '\0';
if (!add_attribute(aheader, beg_attr_name, beg_attr_value)) {
goto cleanup;
}
}
}
if (aheader->addr && aheader->public_key->binary) {
success = 1;
}
cleanup:
free(header_str);
if (!success) { dc_aheader_empty(aheader); }
return success;
}
dc_aheader_t* dc_aheader_new()
{
dc_aheader_t* aheader = NULL;
if ((aheader=calloc(1, sizeof(dc_aheader_t)))==NULL) {
exit(37);
}
aheader->public_key = dc_key_new();
return aheader;
}
void dc_aheader_unref(dc_aheader_t* aheader)
{
if (aheader==NULL) {
return;
}
free(aheader->addr);
dc_key_unref(aheader->public_key);
free(aheader);
}
dc_aheader_t* dc_aheader_new_from_imffields(const char* wanted_from, const struct mailimf_fields* header)
{
clistiter*    cur = NULL;
dc_aheader_t* fine_header = NULL;
if (wanted_from==NULL || header==NULL) {
return 0;
}
for (cur = clist_begin(header->fld_list); cur!=NULL ; cur=clist_next(cur))
{
struct mailimf_field* field = (struct mailimf_field*)clist_content(cur);
if (field && field->fld_type==MAILIMF_FIELD_OPTIONAL_FIELD)
{
struct mailimf_optional_field* optional_field = field->fld_data.fld_optional_field;
if (optional_field && optional_field->fld_name && strcasecmp(optional_field->fld_name, "Autocrypt")==0)
{
dc_aheader_t* test = dc_aheader_new();
if (!dc_aheader_set_from_string(test, optional_field->fld_value)
|| dc_addr_cmp(test->addr, wanted_from)!=0) {
dc_aheader_unref(test);
test = NULL;
}
if (fine_header==NULL) {
fine_header = test;
}
else if (test) {
dc_aheader_unref(fine_header);
dc_aheader_unref(test);
return NULL;
}
}
}
}
return fine_header;
}