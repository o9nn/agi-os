#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailmime.h"
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "mailmime_types.h"
#include "mailmime_disposition.h"
#include "mailimf.h"
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif
static int mailmime_attribute_parse(const char * message, size_t length,
size_t * indx,
char ** result);
static int
mailmime_composite_type_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_composite_type ** result);
static int is_text(char ch);
static int
mailmime_discrete_type_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_discrete_type ** result);
static int mailmime_mechanism_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_mechanism ** result);
static int mailmime_subtype_parse(const char * message, size_t length,
size_t * indx, char ** result);
static int is_token(char ch);
static int mailmime_token_parse(const char * message, size_t length,
size_t * indx,
char ** token);
static int is_tspecials(char ch);
static int mailmime_type_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_type ** result);
static int mailmime_attribute_parse(const char * message, size_t length,
size_t * indx,
char ** result)
{
return mailmime_token_parse(message, length, indx, result);
}
static int
mailmime_composite_type_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_composite_type ** result)
{
char * extension_token;
int type;
struct mailmime_composite_type * ct;
size_t cur_token;
int r;
int res;
cur_token = * indx;
extension_token = NULL;
type = MAILMIME_COMPOSITE_TYPE_ERROR;
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "message");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_COMPOSITE_TYPE_MESSAGE;
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "multipart");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_COMPOSITE_TYPE_MULTIPART;
}
if (r != MAILIMF_NO_ERROR) {
res = r;
goto err;
}
ct = mailmime_composite_type_new(type, extension_token);
if (ct == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free_extension;
}
* result = ct;
* indx = cur_token;
return MAILIMF_NO_ERROR;
free_extension:
if (extension_token != NULL)
mailmime_extension_token_free(extension_token);
err:
return res;
}
LIBETPAN_EXPORT
int mailmime_content_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_content ** result)
{
size_t cur_token;
struct mailmime_type * type;
char * subtype;
clist * parameters_list;
struct mailmime_content * content;
int r;
int res;
cur_token = * indx;
mailimf_cfws_parse(message, length, &cur_token);
type = NULL;
r = mailmime_type_parse(message, length, &cur_token, &type);
if (r != MAILIMF_NO_ERROR) {
res = r;
goto err;
}
r = mailimf_unstrict_char_parse(message, length, &cur_token, '/');
switch (r) {
case MAILIMF_NO_ERROR:
r = mailimf_cfws_parse(message, length, &cur_token);
if ((r != MAILIMF_NO_ERROR) && (r != MAILIMF_ERROR_PARSE)) {
res = r;
goto free_type;
}
r = mailmime_subtype_parse(message, length, &cur_token, &subtype);
if (r != MAILIMF_NO_ERROR) {
res = r;
goto free_type;
}
break;
case MAILIMF_ERROR_PARSE:
subtype = strdup("unknown");
break;
default:
res = r;
goto free_type;
}
parameters_list = clist_new();
if (parameters_list == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free_subtype;
}
while (1) {
size_t final_token;
struct mailmime_parameter * parameter;
final_token = cur_token;
r = mailimf_unstrict_char_parse(message, length, &cur_token, ';');
if (r != MAILIMF_NO_ERROR) {
cur_token = final_token;
break;
}
r = mailimf_cfws_parse(message, length, &cur_token);
if ((r != MAILIMF_NO_ERROR) && (r != MAILIMF_ERROR_PARSE)) {
res = r;
goto free_subtype;
}
r = mailmime_parameter_parse(message, length, &cur_token, &parameter);
if (r == MAILIMF_NO_ERROR) {
}
else if (r == MAILIMF_ERROR_PARSE) {
cur_token = final_token;
break;
}
else {
res = r;
goto free_subtype;
}
r = clist_append(parameters_list, parameter);
if (r < 0) {
mailmime_parameter_free(parameter);
res = MAILIMF_ERROR_MEMORY;
goto free_parameters;
}
}
content = mailmime_content_new(type, subtype, parameters_list);
if (content == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free_parameters;
}
* result = content;
* indx = cur_token;
return MAILIMF_NO_ERROR;
free_parameters:
clist_foreach(parameters_list, (clist_func) mailmime_parameter_free, NULL);
clist_free(parameters_list);
free_subtype:
mailmime_subtype_free(subtype);
free_type:
mailmime_type_free(type);
err:
return res;
}
static int is_text(char ch)
{
unsigned char uch = (unsigned char) ch;
if (uch < 1)
return FALSE;
if ((uch == 10) || (uch == 13))
return FALSE;
return TRUE;
}
LIBETPAN_EXPORT
int mailmime_description_parse(const char * message, size_t length,
size_t * indx,
char ** result)
{
return mailimf_custom_string_parse(message, length,
indx, result,
is_text);
}
LIBETPAN_EXPORT
int mailmime_location_parse(const char * message, size_t length,
size_t * indx,
char ** result)
{
return mailimf_custom_string_parse(message, length,
indx, result,
is_text);
}
static int
mailmime_discrete_type_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_discrete_type ** result)
{
char * extension;
int type;
struct mailmime_discrete_type * discrete_type;
size_t cur_token;
int r;
int res;
cur_token = * indx;
extension = NULL;
type = MAILMIME_DISCRETE_TYPE_ERROR;
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "text");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_DISCRETE_TYPE_TEXT;
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "image");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_DISCRETE_TYPE_IMAGE;
}
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "audio");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_DISCRETE_TYPE_AUDIO;
}
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "video");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_DISCRETE_TYPE_VIDEO;
}
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "application");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_DISCRETE_TYPE_APPLICATION;
}
if (r == MAILIMF_ERROR_PARSE) {
r = mailmime_extension_token_parse(message, length,
&cur_token, &extension);
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_DISCRETE_TYPE_EXTENSION;
}
if (r != MAILIMF_NO_ERROR) {
res = r;
goto err;
}
discrete_type = mailmime_discrete_type_new(type, extension);
if (discrete_type == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free;
}
* result = discrete_type;
* indx = cur_token;
return MAILIMF_NO_ERROR;
free:
mailmime_extension_token_free(extension);
err:
return res;
}
LIBETPAN_EXPORT
int mailmime_encoding_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_mechanism ** result)
{
return mailmime_mechanism_parse(message, length, indx, result);
}
enum {
FIELD_STATE_START,
FIELD_STATE_T,
FIELD_STATE_D,
FIELD_STATE_L
};
static int guess_field_type(char * name)
{
int state;
if (* name == 'M')
return MAILMIME_FIELD_VERSION;
if (strncasecmp(name, "Content-", 8) != 0)
return MAILMIME_FIELD_NONE;
name += 8;
state = FIELD_STATE_START;
while (1) {
switch (state) {
case FIELD_STATE_START:
switch ((char) toupper((unsigned char) * name)) {
case 'T':
state = FIELD_STATE_T;
break;
case 'I':
return MAILMIME_FIELD_ID;
case 'D':
state = FIELD_STATE_D;
break;
case 'L':
state = FIELD_STATE_L;
break;
default:
return MAILMIME_FIELD_NONE;
}
break;
case FIELD_STATE_T:
switch ((char) toupper((unsigned char) * name)) {
case 'Y':
return MAILMIME_FIELD_TYPE;
case 'R':
return MAILMIME_FIELD_TRANSFER_ENCODING;
default:
return MAILMIME_FIELD_NONE;
}
break;
case FIELD_STATE_D:
switch ((char) toupper((unsigned char) * name)) {
case 'E':
return MAILMIME_FIELD_DESCRIPTION;
case 'I':
return MAILMIME_FIELD_DISPOSITION;
default:
return MAILMIME_FIELD_NONE;
}
break;
case FIELD_STATE_L:
switch ((char) toupper((unsigned char) * name)) {
case 'A':
return MAILMIME_FIELD_LANGUAGE;
case 'O':
return MAILMIME_FIELD_LOCATION;
default:
return MAILMIME_FIELD_NONE;
}
break;
}
name ++;
}
}
LIBETPAN_EXPORT
int
mailmime_field_parse(struct mailimf_optional_field * field,
struct mailmime_field ** result)
{
char * name;
char * value;
int guessed_type;
size_t cur_token;
struct mailmime_content * content;
struct mailmime_mechanism * encoding;
char * id;
char * description;
uint32_t version;
struct mailmime_field * mime_field;
struct mailmime_language * language;
struct mailmime_disposition * disposition;
char * location;
int res;
int r;
name = field->fld_name;
value = field->fld_value;
cur_token = 0;
content = NULL;
encoding = NULL;
id = NULL;
description = NULL;
version = 0;
disposition = NULL;
language = NULL;
location = NULL;
guessed_type = guess_field_type(name);
switch (guessed_type) {
case MAILMIME_FIELD_TYPE:
if (strcasecmp(name, "Content-Type") != 0)
return MAILIMF_ERROR_PARSE;
{
size_t cur_token = 0;
char * decoded_value;
r = mailmime_encoded_phrase_parse("us-ascii",
value, strlen(value),
&cur_token, "utf-8", &decoded_value);
if (r != MAILIMF_NO_ERROR) {
cur_token = 0;
r = mailmime_content_parse(value, strlen(value), &cur_token, &content);
}
else {
cur_token = 0;
r = mailmime_content_parse(decoded_value, strlen(decoded_value), &cur_token, &content);
free(decoded_value);
}
if (r != MAILIMF_NO_ERROR)
return r;
}
break;
case MAILMIME_FIELD_TRANSFER_ENCODING:
if (strcasecmp(name, "Content-Transfer-Encoding") != 0)
return MAILIMF_ERROR_PARSE;
r = mailmime_encoding_parse(value, strlen(value), &cur_token, &encoding);
if (r != MAILIMF_NO_ERROR)
return r;
break;
case MAILMIME_FIELD_ID:
if (strcasecmp(name, "Content-ID") != 0)
return MAILIMF_ERROR_PARSE;
r = mailmime_id_parse(value, strlen(value), &cur_token, &id);
if (r != MAILIMF_NO_ERROR)
return r;
break;
case MAILMIME_FIELD_DESCRIPTION:
if (strcasecmp(name, "Content-Description") != 0)
return MAILIMF_ERROR_PARSE;
r = mailmime_description_parse(value, strlen(value),
&cur_token, &description);
if (r != MAILIMF_NO_ERROR)
return r;
break;
case MAILMIME_FIELD_VERSION:
if (strcasecmp(name, "MIME-Version") != 0)
return MAILIMF_ERROR_PARSE;
r = mailmime_version_parse(value, strlen(value), &cur_token, &version);
if (r != MAILIMF_NO_ERROR)
return r;
break;
case MAILMIME_FIELD_DISPOSITION:
if (strcasecmp(name, "Content-Disposition") != 0)
return MAILIMF_ERROR_PARSE;
r = mailmime_disposition_parse(value, strlen(value),
&cur_token, &disposition);
if (r != MAILIMF_NO_ERROR)
return r;
break;
case MAILMIME_FIELD_LANGUAGE:
if (strcasecmp(name, "Content-Language") != 0)
return MAILIMF_ERROR_PARSE;
r = mailmime_language_parse(value, strlen(value), &cur_token, &language);
if (r != MAILIMF_NO_ERROR)
return r;
break;
case MAILMIME_FIELD_LOCATION:
if (strcasecmp(name, "Content-Location") != 0)
return MAILIMF_ERROR_PARSE;
r = mailmime_location_parse(value, strlen(value), &cur_token, &location);
if (r != MAILIMF_NO_ERROR)
return r;
break;
default:
return MAILIMF_ERROR_PARSE;
}
mime_field = mailmime_field_new(guessed_type, content, encoding,
id, description, version, disposition,
language, location);
if (mime_field == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free;
}
* result = mime_field;
return MAILIMF_NO_ERROR;
free:
if (location != NULL)
mailmime_location_free(location);
if (language != NULL)
mailmime_language_free(language);
if (content != NULL)
mailmime_content_free(content);
if (encoding != NULL)
mailmime_encoding_free(encoding);
if (id != NULL)
mailmime_id_free(id);
if (description != NULL)
mailmime_description_free(description);
return res;
}
LIBETPAN_EXPORT
int
mailmime_extension_token_parse(const char * message, size_t length,
size_t * indx, char ** result)
{
return mailmime_token_parse(message, length, indx, result);
}
LIBETPAN_EXPORT
int mailmime_id_parse(const char * message, size_t length,
size_t * indx, char ** result)
{
return mailimf_msg_id_parse(message, length, indx, result);
}
static int mailmime_mechanism_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_mechanism ** result)
{
char * token;
int type;
struct mailmime_mechanism * mechanism;
size_t cur_token;
int r;
int res;
cur_token = * indx;
type = MAILMIME_MECHANISM_ERROR;
token = NULL;
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "7bit");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_MECHANISM_7BIT;
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "8bit");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_MECHANISM_8BIT;
}
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "binary");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_MECHANISM_BINARY;
}
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "quoted-printable");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_MECHANISM_QUOTED_PRINTABLE;
}
if (r == MAILIMF_ERROR_PARSE) {
r = mailimf_token_case_insensitive_parse(message, length,
&cur_token, "base64");
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_MECHANISM_BASE64;
}
if (r == MAILIMF_ERROR_PARSE) {
r = mailmime_token_parse(message, length, &cur_token, &token);
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_MECHANISM_TOKEN;
}
if (r != MAILIMF_NO_ERROR) {
res = r;
goto err;
}
mechanism = mailmime_mechanism_new(type, token);
if (mechanism == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free;
}
* result = mechanism;
* indx = cur_token;
return MAILIMF_NO_ERROR;
free:
if (token != NULL)
mailmime_token_free(token);
err:
return res;
}
#if 0
LIBETPAN_EXPORT
int
mailmime_unparsed_fields_parse(struct mailimf_unparsed_fields *
fields,
struct mailmime_fields **
result)
{
clistiter * cur;
struct mailmime_fields * mime_fields;
clist * list;
int r;
int res;
list = clist_new();
if (list == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto err;
}
if (fields->list == NULL) {
res = MAILIMF_ERROR_PARSE;
goto err;
}
for(cur = clist_begin(fields->list) ; cur != NULL ;
cur = clist_next(cur)) {
struct mailimf_optional_field * field = cur->data;
struct mailmime_field * mime_field;
r = mailmime_field_parse(field, &mime_field);
if (r == MAILIMF_NO_ERROR) {
r = clist_append(list, mime_field);
if (r < 0) {
mailmime_field_free(mime_field);
res = MAILIMF_ERROR_MEMORY;
goto free_list;
}
}
}
if (clist_begin(list) == NULL) {
res = MAILIMF_ERROR_PARSE;
goto free_list;
}
mime_fields = mailmime_fields_new(list);
if (mime_fields == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free_list;
}
* result = mime_fields;
return MAILIMF_NO_ERROR;
free_list:
clist_foreach(list, (clist_func) mailmime_field_free, NULL);
clist_free(list);
err:
return res;
}
#endif
LIBETPAN_EXPORT
int
mailmime_fields_parse(struct mailimf_fields *
fields,
struct mailmime_fields **
result)
{
clistiter * cur;
struct mailmime_fields * mime_fields;
clist * list;
int r;
int res;
list = clist_new();
if (list == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto err;
}
for(cur = clist_begin(fields->fld_list) ; cur != NULL ;
cur = clist_next(cur)) {
struct mailimf_field * field;
struct mailmime_field * mime_field;
field = clist_content(cur);
if (field->fld_type == MAILIMF_FIELD_OPTIONAL_FIELD) {
r = mailmime_field_parse(field->fld_data.fld_optional_field,
&mime_field);
if (r == MAILIMF_NO_ERROR) {
r = clist_append(list, mime_field);
if (r < 0) {
mailmime_field_free(mime_field);
res = MAILIMF_ERROR_MEMORY;
goto free_list;
}
}
else if (r == MAILIMF_ERROR_PARSE) {
}
else {
res = r;
goto free_list;
}
}
}
if (clist_begin(list) == NULL) {
res = MAILIMF_ERROR_PARSE;
goto free_list;
}
mime_fields = mailmime_fields_new(list);
if (mime_fields == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free_list;
}
* result = mime_fields;
return MAILIMF_NO_ERROR;
free_list:
clist_foreach(list, (clist_func) mailmime_field_free, NULL);
clist_free(list);
err:
return res;
}
LIBETPAN_EXPORT
int mailmime_parameter_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_parameter ** result)
{
char * attribute;
char * value;
struct mailmime_parameter * parameter;
size_t cur_token;
int r;
int res;
cur_token = * indx;
r = mailmime_attribute_parse(message, length, &cur_token, &attribute);
if (r != MAILIMF_NO_ERROR) {
res = r;
goto err;
}
r = mailimf_unstrict_char_parse(message, length, &cur_token, '=');
if (r != MAILIMF_NO_ERROR) {
res = r;
goto free_attr;
}
r = mailimf_cfws_parse(message, length, &cur_token);
if ((r != MAILIMF_NO_ERROR) && (r != MAILIMF_ERROR_PARSE)) {
res = r;
goto free_attr;
}
r = mailmime_value_parse(message, length, &cur_token, &value);
if (r != MAILIMF_NO_ERROR) {
res = r;
goto free_attr;
}
parameter = mailmime_parameter_new(attribute, value);
if (parameter == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free_value;
}
* result = parameter;
* indx = cur_token;
return MAILIMF_NO_ERROR;
free_value:
mailmime_value_free(value);
free_attr:
mailmime_attribute_free(attribute);
err:
return res;
}
static int mailmime_subtype_parse(const char * message, size_t length,
size_t * indx, char ** result)
{
return mailmime_extension_token_parse(message, length, indx, result);
}
static int is_token(char ch)
{
unsigned char uch = (unsigned char) ch;
if (uch > 0x7F)
return FALSE;
if (uch == ' ')
return FALSE;
if (is_tspecials(ch))
return FALSE;
return TRUE;
}
static int mailmime_token_parse(const char * message, size_t length,
size_t * indx,
char ** token)
{
return mailimf_custom_string_parse(message, length,
indx, token,
is_token);
}
static int is_tspecials(char ch)
{
switch (ch) {
case '(':
case ')':
case '<':
case '>':
case '@':
case ',':
case ';':
case ':':
case '\\':
case '\"':
case '/':
case '[':
case ']':
case '?':
case '=':
return TRUE;
default:
return FALSE;
}
}
static int mailmime_type_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_type ** result)
{
struct mailmime_discrete_type * discrete_type;
struct mailmime_composite_type * composite_type;
size_t cur_token;
struct mailmime_type * mime_type;
int type;
int res;
int r;
cur_token = * indx;
discrete_type = NULL;
composite_type = NULL;
type = MAILMIME_TYPE_ERROR;
r = mailmime_composite_type_parse(message, length, &cur_token,
&composite_type);
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_TYPE_COMPOSITE_TYPE;
if (r == MAILIMF_ERROR_PARSE) {
r = mailmime_discrete_type_parse(message, length, &cur_token,
&discrete_type);
if (r == MAILIMF_NO_ERROR)
type = MAILMIME_TYPE_DISCRETE_TYPE;
}
if (r != MAILIMF_NO_ERROR) {
res = r;
goto err;
}
mime_type = mailmime_type_new(type, discrete_type, composite_type);
if (mime_type == NULL) {
res = r;
goto free;
}
* result = mime_type;
* indx = cur_token;
return MAILIMF_NO_ERROR;
free:
if (discrete_type != NULL)
mailmime_discrete_type_free(discrete_type);
if (composite_type != NULL)
mailmime_composite_type_free(composite_type);
err:
return res;
}
LIBETPAN_EXPORT
int mailmime_value_parse(const char * message, size_t length,
size_t * indx, char ** result)
{
int r;
r = mailimf_atom_parse(message, length, indx, result);
if (r == MAILIMF_ERROR_PARSE)
r = mailimf_quoted_string_parse(message, length, indx, result);
if (r != MAILIMF_NO_ERROR)
return r;
return MAILIMF_NO_ERROR;
}
LIBETPAN_EXPORT
int mailmime_version_parse(const char * message, size_t length,
size_t * indx,
uint32_t * result)
{
size_t cur_token;
uint32_t hi;
uint32_t low;
uint32_t version;
int r;
cur_token = * indx;
r = mailimf_number_parse(message, length, &cur_token, &hi);
if (r != MAILIMF_NO_ERROR)
return r;
r = mailimf_unstrict_char_parse(message, length, &cur_token, '.');
if (r != MAILIMF_NO_ERROR)
return r;
r = mailimf_cfws_parse(message, length, &cur_token);
if ((r != MAILIMF_NO_ERROR) && (r != MAILIMF_ERROR_PARSE))
return r;
r = mailimf_number_parse(message, length, &cur_token, &low);
if (r != MAILIMF_NO_ERROR)
return r;
version = (hi << 16) + low;
* result = version;
* indx = cur_token;
return MAILIMF_NO_ERROR;
}
LIBETPAN_EXPORT
int mailmime_language_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_language ** result)
{
size_t cur_token;
int r;
int res;
clist * list;
struct mailmime_language * language;
cur_token = * indx;
list = clist_new();
if (list == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto err;
}
while (1) {
char * atom;
r = mailimf_unstrict_char_parse(message, length, &cur_token, ',');
if (r == MAILIMF_NO_ERROR) {
}
else if (r == MAILIMF_ERROR_PARSE) {
break;
}
else {
res = r;
goto err;
}
r = mailimf_atom_parse(message, length, &cur_token, &atom);
if (r == MAILIMF_NO_ERROR) {
}
else if (r == MAILIMF_ERROR_PARSE) {
break;
}
else {
res = r;
goto err;
}
r = clist_append(list, atom);
if (r < 0) {
mailimf_atom_free(atom);
res = MAILIMF_ERROR_MEMORY;
goto free;
}
}
language = mailmime_language_new(list);
if (language == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto free;
}
* result = language;
* indx = cur_token;
return MAILIMF_NO_ERROR;
free:
clist_foreach(list, (clist_func) mailimf_atom_free, NULL);
clist_free(list);
err:
return res;
}