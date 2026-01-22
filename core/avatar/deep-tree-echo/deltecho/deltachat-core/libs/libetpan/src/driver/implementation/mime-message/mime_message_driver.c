#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mime_message_driver.h"
#include "libetpan-config.h"
#include <sys/stat.h>
#include <sys/types.h>
#ifdef HAVE_UNISTD_H
#	include <unistd.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#	include <sys/mman.h>
#endif
#include <stdlib.h>
#include <string.h>
#include "mailmessage.h"
#include "mailmessage_tools.h"
#include "maildriver_tools.h"
int mime_message_set_tmpdir(mailmessage * msg, char * tmpdir)
{
return MAIL_NO_ERROR;
}
void mime_message_detach_mime(mailmessage * msg)
{
msg->msg_mime = NULL;
}
mailmessage * mime_message_init(struct mailmime * mime)
{
mailmessage * msg;
int r;
msg = mailmessage_new();
if (msg == NULL)
goto err;
r = mailmessage_init(msg, NULL, mime_message_driver, 0, 0);
if (r != MAIL_NO_ERROR)
goto free;
if (mime != NULL) {
mailmime_free(msg->msg_mime);
msg->msg_mime = mime;
}
return msg;
free:
mailmessage_free(msg);
err:
return NULL;
}
static int initialize(mailmessage * msg)
{
struct mailmime * mime;
int res;
mime = mailmime_new_message_data(NULL);
if (mime == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
msg->msg_mime = mime;
return MAIL_NO_ERROR;
err:
return res;
}
static void uninitialize(mailmessage * msg)
{
if (msg->msg_data != NULL)
free(msg->msg_data);
if (msg->msg_mime != NULL)
mailmime_free(msg->msg_mime);
msg->msg_mime = NULL;
}
static void flush(mailmessage * msg)
{
}
static void check(mailmessage * msg)
{
}
static void fetch_result_free(mailmessage * msg_info, char * content)
{
mmap_string_unref(content);
}
static int body_to_mmapstr(char * data, size_t size,
char ** result, size_t * result_len)
{
MMAPString * mmapstr;
size_t cur_token;
int res;
int r;
cur_token = 0;
while (1) {
r = mailimf_ignore_field_parse(data, size, &cur_token);
if (r == MAILIMF_NO_ERROR) {
}
else
break;
}
r = mailimf_crlf_parse(data, size, &cur_token);
if ((r != MAILIMF_NO_ERROR) && (r != MAILIMF_ERROR_PARSE)) {
res = maildriver_imf_error_to_mail_error(r);
goto err;
}
mmapstr = mmap_string_new_len(data + cur_token, size - cur_token);
if (mmapstr == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
r = mmap_string_ref(mmapstr);
if (r != 0) {
mmap_string_free(mmapstr);
res = MAIL_ERROR_MEMORY;
goto err;
}
* result = mmapstr->str;
* result_len = mmapstr->len;
return MAIL_NO_ERROR;
err:
return res;
}
static int body_body_to_mmapstr(char * data, size_t size,
char ** result, size_t * result_len)
{
size_t cur_token;
int res;
int r;
cur_token = 0;
while (1) {
r = mailimf_ignore_field_parse(data, size, &cur_token);
if (r == MAILIMF_NO_ERROR) {
}
else
break;
}
r = mailimf_crlf_parse(data, size, &cur_token);
if ((r != MAILIMF_NO_ERROR) && (r != MAILIMF_ERROR_PARSE)) {
res = maildriver_imf_error_to_mail_error(r);
goto err;
}
return body_to_mmapstr(data + cur_token, size - cur_token,
result, result_len);
err:
return res;
}
static int fetch_section(mailmessage * msg_info,
struct mailmime * mime,
char ** result, size_t * result_len)
{
int r;
int res;
int col;
MMAPString * str;
if (msg_info->msg_mime == NULL)
return MAIL_ERROR_INVAL;
str = mmap_string_new("");
if (str == NULL) {
res = MAILIMF_ERROR_MEMORY;
goto err;
}
col = 0;
r = mailmime_write_mem(str, &col, mime);
if (r != MAILIMF_NO_ERROR) {
res = maildriver_imf_error_to_mail_error(r);
goto free;
}
if (mime->mm_parent == NULL) {
r = mmap_string_ref(str);
if (r < 0) {
res = MAIL_ERROR_MEMORY;
goto free;
}
* result = str->str;
* result_len = str->len;
r = MAIL_NO_ERROR;
}
else {
r = body_to_mmapstr(str->str, str->len, result, result_len);
if (r == MAIL_NO_ERROR) {
mmap_string_free(str);
}
}
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
return MAIL_NO_ERROR;
free:
mmap_string_free(str);
err:
return res;
}
static int fetch_section_header(mailmessage * msg_info,
struct mailmime * mime,
char ** result, size_t * result_len)
{
int r;
int res;
int col;
MMAPString * str;
if (msg_info->msg_mime == NULL)
return MAIL_ERROR_INVAL;
str = mmap_string_new("");
if (str == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
col = 0;
if (mime->mm_type == MAILMIME_MESSAGE) {
if (mime->mm_data.mm_message.mm_fields != NULL) {
r = mailimf_fields_write_mem(str, &col, mime->mm_data.mm_message.mm_fields);
if (r != MAILIMF_NO_ERROR) {
res = maildriver_imf_error_to_mail_error(r);
goto free;
}
mailimf_string_write_mem(str, &col, "\r\n", 2);
}
}
r = mmap_string_ref(str);
if (r < 0) {
res = MAIL_ERROR_MEMORY;
goto free;
}
* result = str->str;
* result_len = str->len;
return MAIL_NO_ERROR;
free:
mmap_string_free(str);
err:
return res;
}
static int fetch_section_mime(mailmessage * msg_info,
struct mailmime * mime,
char ** result, size_t * result_len)
{
int r;
int res;
int col;
MMAPString * str;
if (msg_info->msg_mime == NULL)
return MAIL_ERROR_INVAL;
str = mmap_string_new("");
if (str == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
col = 0;
if (mime->mm_content_type != NULL) {
r = mailmime_content_write_mem(str, &col, mime->mm_content_type);
if (r != MAILIMF_NO_ERROR) {
res = maildriver_imf_error_to_mail_error(r);
goto free;
}
}
if (mime->mm_mime_fields != NULL) {
r = mailmime_fields_write_mem(str, &col, mime->mm_mime_fields);
if (r != MAILIMF_NO_ERROR) {
res = maildriver_imf_error_to_mail_error(r);
goto free;
}
}
mailimf_string_write_mem(str, &col, "\r\n", 2);
r = mmap_string_ref(str);
if (r < 0) {
res = MAIL_ERROR_MEMORY;
goto free;
}
* result = str->str;
* result_len = str->len;
return MAIL_NO_ERROR;
free:
mmap_string_free(str);
err:
return res;
}
static int fetch_section_body(mailmessage * msg_info,
struct mailmime * mime,
char ** result, size_t * result_len)
{
int r;
int res;
int col;
MMAPString * str;
if (msg_info->msg_mime == NULL)
return MAIL_ERROR_INVAL;
str = mmap_string_new("");
if (str == NULL) {
res = MAIL_ERROR_MEMORY;
goto err;
}
col = 0;
if (mime->mm_mime_fields != NULL) {
r = mailmime_write_mem(str, &col, mime);
if (r != MAILIMF_NO_ERROR) {
res = maildriver_imf_error_to_mail_error(r);
goto free;
}
}
if (mime->mm_type == MAILMIME_MESSAGE)
r = body_body_to_mmapstr(str->str, str->len, result, result_len);
else
r = body_to_mmapstr(str->str, str->len, result, result_len);
if (r != MAIL_NO_ERROR) {
res = r;
goto free;
}
mmap_string_free(str);
return MAIL_NO_ERROR;
free:
mmap_string_free(str);
err:
return res;
}
static int get_bodystructure(mailmessage * msg_info,
struct mailmime ** result)
{
if (msg_info->msg_mime == NULL)
return MAIL_ERROR_INVAL;
* result = msg_info->msg_mime;
return MAIL_NO_ERROR;
}
static int fetch(mailmessage * msg_info,
char ** result, size_t * result_len)
{
return fetch_section(msg_info, msg_info->msg_mime, result, result_len);
}
static int fetch_header(mailmessage * msg_info,
char ** result, size_t * result_len)
{
return fetch_section_header(msg_info,
msg_info->msg_mime, result, result_len);
}
static int fetch_body(mailmessage * msg_info,
char ** result, size_t * result_len)
{
return fetch_section_body(msg_info, msg_info->msg_mime, result, result_len);
}
static int fetch_size(mailmessage * msg_info,
size_t * result)
{
char * msg;
int r;
msg = NULL;
r = fetch(msg_info, &msg, result);
if (r != MAIL_NO_ERROR) {
return r;
}
fetch_result_free(msg_info, msg);
return MAIL_NO_ERROR;
}
static mailmessage_driver local_mime_message_driver = {
"mime",
initialize,
uninitialize,
flush,
check,
fetch_result_free,
fetch,
fetch_header,
fetch_body,
fetch_size,
get_bodystructure,
fetch_section,
fetch_section_header,
fetch_section_mime,
fetch_section_body,
mailmessage_generic_fetch_envelope,
NULL
};
mailmessage_driver * mime_message_driver = &local_mime_message_driver;