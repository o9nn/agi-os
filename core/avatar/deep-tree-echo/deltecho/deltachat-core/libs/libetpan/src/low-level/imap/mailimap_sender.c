#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailstream.h"
#include "mailimap_keywords.h"
#include "mailimap_sender.h"
#include "mailimap_parser.h"
#include "clist.h"
#include "mail.h"
#include "base64.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
static int is_ascii(const char * str);
static int mailimap_atom_send(mailstream * fd, const char * atom);
static int mailimap_auth_type_send(mailstream * fd, const char * auth_type);
static int mailimap_base64_send(mailstream * fd, const char * base64);
static int mailimap_date_send(mailstream * fd,
struct mailimap_date * date);
static int mailimap_date_day_send(mailstream * fd, int day);
static int mailimap_date_month_send(mailstream * fd, int month);
static int mailimap_date_year_send(mailstream *fd, int year);
static int
mailimap_date_time_send(mailstream * fd,
struct mailimap_date_time * date_time);
static int mailimap_digit_send(mailstream * fd, int digit);
static int
mailimap_fetch_type_send(mailstream * fd,
struct mailimap_fetch_type * fetch_type);
static int mailimap_fetch_att_send(mailstream * fd,
struct mailimap_fetch_att * fetch_att);
static int mailimap_flag_send(mailstream * fd,
struct mailimap_flag * flag);
static int mailimap_flag_extension_send(mailstream * fd,
const char * flag_extension);
static int mailimap_flag_keyword_send(mailstream * fd,
const char * flag_keyword);
static int mailimap_flag_list_send(mailstream * fd,
struct mailimap_flag_list * flag_list);
static int mailimap_header_fld_name_send(mailstream * fd, const char * header);
static int
mailimap_header_list_send(mailstream * fd,
struct mailimap_header_list * header_list);
static int mailimap_password_send(mailstream * fd, const char * pass);
static int mailimap_quoted_char_send(mailstream * fd, char ch);
static int
mailimap_section_send(mailstream * fd,
struct mailimap_section * section);
static int
mailimap_section_msgtext_send(mailstream * fd,
struct mailimap_section_msgtext *
section_msgtext);
static int
mailimap_section_part_send(mailstream * fd,
struct mailimap_section_part * section);
static int
mailimap_section_spec_send(mailstream * fd,
struct mailimap_section_spec * section_spec);
static int
mailimap_section_text_send(mailstream * fd,
struct mailimap_section_text * section_text);
static int
mailimap_sequence_num_send(mailstream * fd, uint32_t sequence_num);
static int mailimap_set_item_send(mailstream * fd,
struct mailimap_set_item * item);
static int mailimap_status_att_send(mailstream * fd, int * status_att);
static int
mailimap_store_att_flags_send(mailstream * fd,
struct mailimap_store_att_flags * store_flags);
static int mailimap_userid_send(mailstream * fd, const char * user);
static int mailimap_astring_literalplus_send(mailstream * fd, const char * astring,
int literalplus_enabled);
static int
mailimap_literalplus_count_send(mailstream * fd, size_t count);
static int search_key_notoplevel_send(mailstream * fd,
struct mailimap_search_key * key);
static int search_key_literalplus_notoplevel_send(mailstream * fd,
struct mailimap_search_key * key);
static inline int mailimap_sized_token_send_with_context(mailstream * fd, const char * atom,
size_t len,
mailprogress_function * progr_fun,
void * context);
static inline int mailimap_sized_token_send(mailstream * fd, const char * atom,
size_t len)
{
return mailimap_sized_token_send_with_context(fd, atom, len, NULL, NULL);
}
static inline int mailimap_sized_token_send_with_context(mailstream * fd, const char * atom,
size_t len,
mailprogress_function * progr_fun,
void * context)
{
if (mailstream_send_data_crlf_with_context(fd, atom, len, progr_fun, context) == -1)
return MAILIMAP_ERROR_STREAM;
return MAILIMAP_NO_ERROR;
}
int mailimap_token_send(mailstream * fd, const char * atom)
{
return mailimap_sized_token_send(fd, atom, strlen(atom));
}
int mailimap_char_send(mailstream * fd, char ch)
{
if (mailstream_write(fd, &ch, 1) == -1)
return MAILIMAP_ERROR_STREAM;
return MAILIMAP_NO_ERROR;
}
static int
mailimap_struct_list_send(mailstream * fd, clist * list,
char symbol,
mailimap_struct_sender * sender)
{
clistiter * cur;
void * elt;
int r;
cur = clist_begin(list);
if (cur == NULL)
return MAILIMAP_NO_ERROR;
elt = clist_content(cur);
r = (* sender)(fd, elt);
if (r != MAILIMAP_NO_ERROR)
return r;
cur = clist_next(cur);
while (cur != NULL) {
r = mailimap_char_send(fd, symbol);
if (r != MAILIMAP_NO_ERROR)
return r;
elt = clist_content(cur);
r = (* sender)(fd, elt);
if (r != MAILIMAP_NO_ERROR)
return r;
cur = clist_next(cur);
}
return MAILIMAP_NO_ERROR;
}
int
mailimap_struct_spaced_list_send(mailstream * fd, clist * list,
mailimap_struct_sender * sender)
{
return mailimap_struct_list_send(fd, list, ' ', sender);
}
int mailimap_space_send(mailstream * fd)
{
return mailimap_char_send(fd, ' ');
}
int mailimap_crlf_send(mailstream * fd)
{
int r;
r = mailimap_char_send(fd, '\r');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '\n');
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_oparenth_send(mailstream * fd)
{
return mailimap_char_send(fd, '(');
}
int mailimap_cparenth_send(mailstream * fd)
{
return mailimap_char_send(fd, ')');
}
static int mailimap_dquote_send(mailstream * fd)
{
return mailimap_char_send(fd, '"');
}
int mailimap_append_send(mailstream * fd,
const char * mailbox,
struct mailimap_flag_list * flag_list,
struct mailimap_date_time * date_time,
size_t literal_size)
{
int r;
r = mailimap_token_send(fd, "APPEND");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mailbox);
if (r != MAILIMAP_NO_ERROR)
return r;
if (flag_list != NULL) {
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_flag_list_send(fd, flag_list);
if (r != MAILIMAP_NO_ERROR)
return r;
}
if (date_time != NULL) {
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_time_send(fd, date_time);
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_literal_count_send(fd, literal_size);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int is_atom(const char * str)
{
if (* str == '\0')
return 0;
while (* str != '\0') {
unsigned char uch = (unsigned char) * str;
if (uch != '-') {
if (!isalnum(uch))
return 0;
}
str ++;
}
return 1;
}
static int mailimap_literalplus_send(mailstream * fd, const char * literal)
{
size_t len;
size_t literal_len;
int r;
len = strlen(literal);
literal_len = mailstream_get_data_crlf_size(literal, len);
r = mailimap_literalplus_count_send(fd, literal_len);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_literal_data_send(fd, literal, literal_len, 0, NULL);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_astring_literalplus_send(mailstream * fd, const char * astring,
int literalplus_enabled)
{
if (is_ascii(astring) || !literalplus_enabled) {
return mailimap_astring_send(fd, astring);
}
else {
return mailimap_literalplus_send(fd, astring);
}
}
int mailimap_astring_send(mailstream * fd, const char * astring)
{
if (is_atom(astring))
return mailimap_atom_send(fd, astring);
else
return mailimap_quoted_send(fd, astring);
}
static int mailimap_atom_send(mailstream * fd, const char * atom)
{
return mailimap_token_send(fd, atom);
}
int mailimap_authenticate_send(mailstream * fd,
const char * auth_type)
{
int r;
r = mailimap_token_send(fd, "AUTHENTICATE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_auth_type_send(fd, auth_type);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_authenticate_resp_send(mailstream * fd,
const char * base64)
{
int r;
r = mailimap_base64_send(fd, base64);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_auth_type_send(mailstream * fd, const char * auth_type)
{
return mailimap_atom_send(fd, auth_type);
}
static int mailimap_base64_send(mailstream * fd, const char * base64)
{
return mailimap_token_send(fd, base64);
}
int mailimap_capability_send(mailstream * fd)
{
int r;
r = mailimap_token_send(fd, "CAPABILITY");
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_logout_send(mailstream * fd)
{
int r;
r = mailimap_token_send(fd, "LOGOUT");
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_noop_send(mailstream * fd)
{
int r;
r = mailimap_token_send(fd, "NOOP");
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_check_send(mailstream * fd)
{
int r;
r = mailimap_token_send(fd, "CHECK");
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_close_send(mailstream * fd)
{
int r;
r = mailimap_token_send(fd, "CLOSE");
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_expunge_send(mailstream * fd)
{
int r;
r = mailimap_token_send(fd, "EXPUNGE");
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_copy_send(mailstream * fd,
struct mailimap_set * set,
const char * mb)
{
int r;
r = mailimap_token_send(fd, "COPY");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_set_send(fd, set);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_uid_copy_send(mailstream * fd,
struct mailimap_set * set,
const char * mb)
{
int r;
r = mailimap_token_send(fd, "UID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return mailimap_copy_send(fd, set, mb);
}
int mailimap_move_send(mailstream * fd,
struct mailimap_set * set,
const char * mb)
{
int r;
r = mailimap_token_send(fd, "MOVE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_set_send(fd, set);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_uid_move_send(mailstream * fd,
struct mailimap_set * set,
const char * mb)
{
int r;
r = mailimap_token_send(fd, "UID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return mailimap_move_send(fd, set, mb);
}
int mailimap_create_send(mailstream * fd,
const char * mb)
{
int r;
r = mailimap_token_send(fd, "CREATE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_date_send(mailstream * fd,
struct mailimap_date * date)
{
int r;
r = mailimap_date_day_send(fd, date->dt_day);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '-');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_month_send(fd, date->dt_month);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '-');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_year_send(fd, date->dt_year);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_date_day_send(mailstream * fd, int day)
{
return mailimap_number_send(fd, day);
}
static int mailimap_date_day_fixed_send(mailstream * fd, int day)
{
int r;
if (day < 10) {
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_number_send(fd, day);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
else
return mailimap_number_send(fd, day);
}
static int mailimap_date_month_send(mailstream * fd, int month)
{
const char * name;
int r;
name = mailimap_month_get_token_str(month);
if (name == NULL)
return MAILIMAP_ERROR_INVAL;
r = mailimap_token_send(fd, name);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_fixed_digit_send(mailstream * fd,
int num, int count)
{
int r;
if (count == 0)
return MAILIMAP_NO_ERROR;
r = mailimap_fixed_digit_send(fd, num / 10, count - 1);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_digit_send(fd, num % 10);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_date_year_send(mailstream * fd, int year)
{
int r;
r = mailimap_fixed_digit_send(fd, year, 4);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int
mailimap_date_time_send(mailstream * fd,
struct mailimap_date_time * date_time)
{
int r;
int zone;
r = mailimap_dquote_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_day_fixed_send(fd, date_time->dt_day);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '-');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_month_send(fd, date_time->dt_month);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '-');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_year_send(fd, date_time->dt_year);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_fixed_digit_send(fd, date_time->dt_hour, 2);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, ':');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_fixed_digit_send(fd, date_time->dt_min, 2);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, ':');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_fixed_digit_send(fd, date_time->dt_sec, 2);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
if (date_time->dt_zone < 0) {
r = mailimap_char_send(fd, '-');
if (r != MAILIMAP_NO_ERROR)
return r;
zone = -date_time->dt_zone;
}
else {
r = mailimap_char_send(fd, '+');
if (r != MAILIMAP_NO_ERROR)
return r;
zone = date_time->dt_zone;
}
r = mailimap_fixed_digit_send(fd, zone, 4);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_dquote_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_delete_send(mailstream * fd, const char * mb)
{
int r;
r = mailimap_token_send(fd, "DELETE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_digit_send(mailstream * fd, int digit)
{
return mailimap_char_send(fd, digit + '0');
}
int mailimap_examine_send(mailstream * fd, const char * mb, int condstore)
{
int r;
r = mailimap_token_send(fd, "EXAMINE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
if (condstore) {
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_oparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_token_send(fd, "CONDSTORE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_cparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
}
return MAILIMAP_NO_ERROR;
}
static int
mailimap_fetch_att_list_send(mailstream * fd, clist * fetch_att_list);
static int
mailimap_fetch_type_send(mailstream * fd,
struct mailimap_fetch_type * fetch_type)
{
switch (fetch_type->ft_type) {
case MAILIMAP_FETCH_TYPE_ALL:
return mailimap_token_send(fd, "ALL");
case MAILIMAP_FETCH_TYPE_FULL:
return mailimap_token_send(fd, "FULL");
case MAILIMAP_FETCH_TYPE_FAST:
return mailimap_token_send(fd, "FAST");
case MAILIMAP_FETCH_TYPE_FETCH_ATT:
return mailimap_fetch_att_send(fd, fetch_type->ft_data.ft_fetch_att);
case MAILIMAP_FETCH_TYPE_FETCH_ATT_LIST:
return mailimap_fetch_att_list_send(fd,
fetch_type->ft_data.ft_fetch_att_list);
default:
return MAILIMAP_ERROR_INVAL;
}
}
int mailimap_fetch_send(mailstream * fd,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type)
{
int r;
r = mailimap_token_send(fd, "FETCH");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_set_send(fd, set);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_fetch_type_send(fd, fetch_type);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int
mailimap_uid_fetch_send(mailstream * fd,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type)
{
int r;
r = mailimap_token_send(fd, "UID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return mailimap_fetch_send(fd, set, fetch_type);
}
static int
mailimap_fetch_att_list_send(mailstream * fd, clist * fetch_att_list)
{
int r;
r = mailimap_oparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_struct_spaced_list_send(fd, fetch_att_list,
(mailimap_struct_sender *)
mailimap_fetch_att_send);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_cparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_fetch_att_send(mailstream * fd,
struct mailimap_fetch_att * fetch_att)
{
int r;
switch(fetch_att->att_type) {
case MAILIMAP_FETCH_ATT_ENVELOPE:
return mailimap_token_send(fd, "ENVELOPE");
case MAILIMAP_FETCH_ATT_FLAGS:
return mailimap_token_send(fd, "FLAGS");
case MAILIMAP_FETCH_ATT_INTERNALDATE:
return mailimap_token_send(fd, "INTERNALDATE");
case MAILIMAP_FETCH_ATT_RFC822:
return mailimap_token_send(fd, "RFC822");
case MAILIMAP_FETCH_ATT_RFC822_HEADER:
return mailimap_token_send(fd, "RFC822.HEADER");
case MAILIMAP_FETCH_ATT_RFC822_SIZE:
return mailimap_token_send(fd, "RFC822.SIZE");
case MAILIMAP_FETCH_ATT_RFC822_TEXT:
return mailimap_token_send(fd, "RFC822.TEXT");
case MAILIMAP_FETCH_ATT_BODY:
return mailimap_token_send(fd, "BODY");
case MAILIMAP_FETCH_ATT_BODYSTRUCTURE:
return mailimap_token_send(fd, "BODYSTRUCTURE");
case MAILIMAP_FETCH_ATT_UID:
return mailimap_token_send(fd, "UID");
case MAILIMAP_FETCH_ATT_BODY_SECTION:
r = mailimap_token_send(fd, "BODY");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_section_send(fd, fetch_att->att_section);
if (r != MAILIMAP_NO_ERROR)
return r;
if (fetch_att->att_size != 0) {
r = mailimap_char_send(fd, '<');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_number_send(fd, fetch_att->att_offset);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '.');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_number_send(fd, fetch_att->att_size);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '>');
if (r != MAILIMAP_NO_ERROR)
return r;
}
return MAILIMAP_NO_ERROR;
case MAILIMAP_FETCH_ATT_BODY_PEEK_SECTION:
r = mailimap_token_send(fd, "BODY.PEEK");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_section_send(fd, fetch_att->att_section);
if (r != MAILIMAP_NO_ERROR)
return r;
if (fetch_att->att_size != 0) {
r = mailimap_char_send(fd, '<');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_number_send(fd, fetch_att->att_offset);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '.');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_number_send(fd, fetch_att->att_size);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '>');
if (r != MAILIMAP_NO_ERROR)
return r;
}
return MAILIMAP_NO_ERROR;
case MAILIMAP_FETCH_ATT_EXTENSION:
r = mailimap_token_send(fd, fetch_att->att_extension);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
default:
return MAILIMAP_ERROR_INVAL;
}
}
static int mailimap_flag_send(mailstream * fd,
struct mailimap_flag * flag)
{
switch(flag->fl_type) {
case MAILIMAP_FLAG_ANSWERED:
return mailimap_token_send(fd, "\\Answered");
case MAILIMAP_FLAG_FLAGGED:
return mailimap_token_send(fd, "\\Flagged");
case MAILIMAP_FLAG_DELETED:
return mailimap_token_send(fd, "\\Deleted");
case MAILIMAP_FLAG_SEEN:
return mailimap_token_send(fd, "\\Seen");
case MAILIMAP_FLAG_DRAFT:
return mailimap_token_send(fd, "\\Draft");
case MAILIMAP_FLAG_KEYWORD:
return mailimap_flag_keyword_send(fd, flag->fl_data.fl_keyword);
case MAILIMAP_FLAG_EXTENSION:
return mailimap_flag_extension_send(fd, flag->fl_data.fl_extension);
default:
return MAILIMAP_ERROR_INVAL;
}
}
static int mailimap_flag_extension_send(mailstream * fd,
const char * flag_extension)
{
int r;
r = mailimap_char_send(fd, '\\');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_atom_send(fd, flag_extension);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_flag_keyword_send(mailstream * fd,
const char * flag_keyword)
{
return mailimap_token_send(fd, flag_keyword);
}
static int mailimap_flag_list_send(mailstream * fd,
struct mailimap_flag_list * flag_list)
{
int r;
r = mailimap_oparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
if (flag_list->fl_list != NULL) {
r = mailimap_struct_spaced_list_send(fd, flag_list->fl_list,
(mailimap_struct_sender *) mailimap_flag_send);
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_cparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_header_fld_name_send(mailstream * fd, const char * header)
{
return mailimap_astring_send(fd, header);
}
static int
mailimap_header_list_send(mailstream * fd,
struct mailimap_header_list * header_list)
{
int r;
r = mailimap_oparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_struct_spaced_list_send(fd, header_list->hdr_list,
(mailimap_struct_sender *) mailimap_header_fld_name_send);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_cparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_list_send(mailstream * fd,
const char * mb,
const char * list_mb)
{
int r;
r = mailimap_token_send(fd, "LIST");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_list_mailbox_send(fd, list_mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int
mailimap_list_mailbox_send(mailstream * fd, const char * pattern)
{
return mailimap_quoted_send(fd, pattern);
}
int
mailimap_literal_send(mailstream * fd, const char * literal,
size_t progr_rate,
progress_function * progr_fun)
{
size_t len;
size_t literal_len;
int r;
len = strlen(literal);
literal_len = mailstream_get_data_crlf_size(literal, len);
r = mailimap_literal_count_send(fd, literal_len);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_literal_data_send(fd, literal, len, progr_rate, progr_fun);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int literal_count_send(mailstream * fd, size_t count, int literalplus_enabled)
{
int r;
r = mailimap_char_send(fd, '{');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_number_send(fd, (uint32_t) count);
if (r != MAILIMAP_NO_ERROR)
return r;
if (literalplus_enabled) {
r = mailimap_char_send(fd, '+');
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_char_send(fd, '}');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_crlf_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int
mailimap_literal_count_send(mailstream * fd, size_t count)
{
return literal_count_send(fd, count, 0);
}
static int
mailimap_literalplus_count_send(mailstream * fd, size_t count)
{
return literal_count_send(fd, count, 1);
}
int
mailimap_literal_data_send(mailstream * fd, const char * literal, size_t len,
size_t progr_rate,
progress_function * progr_fun)
{
return mailimap_literal_data_send_with_context(fd, literal, len, NULL, NULL);
}
int
mailimap_literal_data_send_with_context(mailstream * fd, const char * literal, size_t len,
mailprogress_function * progr_fun,
void * context)
{
int r;
r = mailimap_sized_token_send_with_context(fd, literal, len, progr_fun, context);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_login_send(mailstream * fd,
const char * userid, const char * password)
{
int r;
r = mailimap_token_send(fd, "LOGIN");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_userid_send(fd, userid);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_password_send(fd, password);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_lsub_send(mailstream * fd,
const char * mb, const char * list_mb)
{
int r;
r = mailimap_token_send(fd, "LSUB");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_list_mailbox_send(fd, list_mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_mailbox_send(mailstream * fd, const char * mb)
{
return mailimap_astring_send(fd, mb);
}
int mailimap_number_send(mailstream * fd, uint32_t number)
{
int r;
if (number / 10 != 0) {
r = mailimap_number_send(fd, number / 10);
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_digit_send(fd, number % 10);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_password_send(mailstream * fd, const char * pass)
{
return mailimap_astring_send(fd, pass);
}
static int is_quoted_specials(char ch)
{
return (ch == '\"') || (ch == '\\');
}
static int mailimap_quoted_char_send(mailstream * fd, char ch)
{
int r;
if (is_quoted_specials(ch)) {
r = mailimap_char_send(fd, '\\');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, ch);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
else
return mailimap_char_send(fd, ch);
}
int mailimap_quoted_send(mailstream * fd, const char * quoted)
{
const char * pos;
int r;
pos = quoted;
r = mailimap_dquote_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
while (* pos != 0) {
r = mailimap_quoted_char_send(fd, * pos);
if (r != MAILIMAP_NO_ERROR)
return r;
pos ++;
}
r = mailimap_dquote_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_rename_send(mailstream * fd, const char * mb,
const char * new_name)
{
int r;
r = mailimap_token_send(fd, "RENAME");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, new_name);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int is_ascii(const char * str)
{
const char * p = str;
while (* p != 0) {
if ((unsigned char) * p >= 128) {
return 0;
}
p ++;
}
return 1;
}
static int mailimap_search_key_need_to_send_charset(struct mailimap_search_key * key)
{
clistiter * cur;
struct mailimap_search_key * elt;
int r;
switch (key->sk_type) {
case MAILIMAP_SEARCH_KEY_ALL:
return 0;
case MAILIMAP_SEARCH_KEY_ANSWERED:
return 0;
case MAILIMAP_SEARCH_KEY_BCC:
return !is_ascii(key->sk_data.sk_bcc);
case MAILIMAP_SEARCH_KEY_BEFORE:
return 0;
case MAILIMAP_SEARCH_KEY_BODY:
return !is_ascii(key->sk_data.sk_body);
case MAILIMAP_SEARCH_KEY_CC:
return !is_ascii(key->sk_data.sk_cc);
case MAILIMAP_SEARCH_KEY_DELETED:
return 0;
case MAILIMAP_SEARCH_KEY_FLAGGED:
return 0;
case MAILIMAP_SEARCH_KEY_FROM:
return !is_ascii(key->sk_data.sk_from);
case MAILIMAP_SEARCH_KEY_KEYWORD:
return !is_ascii(key->sk_data.sk_keyword);
case MAILIMAP_SEARCH_KEY_NEW:
return 0;
case MAILIMAP_SEARCH_KEY_OLD:
return 0;
case MAILIMAP_SEARCH_KEY_ON:
return 0;
case MAILIMAP_SEARCH_KEY_RECENT:
return 0;
case MAILIMAP_SEARCH_KEY_SEEN:
return 0;
case MAILIMAP_SEARCH_KEY_SINCE:
return 0;
case MAILIMAP_SEARCH_KEY_SUBJECT:
return !is_ascii(key->sk_data.sk_subject);
case MAILIMAP_SEARCH_KEY_TEXT:
return !is_ascii(key->sk_data.sk_text);
case MAILIMAP_SEARCH_KEY_TO:
return !is_ascii(key->sk_data.sk_to);
case MAILIMAP_SEARCH_KEY_UNANSWERED:
return 0;
case MAILIMAP_SEARCH_KEY_UNDELETED:
return 0;
case MAILIMAP_SEARCH_KEY_UNFLAGGED:
return 0;
case MAILIMAP_SEARCH_KEY_UNKEYWORD:
return !is_ascii(key->sk_data.sk_unkeyword);
case MAILIMAP_SEARCH_KEY_UNSEEN:
return 0;
case MAILIMAP_SEARCH_KEY_DRAFT:
return 0;
case MAILIMAP_SEARCH_KEY_HEADER:
return !is_ascii(key->sk_data.sk_header.sk_header_name) || !is_ascii(key->sk_data.sk_header.sk_header_value);
case MAILIMAP_SEARCH_KEY_LARGER:
return 0;
case MAILIMAP_SEARCH_KEY_NOT:
return mailimap_search_key_need_to_send_charset(key->sk_data.sk_not);
case MAILIMAP_SEARCH_KEY_OR:
return mailimap_search_key_need_to_send_charset(key->sk_data.sk_or.sk_or1) ||
mailimap_search_key_need_to_send_charset(key->sk_data.sk_or.sk_or2);
case MAILIMAP_SEARCH_KEY_SENTBEFORE:
return 0;
case MAILIMAP_SEARCH_KEY_SENTON:
return 0;
case MAILIMAP_SEARCH_KEY_SENTSINCE:
return 0;
case MAILIMAP_SEARCH_KEY_SMALLER:
return 0;
case MAILIMAP_SEARCH_KEY_UID:
return 0;
case MAILIMAP_SEARCH_KEY_UNDRAFT:
return 0;
case MAILIMAP_SEARCH_KEY_SET:
return 0;
case MAILIMAP_SEARCH_KEY_XGMTHRID:
return 0;
case MAILIMAP_SEARCH_KEY_MULTIPLE:
{
cur = clist_begin(key->sk_data.sk_multiple);
if (cur == NULL)
return 0;
elt = (struct mailimap_search_key *) clist_content(cur);
r = mailimap_search_key_need_to_send_charset(elt);
if (r != 0)
return r;
cur = clist_next(cur);
while (cur != NULL) {
elt = (struct mailimap_search_key *) clist_content(cur);
r = mailimap_search_key_need_to_send_charset(elt);
if (r != 0)
return r;
cur = clist_next(cur);
}
return 0;
}
case MAILIMAP_SEARCH_KEY_MODSEQ:
return 0;
case MAILIMAP_SEARCH_KEY_XGMMSGID:
return 0;
default:
return 1;
}
}
int
mailimap_search_send(mailstream * fd, const char * charset,
struct mailimap_search_key * key)
{
int r;
int needToSendCharset = 1;
r = mailimap_token_send(fd, "SEARCH");
if (r != MAILIMAP_NO_ERROR)
return r;
needToSendCharset = mailimap_search_key_need_to_send_charset(key);
if (charset != NULL && needToSendCharset) {
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_token_send(fd, "CHARSET");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_send(fd, charset);
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_search_key_send(fd, key);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int
mailimap_uid_search_send(mailstream * fd, const char * charset,
struct mailimap_search_key * key)
{
int r;
r = mailimap_token_send(fd, "UID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return mailimap_search_send(fd, charset, key);
}
int mailimap_search_literalplus_send(mailstream * fd, const char * charset,
struct mailimap_search_key * key)
{
int r;
int needToSendCharset = 1;
r = mailimap_token_send(fd, "SEARCH");
if (r != MAILIMAP_NO_ERROR)
return r;
needToSendCharset = mailimap_search_key_need_to_send_charset(key);
if (charset != NULL && needToSendCharset) {
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_token_send(fd, "CHARSET");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_send(fd, charset);
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_search_key_literalplus_send(fd, key);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_uid_search_literalplus_send(mailstream * fd, const char * charset,
struct mailimap_search_key * key)
{
int r;
r = mailimap_token_send(fd, "UID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return mailimap_search_literalplus_send(fd, charset, key);
}
static int search_key_send(mailstream * fd,
struct mailimap_search_key * key,
int literalplus_enabled, int toplevel)
{
int r;
switch (key->sk_type) {
case MAILIMAP_SEARCH_KEY_ALL:
return mailimap_token_send(fd, "ALL");
case MAILIMAP_SEARCH_KEY_ANSWERED:
return mailimap_token_send(fd, "ANSWERED");
case MAILIMAP_SEARCH_KEY_BCC:
r = mailimap_token_send(fd, "BCC");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_bcc, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_BEFORE:
r = mailimap_token_send(fd, "BEFORE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_send(fd, key->sk_data.sk_before);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_BODY:
r = mailimap_token_send(fd, "BODY");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_body, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_CC:
r = mailimap_token_send(fd, "CC");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_cc, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_DELETED:
return mailimap_token_send(fd, "DELETED");
case MAILIMAP_SEARCH_KEY_FLAGGED:
return mailimap_token_send(fd, "FLAGGED");
case MAILIMAP_SEARCH_KEY_FROM:
r = mailimap_token_send(fd, "FROM");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_from, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_KEYWORD:
r = mailimap_token_send(fd, "KEYWORD");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_flag_keyword_send(fd, key->sk_data.sk_keyword);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_NEW:
return mailimap_token_send(fd, "NEW");
case MAILIMAP_SEARCH_KEY_OLD:
return mailimap_token_send(fd, "OLD");
case MAILIMAP_SEARCH_KEY_ON:
r = mailimap_token_send(fd, "ON");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_send(fd, key->sk_data.sk_on);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_RECENT:
return mailimap_token_send(fd, "RECENT");
case MAILIMAP_SEARCH_KEY_SEEN:
return mailimap_token_send(fd, "SEEN");
case MAILIMAP_SEARCH_KEY_SINCE:
r = mailimap_token_send(fd, "SINCE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_send(fd, key->sk_data.sk_since);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_SUBJECT:
r = mailimap_token_send(fd, "SUBJECT");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_subject, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_TEXT:
r = mailimap_token_send(fd, "TEXT");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_text, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_TO:
r = mailimap_token_send(fd, "TO");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_text, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_UNANSWERED:
return mailimap_token_send(fd, "UNANSWERED");
case MAILIMAP_SEARCH_KEY_UNDELETED:
return mailimap_token_send(fd, "UNDELETED");
case MAILIMAP_SEARCH_KEY_UNFLAGGED:
return mailimap_token_send(fd, "UNFLAGGED");
case MAILIMAP_SEARCH_KEY_UNKEYWORD:
r = mailimap_token_send(fd, "UNKEYWORD");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_unkeyword, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_UNSEEN:
return mailimap_token_send(fd, "UNSEEN");
case MAILIMAP_SEARCH_KEY_DRAFT:
return mailimap_token_send(fd, "DRAFT");
case MAILIMAP_SEARCH_KEY_HEADER:
r = mailimap_token_send(fd, "HEADER");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_header_fld_name_send(fd, key->sk_data.sk_header.sk_header_name);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_header.sk_header_value,
literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_LARGER:
r = mailimap_token_send(fd, "LARGER");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_number_send(fd, key->sk_data.sk_larger);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_NOT:
r = mailimap_token_send(fd, "NOT");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = search_key_send(fd, key->sk_data.sk_not, literalplus_enabled, 0);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_OR:
r = mailimap_token_send(fd, "OR");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = search_key_send(fd, key->sk_data.sk_or.sk_or1, literalplus_enabled, 0);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = search_key_send(fd, key->sk_data.sk_or.sk_or2, literalplus_enabled, 0);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_SENTBEFORE:
r = mailimap_token_send(fd, "SENTBEFORE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_send(fd, key->sk_data.sk_sentbefore);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_SENTON:
r = mailimap_token_send(fd, "SENTON");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_send(fd, key->sk_data.sk_senton);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_SENTSINCE:
r = mailimap_token_send(fd, "SENTSINCE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_date_send(fd, key->sk_data.sk_sentsince);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_SMALLER:
r = mailimap_token_send(fd, "SMALLER");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_number_send(fd, key->sk_data.sk_smaller);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_UID:
r = mailimap_token_send(fd, "UID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_set_send(fd, key->sk_data.sk_set);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_UNDRAFT:
return mailimap_token_send(fd, "UNDRAFT");
case MAILIMAP_SEARCH_KEY_SET:
return mailimap_set_send(fd, key->sk_data.sk_set);
case MAILIMAP_SEARCH_KEY_XGMTHRID:
r = mailimap_token_send(fd, "X-GM-THRID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_uint64_send(fd, key->sk_data.sk_xgmthrid);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_XGMMSGID:
r = mailimap_token_send(fd, "X-GM-MSGID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_uint64_send(fd, key->sk_data.sk_xgmmsgid);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_XGMRAW:
r = mailimap_token_send(fd, "X-GM-RAW");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_literalplus_send(fd, key->sk_data.sk_xgmraw, literalplus_enabled);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_MULTIPLE:
if (!toplevel) {
r = mailimap_oparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
}
if (literalplus_enabled) {
r = mailimap_struct_spaced_list_send(fd, key->sk_data.sk_multiple,
(mailimap_struct_sender *) search_key_literalplus_notoplevel_send);
}
else {
r = mailimap_struct_spaced_list_send(fd, key->sk_data.sk_multiple,
(mailimap_struct_sender *) search_key_notoplevel_send);
}
if (r != MAILIMAP_NO_ERROR)
return r;
if (!toplevel) {
r = mailimap_cparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
}
return MAILIMAP_NO_ERROR;
case MAILIMAP_SEARCH_KEY_MODSEQ:
r = mailimap_token_send(fd, "MODSEQ");
if (r != MAILIMAP_NO_ERROR)
return r;
if (key->sk_data.sk_modseq.sk_entry_name != NULL) {
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
mailimap_dquote_send(fd);
r = mailimap_token_send(fd, "/flags/");
if (r != MAILIMAP_NO_ERROR)
return r;
if (key->sk_data.sk_modseq.sk_entry_name->fl_type != MAILIMAP_FLAG_KEYWORD) {
r = mailimap_token_send(fd, "\\");
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_flag_send(fd, key->sk_data.sk_modseq.sk_entry_name);
if (r != MAILIMAP_NO_ERROR)
return r;
mailimap_dquote_send(fd);
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
switch (key->sk_data.sk_modseq.sk_entry_type_req) {
case MAILIMAP_SEARCH_KEY_MODSEQ_ENTRY_TYPE_REQ_PRIV:
r = mailimap_token_send(fd, "priv");
if (r != MAILIMAP_NO_ERROR)
return r;
break;
case MAILIMAP_SEARCH_KEY_MODSEQ_ENTRY_TYPE_REQ_SHARED:
r = mailimap_token_send(fd, "shared");
if (r != MAILIMAP_NO_ERROR)
return r;
break;
case MAILIMAP_SEARCH_KEY_MODSEQ_ENTRY_TYPE_REQ_ALL:
r = mailimap_token_send(fd, "all");
if (r != MAILIMAP_NO_ERROR)
return r;
break;
}
}
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mod_sequence_value_send(fd, key->sk_data.sk_modseq.sk_modseq_valzer);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
default:
return MAILIMAP_ERROR_INVAL;
}
}
static int search_key_notoplevel_send(mailstream * fd,
struct mailimap_search_key * key)
{
return search_key_send(fd, key, 0, 0);
}
static int search_key_literalplus_notoplevel_send(mailstream * fd,
struct mailimap_search_key * key)
{
return search_key_send(fd, key, 1, 0);
}
int mailimap_search_key_send(mailstream * fd,
struct mailimap_search_key * key)
{
return search_key_send(fd, key, 0, 1);
}
int mailimap_search_key_literalplus_send(mailstream * fd,
struct mailimap_search_key * key)
{
return search_key_send(fd, key, 1, 1);
}
int mailimap_mod_sequence_value_send(mailstream * fd, uint64_t number)
{
return mailimap_uint64_send(fd, number);
}
int mailimap_uint64_send(mailstream * fd, uint64_t number)
{
char numberval[30];
snprintf(numberval, sizeof(numberval), "%llu", (long long unsigned) number);
return mailimap_token_send(fd, numberval);
}
static int
mailimap_section_send(mailstream * fd,
struct mailimap_section * section)
{
int r;
r = mailimap_char_send(fd, '[');
if (r != MAILIMAP_NO_ERROR)
return r;
if (section != NULL) {
if (section->sec_spec != NULL) {
r = mailimap_section_spec_send(fd, section->sec_spec);
if (r != MAILIMAP_NO_ERROR)
return r;
}
}
r = mailimap_char_send(fd, ']');
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int
mailimap_section_msgtext_send(mailstream * fd,
struct mailimap_section_msgtext *
section_msgtext)
{
int r;
switch (section_msgtext->sec_type) {
case MAILIMAP_SECTION_MSGTEXT_HEADER:
return mailimap_token_send(fd, "HEADER");
case MAILIMAP_SECTION_MSGTEXT_HEADER_FIELDS:
r = mailimap_token_send(fd, "HEADER.FIELDS");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_header_list_send(fd, section_msgtext->sec_header_list);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SECTION_MSGTEXT_HEADER_FIELDS_NOT:
r = mailimap_token_send(fd, "HEADER.FIELDS.NOT");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_header_list_send(fd, section_msgtext->sec_header_list);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
case MAILIMAP_SECTION_MSGTEXT_TEXT:
return mailimap_token_send(fd, "TEXT");
default:
return MAILIMAP_ERROR_INVAL;
}
}
static int
mailimap_pnumber_send(mailstream * fd, uint32_t * pnumber)
{
return mailimap_number_send(fd, * pnumber);
}
static int
mailimap_section_part_send(mailstream * fd,
struct mailimap_section_part * section)
{
int r;
r = mailimap_struct_list_send(fd, section->sec_id, '.',
(mailimap_struct_sender *) mailimap_pnumber_send);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int
mailimap_section_spec_send(mailstream * fd,
struct mailimap_section_spec * section_spec)
{
int r;
switch (section_spec->sec_type) {
case MAILIMAP_SECTION_SPEC_SECTION_MSGTEXT:
return mailimap_section_msgtext_send(fd,
section_spec->sec_data.sec_msgtext);
case MAILIMAP_SECTION_SPEC_SECTION_PART:
r = mailimap_section_part_send(fd, section_spec->sec_data.sec_part);
if (r != MAILIMAP_NO_ERROR)
return r;
if (section_spec->sec_text != NULL) {
r = mailimap_char_send(fd, '.');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_section_text_send(fd,
section_spec->sec_text);
if (r != MAILIMAP_NO_ERROR)
return r;
}
return MAILIMAP_NO_ERROR;
default:
return MAILIMAP_ERROR_INVAL;
}
}
static int
mailimap_section_text_send(mailstream * fd,
struct mailimap_section_text * section_text)
{
switch (section_text->sec_type) {
case MAILIMAP_SECTION_TEXT_SECTION_MSGTEXT:
return mailimap_section_msgtext_send(fd, section_text->sec_msgtext);
case MAILIMAP_SECTION_TEXT_MIME:
return mailimap_token_send(fd, "MIME");
default:
return MAILIMAP_NO_ERROR;
}
}
int
mailimap_select_send(mailstream * fd, const char * mb, int condstore)
{
int r;
r = mailimap_token_send(fd, "SELECT");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
if (condstore) {
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_oparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_token_send(fd, "CONDSTORE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_cparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
}
return MAILIMAP_NO_ERROR;
}
static int
mailimap_sequence_num_send(mailstream * fd, uint32_t sequence_num)
{
if (sequence_num == 0)
return mailimap_char_send(fd, '*');
else
return mailimap_number_send(fd, sequence_num);
}
static int mailimap_set_item_send(mailstream * fd,
struct mailimap_set_item * item)
{
int r;
if (item->set_first == item->set_last)
return mailimap_sequence_num_send(fd, item->set_first);
else {
r = mailimap_sequence_num_send(fd, item->set_first);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, ':');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_sequence_num_send(fd, item->set_last);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
}
int mailimap_set_send(mailstream * fd,
struct mailimap_set * set)
{
return mailimap_struct_list_send(fd, set->set_list, ',',
(mailimap_struct_sender *) mailimap_set_item_send);
}
static int
mailimap_status_att_list_send(mailstream * fd,
struct mailimap_status_att_list * status_att_list)
{
return mailimap_struct_spaced_list_send(fd, status_att_list->att_list,
(mailimap_struct_sender *) mailimap_status_att_send);
}
int
mailimap_status_send(mailstream * fd, const char * mb,
struct mailimap_status_att_list * status_att_list)
{
int r;
r = mailimap_token_send(fd, "STATUS");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, '(');
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_status_att_list_send(fd, status_att_list);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_char_send(fd, ')');
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_status_att_send(mailstream * fd, int * status_att)
{
const char * token;
token = mailimap_status_att_get_token_str(* status_att);
if (token == NULL) {
return MAILIMAP_ERROR_INVAL;
}
return mailimap_token_send(fd, token);
}
int
mailimap_store_send(mailstream * fd,
struct mailimap_set * set, int use_unchangedsince, uint64_t mod_sequence_valzer,
struct mailimap_store_att_flags * store_att_flags)
{
int r;
r = mailimap_token_send(fd, "STORE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_set_send(fd, set);
if (r != MAILIMAP_NO_ERROR)
return r;
if (use_unchangedsince) {
r = mailimap_oparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_token_send(fd, "UNCHANGEDSINCE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mod_sequence_value_send(fd, mod_sequence_valzer);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_cparenth_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_store_att_flags_send(fd, store_att_flags);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int
mailimap_uid_store_send(mailstream * fd,
struct mailimap_set * set, int use_unchangedsince, uint64_t mod_sequence_valzer,
struct mailimap_store_att_flags * store_att_flags)
{
int r;
r = mailimap_token_send(fd, "UID");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return mailimap_store_send(fd, set,
use_unchangedsince, mod_sequence_valzer, store_att_flags);
}
static int
mailimap_store_att_flags_send(mailstream * fd,
struct mailimap_store_att_flags * store_flags)
{
int r;
switch (store_flags->fl_sign) {
case 1:
r = mailimap_char_send(fd, '+');
if (r != MAILIMAP_NO_ERROR)
return r;
break;
case -1:
r = mailimap_char_send(fd, '-');
if (r != MAILIMAP_NO_ERROR)
return r;
break;
}
r = mailimap_token_send(fd, "FLAGS");
if (r != MAILIMAP_NO_ERROR)
return r;
if (store_flags->fl_silent) {
r = mailimap_token_send(fd, ".SILENT");
if (r != MAILIMAP_NO_ERROR)
return r;
}
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_flag_list_send(fd, store_flags->fl_flag_list);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_subscribe_send(mailstream * fd, const char * mb)
{
int r;
r = mailimap_token_send(fd, "SUBSCRIBE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_tag_send(mailstream * fd, const char * tag)
{
return mailimap_token_send(fd, tag);
}
int mailimap_unsubscribe_send(mailstream * fd,
const char * mb)
{
int r;
r = mailimap_token_send(fd, "UNSUBSCRIBE");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_starttls_send(mailstream * fd)
{
return mailimap_token_send(fd, "STARTTLS");
}
int
mailimap_send_custom_command(mailstream *fd, const char * command)
{
int r;
r = mailimap_token_send(fd, command);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
static int mailimap_userid_send(mailstream * fd, const char * user)
{
return mailimap_astring_send(fd, user);
}