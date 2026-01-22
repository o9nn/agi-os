#ifndef MAILIMAP_COMMON_H
#define MAILIMAP_COMMON_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mailstream.h"
int mailimap_char_parse(mailstream * fd, MMAPString * buffer,
size_t * indx, char token);
int mailimap_space_parse(mailstream * fd, MMAPString * buffer,
size_t * indx);
int mailimap_token_case_insensitive_parse(mailstream * fd,
MMAPString * buffer,
size_t * indx,
const char * token);
int mailimap_status_att_get_token_value(mailstream * fd, MMAPString * buffer,
size_t * indx);
const char * mailimap_status_att_get_token_str(int indx);
int mailimap_month_get_token_value(mailstream * fd, MMAPString * buffer,
size_t * indx);
const char * mailimap_month_get_token_str(int indx);
int mailimap_flag_get_token_value(mailstream * fd, MMAPString * buffer,
size_t * indx);
const char * mailimap_flag_get_token_str(int indx);
int mailimap_encoding_get_token_value(mailstream * fd, MMAPString * buffer,
size_t * indx);
int mailimap_mbx_list_sflag_get_token_value(mailstream * fd,
MMAPString * buffer,
size_t * indx);
int mailimap_media_basic_get_token_value(mailstream * fd, MMAPString * buffer,
size_t * indx);
int mailimap_resp_cond_state_get_token_value(mailstream * fd,
MMAPString * buffer,
size_t * indx);
int mailimap_resp_text_code_1_get_token_value(mailstream * fd,
MMAPString * buffer,
size_t * indx);
int mailimap_resp_text_code_2_get_token_value(mailstream * fd,
MMAPString * buffer,
size_t * indx);
int mailimap_section_msgtext_get_token_value(mailstream * fd,
MMAPString * buffer,
size_t * indx);
#ifdef __cplusplus
}
#endif
#endif