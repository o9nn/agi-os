#ifndef NEWSNNTP_TYPES_H
#define NEWSNNTP_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/clist.h>
#include <libetpan/mailstream.h>
#include <libetpan/mmapstring.h>
enum {
NEWSNNTP_NO_ERROR = 0,
NEWSNNTP_WARNING_REQUEST_AUTHORIZATION_USERNAME=1,
NEWSNNTP_ERROR_REQUEST_AUTHORIZATION_USERNAME=1,
NEWSNNTP_WARNING_REQUEST_AUTHORIZATION_PASSWORD,
NEWSNNTP_ERROR_STREAM,
NEWSNNTP_ERROR_UNEXPECTED,
NEWSNNTP_ERROR_NO_NEWSGROUP_SELECTED,
NEWSNNTP_ERROR_NO_ARTICLE_SELECTED,
NEWSNNTP_ERROR_INVALID_ARTICLE_NUMBER,
NEWSNNTP_ERROR_ARTICLE_NOT_FOUND,
NEWSNNTP_ERROR_UNEXPECTED_RESPONSE,
NEWSNNTP_ERROR_INVALID_RESPONSE,
NEWSNNTP_ERROR_NO_SUCH_NEWS_GROUP,
NEWSNNTP_ERROR_POSTING_NOT_ALLOWED,
NEWSNNTP_ERROR_POSTING_FAILED,
NEWSNNTP_ERROR_PROGRAM_ERROR,
NEWSNNTP_ERROR_NO_PERMISSION,
NEWSNNTP_ERROR_COMMAND_NOT_UNDERSTOOD,
NEWSNNTP_ERROR_COMMAND_NOT_SUPPORTED,
NEWSNNTP_ERROR_CONNECTION_REFUSED,
NEWSNNTP_ERROR_MEMORY,
NEWSNNTP_ERROR_AUTHENTICATION_REJECTED,
NEWSNNTP_ERROR_BAD_STATE,
NEWSNNTP_ERROR_SSL,
NEWSNNTP_ERROR_AUTHENTICATION_OUT_OF_SEQUENCE,
};
typedef struct newsnntp newsnntp;
struct newsnntp
{
mailstream * nntp_stream;
int nntp_readonly;
size_t nntp_progr_rate;
progress_function * nntp_progr_fun;
MMAPString * nntp_stream_buffer;
MMAPString * nntp_response_buffer;
char * nntp_response;
time_t nntp_timeout;
void (* nntp_logger)(newsnntp * session, int log_type, const char * str, size_t size, void * context);
void * nntp_logger_context;
mailprogress_function * nntp_progress_fun;
void * nntp_progress_context;
};
struct newsnntp_group_info
{
char * grp_name;
uint32_t grp_first;
uint32_t grp_last;
uint32_t grp_count;
char grp_type;
};
struct newsnntp_group_time {
char * grp_name;
time_t grp_date;
char * grp_email;
};
struct newsnntp_distrib_value_meaning {
char * dst_value;
char * dst_meaning;
};
struct newsnntp_distrib_default_value {
uint32_t dst_weight;
char * dst_group_pattern;
char * dst_value;
};
struct newsnntp_group_description {
char * grp_name;
char * grp_description;
};
struct newsnntp_xhdr_resp_item {
uint32_t hdr_article;
char * hdr_value;
};
struct newsnntp_xover_resp_item {
uint32_t ovr_article;
char * ovr_subject;
char * ovr_author;
char * ovr_date;
char * ovr_message_id;
char * ovr_references;
size_t ovr_size;
uint32_t ovr_line_count;
clist * ovr_others;
};
#ifdef __cplusplus
}
#endif
#endif