#ifndef MAILPOP3_TYPES_H
#define MAILPOP3_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailstream.h>
#include <libetpan/mmapstring.h>
#include <libetpan/carray.h>
#include <libetpan/clist.h>
enum {
MAILPOP3_NO_ERROR = 0,
MAILPOP3_ERROR_BAD_STATE,
MAILPOP3_ERROR_UNAUTHORIZED,
MAILPOP3_ERROR_STREAM,
MAILPOP3_ERROR_DENIED,
MAILPOP3_ERROR_BAD_USER,
MAILPOP3_ERROR_BAD_PASSWORD,
MAILPOP3_ERROR_CANT_LIST,
MAILPOP3_ERROR_NO_SUCH_MESSAGE,
MAILPOP3_ERROR_MEMORY,
MAILPOP3_ERROR_CONNECTION_REFUSED,
MAILPOP3_ERROR_APOP_NOT_SUPPORTED,
MAILPOP3_ERROR_CAPA_NOT_SUPPORTED,
MAILPOP3_ERROR_STLS_NOT_SUPPORTED,
MAILPOP3_ERROR_SSL,
MAILPOP3_ERROR_QUIT_FAILED
};
typedef struct mailpop3 mailpop3;
struct mailpop3
{
char * pop3_response;
char * pop3_timestamp;
mailstream * pop3_stream;
size_t pop3_progr_rate;
progress_function * pop3_progr_fun;
MMAPString * pop3_stream_buffer;
MMAPString * pop3_response_buffer;
carray * pop3_msg_tab;
int pop3_state;
unsigned int pop3_deleted_count;
struct {
void * sasl_conn;
const char * sasl_server_fqdn;
const char * sasl_login;
const char * sasl_auth_name;
const char * sasl_password;
const char * sasl_realm;
void * sasl_secret;
} pop3_sasl;
time_t pop3_timeout;
mailprogress_function * pop3_progress_fun;
void * pop3_progress_context;
void (* pop3_logger)(mailpop3 * session, int log_type, const char * str, size_t size, void * context);
void * pop3_logger_context;
};
struct mailpop3_msg_info
{
unsigned int msg_index;
uint32_t msg_size;
char * msg_uidl;
int msg_deleted;
};
struct mailpop3_capa {
char * cap_name;
clist * cap_param;
};
struct mailpop3_stat_response {
unsigned int msgs_count;
size_t msgs_size;
};
#ifdef __cplusplus
}
#endif
#endif