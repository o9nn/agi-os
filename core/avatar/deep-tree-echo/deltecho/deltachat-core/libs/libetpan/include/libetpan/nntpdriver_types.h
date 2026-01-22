#ifndef NNTPDRIVER_TYPES_H
#define NNTPDRIVER_TYPES_H
#include <libetpan/libetpan-config.h>
#include <libetpan/maildriver_types.h>
#include <libetpan/newsnntp.h>
#include <libetpan/clist.h>
#include <libetpan/generic_cache_types.h>
#include <libetpan/mailstorage_types.h>
#ifdef __cplusplus
extern "C" {
#endif
enum {
NNTPDRIVER_SET_MAX_ARTICLES = 1
};
struct nntp_session_state_data {
newsnntp * nntp_session;
char * nntp_userid;
char * nntp_password;
struct newsnntp_group_info * nntp_group_info;
char * nntp_group_name;
clist * nntp_subscribed_list;
uint32_t nntp_max_articles;
int nntp_mode_reader;
};
enum {
NNTPDRIVER_CACHED_SET_MAX_ARTICLES = 1,
NNTPDRIVER_CACHED_SET_CACHE_DIRECTORY,
NNTPDRIVER_CACHED_SET_FLAGS_DIRECTORY
};
struct nntp_cached_session_state_data {
mailsession * nntp_ancestor;
char nntp_cache_directory[PATH_MAX];
char nntp_flags_directory[PATH_MAX];
struct mail_flags_store * nntp_flags_store;
};
struct nntp_mailstorage {
char * nntp_servername;
uint16_t nntp_port;
char * nntp_command;
int nntp_connection_type;
int nntp_auth_type;
char * nntp_login;
char * nntp_password;
int nntp_cached;
char * nntp_cache_directory;
char * nntp_flags_directory;
char * nntp_local_address;
uint16_t nntp_local_port;
};
enum {
NNTP_AUTH_TYPE_PLAIN
};
#ifdef __cplusplus
}
#endif
#endif