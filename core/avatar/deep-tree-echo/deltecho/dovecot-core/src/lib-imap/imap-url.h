#ifndef IMAP_URL_H
#define IMAP_URL_H
#include "uri-util.h"
struct imap_url {
struct uri_host host;
in_port_t port;
const char *userid;
const char *auth_type;
const char *mailbox;
uint32_t uidvalidity;
uint32_t uid;
const char *section;
uoff_t partial_offset;
uoff_t partial_size;
const char *search_program;
const char *uauth_rumpurl;
const char *uauth_access_application;
const char *uauth_access_user;
const char *uauth_mechanism;
const unsigned char *uauth_token;
size_t uauth_token_size;
time_t uauth_expire;
bool have_partial:1;
};
enum imap_url_parse_flags {
IMAP_URL_PARSE_SCHEME_EXTERNAL	= 0x01,
IMAP_URL_PARSE_REQUIRE_RELATIVE	= 0x02,
IMAP_URL_PARSE_ALLOW_URLAUTH	= 0x04
};
int imap_url_parse_prefix(const char *url, const struct imap_url *base,
enum imap_url_parse_flags flags, const char **end_r,
struct imap_url **url_r, const char **error_r)
ATTR_NULL(2, 4);
static inline int
imap_url_parse(const char *url, const struct imap_url *base,
enum imap_url_parse_flags flags, struct imap_url **url_r,
const char **error_r) ATTR_NULL(2)
{
return imap_url_parse_prefix(url, base, flags, NULL, url_r, error_r);
}
const char *imap_url_create(const struct imap_url *url);
const char *imap_url_add_urlauth(const char *rumpurl, const char *mechanism,
const unsigned char *token, size_t token_len);
#endif