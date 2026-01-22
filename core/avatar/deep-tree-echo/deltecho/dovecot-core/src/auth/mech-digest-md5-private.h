#ifndef MECH_DIGEST_MD5_PRIVATE_H
#define MECH_DIGEST_MD5_PRIVATE_H
#include "auth-request.h"
enum qop_option {
QOP_AUTH	= 0x01,
QOP_AUTH_INT	= 0x02,
QOP_AUTH_CONF	= 0x04,
QOP_COUNT	= 3
};
struct digest_auth_request {
struct auth_request auth_request;
pool_t pool;
char *nonce;
enum qop_option qop;
char *username;
char *cnonce;
char *nonce_count;
char *qop_value;
char *digest_uri;
char *authzid;
unsigned char response[32];
unsigned long maxbuf;
bool nonce_found:1;
char *rspauth;
};
#endif