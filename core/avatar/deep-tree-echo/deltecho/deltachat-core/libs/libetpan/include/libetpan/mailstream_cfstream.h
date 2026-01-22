#ifndef MAILSTREAM_CFSTREAM_H
#define MAILSTREAM_CFSTREAM_H
#include <libetpan/libetpan-config.h>
#include <libetpan/mailstream.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
extern int mailstream_cfstream_enabled;
LIBETPAN_EXPORT
extern int mailstream_cfstream_voip_enabled;
enum {
MAILSTREAM_CFSTREAM_SSL_ALLOWS_EXPIRED_CERTIFICATES = 1 << 0,
MAILSTREAM_CFSTREAM_SSL_ALLOWS_EXPIRED_ROOTS = 1 << 1,
MAILSTREAM_CFSTREAM_SSL_ALLOWS_ANY_ROOT = 1 << 2,
MAILSTREAM_CFSTREAM_SSL_DISABLE_VALIDATES_CERTIFICATE_CHAIN = 1 << 3,
MAILSTREAM_CFSTREAM_SSL_NO_VERIFICATION = MAILSTREAM_CFSTREAM_SSL_ALLOWS_EXPIRED_CERTIFICATES |
MAILSTREAM_CFSTREAM_SSL_ALLOWS_EXPIRED_ROOTS |
MAILSTREAM_CFSTREAM_SSL_ALLOWS_ANY_ROOT |
MAILSTREAM_CFSTREAM_SSL_DISABLE_VALIDATES_CERTIFICATE_CHAIN
};
enum {
MAILSTREAM_CFSTREAM_SSL_LEVEL_NONE,
MAILSTREAM_CFSTREAM_SSL_LEVEL_SSLv2,
MAILSTREAM_CFSTREAM_SSL_LEVEL_SSLv3,
MAILSTREAM_CFSTREAM_SSL_LEVEL_TLSv1,
MAILSTREAM_CFSTREAM_SSL_LEVEL_NEGOCIATED_SSL
};
extern mailstream_low_driver * mailstream_cfstream_driver;
mailstream * mailstream_cfstream_open(const char * hostname, int16_t port);
mailstream * mailstream_cfstream_open_timeout(const char * hostname, int16_t port, time_t timeout);
mailstream * mailstream_cfstream_open_voip(const char * hostname, int16_t port, int voip_enabled);
mailstream * mailstream_cfstream_open_voip_timeout(const char * hostname, int16_t port, int voip_enabled,
time_t timeout);
mailstream_low * mailstream_low_cfstream_open(const char * hostname, int16_t port);
mailstream_low * mailstream_low_cfstream_open_timeout(const char * hostname, int16_t port,
time_t timeout);
mailstream_low * mailstream_low_cfstream_open_voip(const char * hostname, int16_t port, int voip_enabled);
mailstream_low * mailstream_low_cfstream_open_voip_timeout(const char * hostname, int16_t port,
int voip_enabled, time_t timeout);
void mailstream_cfstream_set_ssl_verification_mask(mailstream * s, int verification_mask);
void mailstream_cfstream_set_ssl_peer_name(mailstream * s, const char * peer_name);
void mailstream_cfstream_set_ssl_is_server(mailstream * s, int is_server);
void mailstream_cfstream_set_ssl_level(mailstream * s, int ssl_level);
int mailstream_cfstream_set_ssl_enabled(mailstream * s, int ssl_enabled);
int mailstream_cfstream_is_ssl_enabled(mailstream * s);
int mailstream_cfstream_wait_idle(mailstream * s, int max_idle_delay);
int mailstream_low_cfstream_wait_idle(mailstream_low * low, int max_idle_delay);
#ifdef __cplusplus
}
#endif
#endif