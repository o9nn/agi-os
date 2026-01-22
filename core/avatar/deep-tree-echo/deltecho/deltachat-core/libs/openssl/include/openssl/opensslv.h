#ifndef HEADER_OPENSSLV_H
# define HEADER_OPENSSLV_H
#ifdef __cplusplus
extern "C" {
#endif
# define OPENSSL_VERSION_NUMBER 0x1000114fL
# ifdef OPENSSL_FIPS
# define OPENSSL_VERSION_TEXT "OpenSSL 1.0.1t-fips  3 May 2016"
# else
# define OPENSSL_VERSION_TEXT "OpenSSL 1.0.1t  3 May 2016"
# endif
# define OPENSSL_VERSION_PTEXT " part of " OPENSSL_VERSION_TEXT
# define SHLIB_VERSION_HISTORY ""
# define SHLIB_VERSION_NUMBER "1.0.0"
#ifdef __cplusplus
}
#endif
#endif