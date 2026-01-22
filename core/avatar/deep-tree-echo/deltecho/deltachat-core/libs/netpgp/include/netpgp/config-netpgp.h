#ifndef __CONFIG_NETPGP_H__
#define __CONFIG_NETPGP_H__
#ifdef HAVE_CONFIG_MESON
#include "config-netpgp-meson.h"
#else
#define HAVE_DLFCN_H 1
#define HAVE_ERRNO_H 1
#define HAVE_FCNTL_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_LIMITS_H 1
#define HAVE_LONG_LONG_INT 1
#define HAVE_MALLOC_H 1
#define HAVE_MEMORY_H 1
#define HAVE_OPENSSL_AES_H 1
#define HAVE_OPENSSL_BN_H 1
#define HAVE_OPENSSL_CAMELLIA_H 1
#define HAVE_OPENSSL_CAST_H 1
#define HAVE_OPENSSL_DES_H 1
#define HAVE_OPENSSL_DSA_H 1
#define HAVE_OPENSSL_ERR_H 1
#ifndef OPENSSL_NO_IDEA
#define OPENSSL_NO_IDEA
#endif
#define HAVE_OPENSSL_MD5_H 1
#define HAVE_OPENSSL_RAND_H 1
#define HAVE_OPENSSL_RSA_H 1
#define HAVE_OPENSSL_SHA_H 1
#define HAVE_SHA256_CTX 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_CDEFS_H 1
#define HAVE_SYS_FILE_H 1
#define HAVE_SYS_MMAN_H 1
#define HAVE_SYS_PARAM_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_UIO_H 1
#define HAVE_UNISTD_H 1
#define HAVE_UNSIGNED_LONG_LONG_INT 1
#define HAVE_ZLIB_H 1
#define LT_OBJDIR ".libs/"
#define PACKAGE "netpgp"
#define PACKAGE_BUGREPORT "Alistair Crooks <agc@netbsd.org> c0596823"
#define PACKAGE_NAME "netpgp"
#define PACKAGE_STRING "netpgp 20140220"
#define PACKAGE_TARNAME "netpgp"
#define PACKAGE_URL ""
#define PACKAGE_VERSION "20140220"
#define STDC_HEADERS 1
#define VERSION "20140220"
#endif
#endif