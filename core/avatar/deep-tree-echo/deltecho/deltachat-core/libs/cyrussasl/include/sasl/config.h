#ifndef CONFIG_H
#define CONFIG_H
#define CONFIGDIR "/usr/lib/sasl2:/etc/sasl2"
#define DO_SASL_CHECKAPOP
#define GSS_USE_MUTEXES
#define HAVE_AUTHDAEMON
#define HAVE_DIRENT_H 1
#define HAVE_DLFCN_H 1
#define HAVE_DN_EXPAND 1
#define HAVE_FCNTL_H 1
#define HAVE_GETADDRINFO
#define HAVE_GETDOMAINNAME 1
#define HAVE_GETHOSTNAME 1
#define HAVE_GETNAMEINFO
#define HAVE_GETPWNAM 1
#define HAVE_GETSUBOPT
#define HAVE_GETTIMEOFDAY 1
#define HAVE_GSSAPI_H
#define HAVE_GSS_C_NT_HOSTBASED_SERVICE
#define HAVE_GSS_C_NT_USER_NAME
#define HAVE_INET_ATON 1
#define HAVE_INTTYPES_H 1
#define HAVE_JRAND48 1
#define HAVE_LIBRESOLV 1
#define HAVE_LIMITS_H 1
#define HAVE_MEMCPY 1
#define HAVE_MEMORY_H 1
#define HAVE_MKDIR 1
#define HAVE_OPENSSL
#define HAVE_PATHS_H 1
#define HAVE_SASLAUTHD
#define HAVE_SECURITY_PAM_APPL_H 1
#define HAVE_SELECT 1
#define HAVE_SNPRINTF
#define HAVE_SOCKET 1
#define HAVE_SOCKLEN_T
#define HAVE_SS_FAMILY
#define HAVE_STDARG_H 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRCHR 1
#define HAVE_STRDUP 1
#define HAVE_STRERROR 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_STRSPN 1
#define HAVE_STRSTR 1
#define HAVE_STRTOL 1
#define HAVE_STRUCT_SOCKADDR_STORAGE
#define HAVE_SYSEXITS_H 1
#define HAVE_SYSLOG 1
#define HAVE_SYSLOG_H 1
#define HAVE_SYS_FILE_H 1
#define HAVE_SYS_PARAM_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_UIO_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_UNISTD_H 1
#define HAVE_VSNPRINTF
#define HAVE___ATTRIBUTE__ 1
#define PACKAGE "cyrus-sasl"
#define PACKAGE_BUGREPORT ""
#define PACKAGE_NAME ""
#define PACKAGE_STRING ""
#define PACKAGE_TARNAME ""
#define PACKAGE_VERSION ""
#define PATH_AUTHDAEMON_SOCKET "/dev/null"
#define PATH_SASLAUTHD_RUNDIR "/var/state/saslauthd"
#define PLUGINDIR "/usr/lib/sasl2"
#define RETSIGTYPE void
#define SASL_DB_PATH "/etc/sasldb2"
#define SASL_DEV_RANDOM "/dev/random"
#define SASL_NDBM
#define SIZEOF_LONG 8
#define STATIC_ANONYMOUS
#define STATIC_CRAMMD5
#define STATIC_DIGESTMD5
#define STATIC_LOGIN
#define STATIC_OTP
#define STATIC_PLAIN
#define STATIC_SCRAM
#define STDC_HEADERS 1
#define TIME_WITH_SYS_TIME 1
#define VERSION "2.1.25"
#define WITH_DES
#define WITH_RC4
#define WITH_SSL_DES
#ifndef __cplusplus
#endif
#if !defined(_WIN32) && !defined(HAVE_SYS_UIO_H)
struct iovec {
char *iov_base;
long iov_len;
};
#else
#include <sys/types.h>
#include <sys/uio.h>
#endif
#ifdef DEV_RANDOM
#endif
#define DEV_RANDOM SASL_DEV_RANDOM
#ifdef HAVE_KRB_GET_ERR_TEXT
#define get_krb_err_txt krb_get_err_text
#else
#define get_krb_err_txt(X) (krb_err_txt[(X)])
#endif
#ifndef __EXTENSIONS__
#define __EXTENSIONS__
#endif
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#ifndef HAVE___ATTRIBUTE__
#define __attribute__(foo)
#endif
#define SASL_PATH_ENV_VAR "SASL_PATH"
#define SASL_CONF_PATH_ENV_VAR "SASL_CONF_PATH"
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#ifndef WIN32
# include <netdb.h>
# ifdef HAVE_SYS_PARAM_H
# include <sys/param.h>
# endif
#else
# include <winsock2.h>
#endif
#include <string.h>
#include <netinet/in.h>
#ifndef HAVE_SOCKLEN_T
typedef unsigned int socklen_t;
#endif
#ifndef HAVE_STRUCT_SOCKADDR_STORAGE
#define _SS_MAXSIZE 128
#define _SS_PADSIZE (_SS_MAXSIZE - sizeof (struct sockaddr))
struct sockaddr_storage {
struct sockaddr ss_sa;
char __ss_pad2[_SS_PADSIZE];
};
# define ss_family ss_sa.sa_family
#endif
#ifndef AF_INET6
#define AF_INET6 AF_MAX
#endif
#ifndef HAVE_GETADDRINFO
#define getaddrinfo sasl_getaddrinfo
#define freeaddrinfo sasl_freeaddrinfo
#define gai_strerror sasl_gai_strerror
#endif
#ifndef HAVE_GETNAMEINFO
#define getnameinfo sasl_getnameinfo
#endif
#if !defined(HAVE_GETNAMEINFO) || !defined(HAVE_GETADDRINFO)
#include "gai.h"
#endif
#ifndef AI_NUMERICHOST
#define AI_NUMERICHOST 4
#define NI_NUMERICHOST 2
#define NI_NAMEREQD 4
#define NI_NUMERICSERV 8
#endif
#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 255
#endif
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
# include <sys/time.h>
# else
# include <time.h>
# endif
#endif
#ifndef HIER_DELIMITER
#define HIER_DELIMITER '/'
#endif
#endif