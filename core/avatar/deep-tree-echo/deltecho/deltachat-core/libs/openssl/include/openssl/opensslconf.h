#ifdef __cplusplus
extern "C" {
#endif
#ifndef OPENSSL_DOING_MAKEDEPEND
#ifndef OPENSSL_NO_EC_NISTP_64_GCC_128
# define OPENSSL_NO_EC_NISTP_64_GCC_128
#endif
#ifndef OPENSSL_NO_GMP
# define OPENSSL_NO_GMP
#endif
#ifndef OPENSSL_NO_JPAKE
# define OPENSSL_NO_JPAKE
#endif
#ifndef OPENSSL_NO_KRB5
# define OPENSSL_NO_KRB5
#endif
#ifndef OPENSSL_NO_MD2
# define OPENSSL_NO_MD2
#endif
#ifndef OPENSSL_NO_RC5
# define OPENSSL_NO_RC5
#endif
#ifndef OPENSSL_NO_RFC3779
# define OPENSSL_NO_RFC3779
#endif
#ifndef OPENSSL_NO_SCTP
# define OPENSSL_NO_SCTP
#endif
#ifndef OPENSSL_NO_SSL2
# define OPENSSL_NO_SSL2
#endif
#ifndef OPENSSL_NO_STORE
# define OPENSSL_NO_STORE
#endif
#ifndef OPENSSL_NO_UNIT_TEST
# define OPENSSL_NO_UNIT_TEST
#endif
#ifndef OPENSSL_NO_WEAK_SSL_CIPHERS
# define OPENSSL_NO_WEAK_SSL_CIPHERS
#endif
#endif
#ifndef OPENSSL_NO_DYNAMIC_ENGINE
# define OPENSSL_NO_DYNAMIC_ENGINE
#endif
#ifdef OPENSSL_ALGORITHM_DEFINES
# if defined(OPENSSL_NO_EC_NISTP_64_GCC_128) && !defined(NO_EC_NISTP_64_GCC_128)
# define NO_EC_NISTP_64_GCC_128
# endif
# if defined(OPENSSL_NO_GMP) && !defined(NO_GMP)
# define NO_GMP
# endif
# if defined(OPENSSL_NO_JPAKE) && !defined(NO_JPAKE)
# define NO_JPAKE
# endif
# if defined(OPENSSL_NO_KRB5) && !defined(NO_KRB5)
# define NO_KRB5
# endif
# if defined(OPENSSL_NO_MD2) && !defined(NO_MD2)
# define NO_MD2
# endif
# if defined(OPENSSL_NO_RC5) && !defined(NO_RC5)
# define NO_RC5
# endif
# if defined(OPENSSL_NO_RFC3779) && !defined(NO_RFC3779)
# define NO_RFC3779
# endif
# if defined(OPENSSL_NO_SCTP) && !defined(NO_SCTP)
# define NO_SCTP
# endif
# if defined(OPENSSL_NO_SSL2) && !defined(NO_SSL2)
# define NO_SSL2
# endif
# if defined(OPENSSL_NO_STORE) && !defined(NO_STORE)
# define NO_STORE
# endif
# if defined(OPENSSL_NO_UNIT_TEST) && !defined(NO_UNIT_TEST)
# define NO_UNIT_TEST
# endif
# if defined(OPENSSL_NO_WEAK_SSL_CIPHERS) && !defined(NO_WEAK_SSL_CIPHERS)
# define NO_WEAK_SSL_CIPHERS
# endif
#endif
#undef I386_ONLY
#if !(defined(VMS) || defined(__VMS))
#if defined(HEADER_CRYPTLIB_H) && !defined(OPENSSLDIR)
#define ENGINESDIR "/usr/local/ssl/lib/engines"
#define OPENSSLDIR "/usr/local/ssl"
#endif
#endif
#undef OPENSSL_UNISTD
#define OPENSSL_UNISTD <unistd.h>
#undef OPENSSL_EXPORT_VAR_AS_FUNCTION
#if defined(HEADER_IDEA_H) && !defined(IDEA_INT)
#define IDEA_INT unsigned int
#endif
#if defined(HEADER_MD2_H) && !defined(MD2_INT)
#define MD2_INT unsigned int
#endif
#if defined(HEADER_RC2_H) && !defined(RC2_INT)
#define RC2_INT unsigned int
#endif
#if defined(HEADER_RC4_H)
#if !defined(RC4_INT)
#define RC4_INT unsigned int
#endif
#if !defined(RC4_CHUNK)
#undef RC4_CHUNK
#endif
#endif
#if (defined(HEADER_NEW_DES_H) || defined(HEADER_DES_H)) && !defined(DES_LONG)
#ifndef DES_LONG
#define DES_LONG unsigned long
#endif
#endif
#if defined(HEADER_BN_H) && !defined(CONFIG_HEADER_BN_H)
#define CONFIG_HEADER_BN_H
#undef BN_LLONG
#undef SIXTY_FOUR_BIT_LONG
#undef SIXTY_FOUR_BIT
#define THIRTY_TWO_BIT
#endif
#if defined(HEADER_RC4_LOCL_H) && !defined(CONFIG_HEADER_RC4_LOCL_H)
#define CONFIG_HEADER_RC4_LOCL_H
#undef RC4_INDEX
#endif
#if defined(HEADER_BF_LOCL_H) && !defined(CONFIG_HEADER_BF_LOCL_H)
#define CONFIG_HEADER_BF_LOCL_H
#undef BF_PTR
#endif
#if defined(HEADER_DES_LOCL_H) && !defined(CONFIG_HEADER_DES_LOCL_H)
#define CONFIG_HEADER_DES_LOCL_H
#ifndef DES_DEFAULT_OPTIONS
#ifndef DES_PTR
#undef DES_PTR
#endif
#ifndef DES_RISC1
#undef DES_RISC1
#endif
#ifndef DES_RISC2
#undef DES_RISC2
#endif
#if defined(DES_RISC1) && defined(DES_RISC2)
#error YOU SHOULD NOT HAVE BOTH DES_RISC1 AND DES_RISC2 DEFINED!!!!!
#endif
#ifndef DES_UNROLL
#undef DES_UNROLL
#endif
#if !defined(DES_PTR) && !defined(DES_RISC1) && !defined(DES_RISC2) && !defined(DES_UNROLL)
#if defined( __sun ) || defined ( sun )
# define DES_PTR
# define DES_RISC1
# define DES_UNROLL
#elif defined( __ultrix )
# define DES_PTR
# define DES_RISC2
# define DES_UNROLL
#elif defined( __osf1__ )
# define DES_PTR
# define DES_RISC2
#elif defined ( _AIX )
#elif defined( __hpux )
#elif defined( __aux )
#elif defined( __dgux )
# define DES_UNROLL
#elif defined( __sgi )
# define DES_PTR
# define DES_RISC2
# define DES_UNROLL
#elif defined(i386) || defined(__i386__)
# define DES_PTR
# define DES_RISC1
# define DES_UNROLL
#endif
#endif
#endif
#endif
#ifdef __cplusplus
}
#endif