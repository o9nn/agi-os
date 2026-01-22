#if WIN32 || WIN64
# define HAVE_BUILTIN_EXPECT 0
#else
# define HAVE_BUILTIN_EXPECT 1
#endif
#define HAVE_DLFCN_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_MEMORY_H 1
#define HAVE_STDINT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRINGS_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_UNISTD_H 1
#define LT_OBJDIR ".libs/"
#define PACKAGE_BUGREPORT "cityhash-discuss@googlegroups.com"
#define PACKAGE_NAME "CityHash"
#define PACKAGE_STRING "CityHash 1.1.1"
#define PACKAGE_TARNAME "cityhash"
#define PACKAGE_URL ""
#define PACKAGE_VERSION "1.1.1"
#define STDC_HEADERS 1
#if defined AC_APPLE_UNIVERSAL_BUILD
# if defined __BIG_ENDIAN__
# define WORDS_BIGENDIAN 1
# endif
#else
# ifndef WORDS_BIGENDIAN
# endif
#endif
#ifndef __cplusplus
#endif