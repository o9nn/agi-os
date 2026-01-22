# ifndef LIBMAD_VERSION_H
# define LIBMAD_VERSION_H
# define MAD_VERSION_MAJOR 0
# define MAD_VERSION_MINOR 15
# define MAD_VERSION_PATCH 1
# define MAD_VERSION_EXTRA " (beta)"
# define MAD_VERSION_STRINGIZE(str) #str
# define MAD_VERSION_STRING(num) MAD_VERSION_STRINGIZE(num)
# define MAD_VERSION MAD_VERSION_STRING(MAD_VERSION_MAJOR) "." \
MAD_VERSION_STRING(MAD_VERSION_MINOR) "." \
MAD_VERSION_STRING(MAD_VERSION_PATCH) \
MAD_VERSION_EXTRA
# define MAD_PUBLISHYEAR "2000-2004"
# define MAD_AUTHOR "Underbit Technologies, Inc."
# define MAD_EMAIL "info@underbit.com"
extern char const mad_version[];
extern char const mad_copyright[];
extern char const mad_author[];
extern char const mad_build[];
# endif