#ifndef LIBETPAN_VERSION_H
#define LIBETPAN_VERSION_H
#ifndef LIBETPAN_VERSION_MAJOR
#define LIBETPAN_VERSION_MAJOR 1
#endif
#ifndef LIBETPAN_VERSION_MINOR
#define LIBETPAN_VERSION_MINOR 8
#endif
#ifndef LIBETPAN_REENTRANT
#if 1
#define LIBETPAN_REENTRANT 1
#endif
#ifndef LIBETPAN_API_CURRENT
#define LIBETPAN_API_CURRENT 21
#endif
#ifndef LIBETPAN_API_REVISION
#define LIBETPAN_API_REVISION 0
#endif
#ifndef LIBETPAN_API_COMPATIBILITY
#define LIBETPAN_API_COMPATIBILITY 20
#endif
#endif
int libetpan_get_version_major(void);
int libetpan_get_version_minor(void);
#endif