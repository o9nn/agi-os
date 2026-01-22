#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "libetpan_version.h"
int libetpan_get_version_major(void)
{
return LIBETPAN_VERSION_MAJOR;
}
int libetpan_get_version_minor(void)
{
return LIBETPAN_VERSION_MINOR;
}