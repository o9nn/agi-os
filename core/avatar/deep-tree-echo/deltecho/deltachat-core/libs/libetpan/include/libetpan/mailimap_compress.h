#ifndef MAILIMAP_COMPRESS_H
#define MAILIMAP_COMPRESS_H
#include <libetpan/mailimap_types.h>
LIBETPAN_EXPORT
int mailimap_compress(mailimap * session);
LIBETPAN_EXPORT
int mailimap_has_compress_deflate(mailimap * session);
#endif