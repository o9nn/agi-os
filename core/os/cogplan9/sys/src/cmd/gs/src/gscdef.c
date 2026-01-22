#include "std.h"
#include "gscdefs.h"
#include "gconfigd.h"
#ifndef GS_BUILDTIME
#  define GS_BUILDTIME\
0
#endif
CONFIG_CONST long gs_buildtime = GS_BUILDTIME;
#ifndef GS_COPYRIGHT
#  define GS_COPYRIGHT\
"Copyright (C) 2005 artofcode LLC, Benicia, CA.  All rights reserved."
#endif
const char *CONFIG_CONST gs_copyright = GS_COPYRIGHT;
#ifndef GS_PRODUCTFAMILY
#  define GS_PRODUCTFAMILY\
"AFPL Ghostscript"
#endif
const char *CONFIG_CONST gs_productfamily = GS_PRODUCTFAMILY;
#ifndef GS_PRODUCT
#  define GS_PRODUCT\
GS_PRODUCTFAMILY
#endif
const char *CONFIG_CONST gs_product = GS_PRODUCT;
const char *
gs_program_name(void)
{
return gs_product;
}
CONFIG_CONST long gs_revision = GS_REVISION;
long
gs_revision_number(void)
{
return gs_revision;
}
CONFIG_CONST long gs_revisiondate = GS_REVISIONDATE;
#ifndef GS_SERIALNUMBER
#  define GS_SERIALNUMBER\
42
#endif
CONFIG_CONST long gs_serialnumber = GS_SERIALNUMBER;
const char *const gs_doc_directory = GS_DOCDIR;
const char *const gs_lib_default_path = GS_LIB_DEFAULT;
const char *const gs_init_file = GS_INIT;