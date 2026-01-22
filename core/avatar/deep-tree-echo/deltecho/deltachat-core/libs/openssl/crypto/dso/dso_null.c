#include <stdio.h>
#include "cryptlib.h"
#include <openssl/dso.h>
static DSO_METHOD dso_meth_null = {
"NULL shared library method",
NULL,
NULL,
NULL,
NULL,
#if 0
NULL,
NULL,
#endif
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL
};
DSO_METHOD *DSO_METHOD_null(void)
{
return (&dso_meth_null);
}