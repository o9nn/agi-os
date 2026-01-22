#include <stdio.h>
#include <openssl/x509v3.h>
#include "ext_dat.h"
main()
{
int i, prev = -1, bad = 0;
X509V3_EXT_METHOD **tmp;
i = sizeof(standard_exts) / sizeof(X509V3_EXT_METHOD *);
if (i != STANDARD_EXTENSION_COUNT)
fprintf(stderr, "Extension number invalid expecting %d\n", i);
tmp = standard_exts;
for (i = 0; i < STANDARD_EXTENSION_COUNT; i++, tmp++) {
if ((*tmp)->ext_nid < prev)
bad = 1;
prev = (*tmp)->ext_nid;
}
if (bad) {
tmp = standard_exts;
fprintf(stderr, "Extensions out of order!\n");
for (i = 0; i < STANDARD_EXTENSION_COUNT; i++, tmp++)
printf("%d : %s\n", (*tmp)->ext_nid, OBJ_nid2sn((*tmp)->ext_nid));
} else
fprintf(stderr, "Order OK\n");
}