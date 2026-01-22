#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include "e_os2.h"
#include "o_dir.h"
#if defined OPENSSL_SYS_UNIX || defined OPENSSL_SYS_WIN32 || defined OPENSSL_SYS_WINCE
# define CURRDIR "."
#elif defined OPENSSL_SYS_VMS
# define CURRDIR "SYS$DISK:[]"
#else
# error "No supported platform defined!"
#endif
int main()
{
OPENSSL_DIR_CTX *ctx = NULL;
const char *result;
while ((result = OPENSSL_DIR_read(&ctx, CURRDIR)) != NULL) {
printf("%s\n", result);
}
if (errno) {
perror("test_dir");
exit(1);
}
if (!OPENSSL_DIR_end(&ctx)) {
perror("test_dir");
exit(2);
}
exit(0);
}