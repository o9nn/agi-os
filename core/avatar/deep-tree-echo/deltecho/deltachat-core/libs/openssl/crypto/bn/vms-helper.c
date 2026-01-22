#include <stdio.h>
#include "cryptlib.h"
#include "bn_lcl.h"
bn_div_words_abort(int i)
{
#ifdef BN_DEBUG
# if !defined(OPENSSL_NO_STDIO) && !defined(OPENSSL_SYS_WIN16)
fprintf(stderr, "Division would overflow (%d)\n", i);
# endif
abort();
#endif
}