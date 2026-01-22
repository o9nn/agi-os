#include "lib.h"
#include "safe-memset.h"
#include "str.h"
#include "md5.h"
#include "password-scheme.h"
static unsigned char itoa64[] =
"./0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
static char	magic[] = "$1$";
static void
to64(string_t *str, unsigned long v, int n)
{
while (--n >= 0) {
str_append_c(str, itoa64[v&0x3f]);
v >>= 6;
}
}
const char *password_generate_md5_crypt(const char *pw, const char *salt)
{
const char *sp,*ep;
unsigned char	final[MD5_RESULTLEN];
int sl,pl,i,j;
struct md5_context ctx,ctx1;
unsigned long l;
string_t *passwd;
size_t pw_len = strlen(pw);
sp = salt;
if (strncmp(sp, magic, sizeof(magic)-1) == 0)
sp += sizeof(magic)-1;
for(ep=sp;*ep != '\0' && *ep != '$' && ep < (sp+8);ep++)
continue;
sl = ep - sp;
md5_init(&ctx);
md5_update(&ctx,pw,pw_len);
md5_update(&ctx,magic,sizeof(magic)-1);
md5_update(&ctx,sp,sl);
md5_init(&ctx1);
md5_update(&ctx1,pw,pw_len);
md5_update(&ctx1,sp,sl);
md5_update(&ctx1,pw,pw_len);
md5_final(&ctx1,final);
for(pl = pw_len; pl > 0; pl -= MD5_RESULTLEN)
md5_update(&ctx,final,pl>MD5_RESULTLEN ? MD5_RESULTLEN : pl);
safe_memset(final, 0, sizeof(final));
for (j=0,i = pw_len; i != 0; i >>= 1)
if ((i&1) != 0)
md5_update(&ctx, final+j, 1);
else
md5_update(&ctx, pw+j, 1);
passwd = t_str_new(sl + 64);
str_append(passwd, magic);
str_append_data(passwd, sp, sl);
str_append_c(passwd, '$');
md5_final(&ctx,final);
for(i=0;i<1000;i++) {
md5_init(&ctx1);
if((i & 1) != 0)
md5_update(&ctx1,pw,pw_len);
else
md5_update(&ctx1,final,MD5_RESULTLEN);
if((i % 3) != 0)
md5_update(&ctx1,sp,sl);
if((i % 7) != 0)
md5_update(&ctx1,pw,pw_len);
if((i & 1) != 0)
md5_update(&ctx1,final,MD5_RESULTLEN);
else
md5_update(&ctx1,pw,pw_len);
md5_final(&ctx1,final);
}
l = (final[ 0]<<16) | (final[ 6]<<8) | final[12]; to64(passwd,l,4);
l = (final[ 1]<<16) | (final[ 7]<<8) | final[13]; to64(passwd,l,4);
l = (final[ 2]<<16) | (final[ 8]<<8) | final[14]; to64(passwd,l,4);
l = (final[ 3]<<16) | (final[ 9]<<8) | final[15]; to64(passwd,l,4);
l = (final[ 4]<<16) | (final[10]<<8) | final[ 5]; to64(passwd,l,4);
l =                    final[11]                ; to64(passwd,l,2);
safe_memset(final, 0, sizeof(final));
return str_c(passwd);
}