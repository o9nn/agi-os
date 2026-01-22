#include <stdio.h>
#include <time.h>
#include "cryptlib.h"
#include "o_time.h"
#include <openssl/asn1t.h>
IMPLEMENT_ASN1_MSTRING(ASN1_TIME, B_ASN1_TIME)
IMPLEMENT_ASN1_FUNCTIONS(ASN1_TIME)
#if 0
int i2d_ASN1_TIME(ASN1_TIME *a, unsigned char **pp)
{
# ifdef CHARSET_EBCDIC
char tmp[24];
ASN1_STRING tmpstr;
if (a->type == V_ASN1_UTCTIME || a->type == V_ASN1_GENERALIZEDTIME) {
int len;
tmpstr = *(ASN1_STRING *)a;
len = tmpstr.length;
ebcdic2ascii(tmp, tmpstr.data,
(len >= sizeof tmp) ? sizeof tmp : len);
tmpstr.data = tmp;
a = (ASN1_GENERALIZEDTIME *)&tmpstr;
}
# endif
if (a->type == V_ASN1_UTCTIME || a->type == V_ASN1_GENERALIZEDTIME)
return (i2d_ASN1_bytes((ASN1_STRING *)a, pp,
a->type, V_ASN1_UNIVERSAL));
ASN1err(ASN1_F_I2D_ASN1_TIME, ASN1_R_EXPECTING_A_TIME);
return -1;
}
#endif
ASN1_TIME *ASN1_TIME_set(ASN1_TIME *s, time_t t)
{
return ASN1_TIME_adj(s, t, 0, 0);
}
ASN1_TIME *ASN1_TIME_adj(ASN1_TIME *s, time_t t,
int offset_day, long offset_sec)
{
struct tm *ts;
struct tm data;
ts = OPENSSL_gmtime(&t, &data);
if (ts == NULL) {
ASN1err(ASN1_F_ASN1_TIME_ADJ, ASN1_R_ERROR_GETTING_TIME);
return NULL;
}
if (offset_day || offset_sec) {
if (!OPENSSL_gmtime_adj(ts, offset_day, offset_sec))
return NULL;
}
if ((ts->tm_year >= 50) && (ts->tm_year < 150))
return ASN1_UTCTIME_adj(s, t, offset_day, offset_sec);
return ASN1_GENERALIZEDTIME_adj(s, t, offset_day, offset_sec);
}
int ASN1_TIME_check(ASN1_TIME *t)
{
if (t->type == V_ASN1_GENERALIZEDTIME)
return ASN1_GENERALIZEDTIME_check(t);
else if (t->type == V_ASN1_UTCTIME)
return ASN1_UTCTIME_check(t);
return 0;
}
ASN1_GENERALIZEDTIME *ASN1_TIME_to_generalizedtime(ASN1_TIME *t,
ASN1_GENERALIZEDTIME **out)
{
ASN1_GENERALIZEDTIME *ret;
char *str;
int newlen;
if (!ASN1_TIME_check(t))
return NULL;
if (!out || !*out) {
if (!(ret = ASN1_GENERALIZEDTIME_new()))
return NULL;
if (out)
*out = ret;
} else
ret = *out;
if (t->type == V_ASN1_GENERALIZEDTIME) {
if (!ASN1_STRING_set(ret, t->data, t->length))
return NULL;
return ret;
}
if (!ASN1_STRING_set(ret, NULL, t->length + 2))
return NULL;
newlen = t->length + 2 + 1;
str = (char *)ret->data;
if (t->data[0] >= '5')
BUF_strlcpy(str, "19", newlen);
else
BUF_strlcpy(str, "20", newlen);
BUF_strlcat(str, (char *)t->data, newlen);
return ret;
}
int ASN1_TIME_set_string(ASN1_TIME *s, const char *str)
{
ASN1_TIME t;
t.length = strlen(str);
t.data = (unsigned char *)str;
t.flags = 0;
t.type = V_ASN1_UTCTIME;
if (!ASN1_TIME_check(&t)) {
t.type = V_ASN1_GENERALIZEDTIME;
if (!ASN1_TIME_check(&t))
return 0;
}
if (s && !ASN1_STRING_copy((ASN1_STRING *)s, (ASN1_STRING *)&t))
return 0;
return 1;
}