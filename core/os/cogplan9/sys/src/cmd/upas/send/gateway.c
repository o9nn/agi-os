#include "common.h"
#include "send.h"
#define isspace(c) ((c)==' ' || (c)=='\t' || (c)=='\n')
extern void
gateway(message *mp)
{
char *base;
String *s;
base = skipequiv(s_to_c(mp->sender));
if(base != s_to_c(mp->sender)){
s = mp->sender;
mp->sender = s_copy(base);
s_free(s);
}
}