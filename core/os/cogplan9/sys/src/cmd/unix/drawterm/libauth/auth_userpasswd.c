#include <u.h>
#include <libc.h>
#include <auth.h>
#include <authsrv.h>
#include "authlocal.h"
static void
netresp(char *key, long chal, char *answer)
{
uchar buf[8];
memset(buf, 0, 8);
sprint((char *)buf, "%lud", chal);
if(encrypt(key, buf, 8) < 0)
abort();
chal = (buf[0]<<24)+(buf[1]<<16)+(buf[2]<<8)+buf[3];
sprint(answer, "%.8lux", chal);
}
AuthInfo*
auth_userpasswd(char *user, char *passwd)
{
char key[DESKEYLEN], resp[16];
AuthInfo *ai;
Chalstate *ch;
if((ch = auth_challenge("user=%q proto=p9cr role=server", user)) == nil)
return nil;
passtokey(key, passwd);
netresp(key, atol(ch->chal), resp);
memset(key, 0, sizeof key);
ch->resp = resp;
ch->nresp = strlen(resp);
ai = auth_response(ch);
auth_freechal(ch);
return ai;
}