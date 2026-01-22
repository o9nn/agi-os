#include "stdpre.h"
#include "gstypes.h"
#include "gscrypt1.h"
int
gs_type1_encrypt(byte * dest, const byte * src, uint len, crypt_state * pstate)
{
crypt_state state = *pstate;
const byte *from = src;
byte *to = dest;
uint count = len;
while (count) {
encrypt_next(*from, state, *to);
from++, to++, count--;
}
*pstate = state;
return 0;
}
int
gs_type1_decrypt(byte * dest, const byte * src, uint len, crypt_state * pstate)
{
crypt_state state = *pstate;
const byte *from = src;
byte *to = dest;
uint count = len;
while (count) {
byte ch = *from++;
decrypt_next(ch, state, *to);
to++, count--;
}
*pstate = state;
return 0;
}