#include <stdio.h>
#define DES_FCRYPT
#include "des_locl.h"
#undef DES_FCRYPT
#undef PERM_OP
#define PERM_OP(a,b,t,n,m) ((t)=((((a)>>(n))^(b))&(m)),\
(b)^=(t),\
(a)^=((t)<<(n)))
#undef HPERM_OP
#define HPERM_OP(a,t,n,m) ((t)=((((a)<<(16-(n)))^(a))&(m)),\
(a)=(a)^(t)^(t>>(16-(n))))\
void fcrypt_body(DES_LONG *out, DES_key_schedule *ks, DES_LONG Eswap0,
DES_LONG Eswap1)
{
register DES_LONG l, r, t, u;
#ifdef DES_PTR
register const unsigned char *des_SP = (const unsigned char *)DES_SPtrans;
#endif
register DES_LONG *s;
register int j;
register DES_LONG E0, E1;
l = 0;
r = 0;
s = (DES_LONG *)ks;
E0 = Eswap0;
E1 = Eswap1;
for (j = 0; j < 25; j++) {
#ifndef DES_UNROLL
register int i;
for (i = 0; i < 32; i += 4) {
D_ENCRYPT(l, r, i + 0);
D_ENCRYPT(r, l, i + 2);
}
#else
D_ENCRYPT(l, r, 0);
D_ENCRYPT(r, l, 2);
D_ENCRYPT(l, r, 4);
D_ENCRYPT(r, l, 6);
D_ENCRYPT(l, r, 8);
D_ENCRYPT(r, l, 10);
D_ENCRYPT(l, r, 12);
D_ENCRYPT(r, l, 14);
D_ENCRYPT(l, r, 16);
D_ENCRYPT(r, l, 18);
D_ENCRYPT(l, r, 20);
D_ENCRYPT(r, l, 22);
D_ENCRYPT(l, r, 24);
D_ENCRYPT(r, l, 26);
D_ENCRYPT(l, r, 28);
D_ENCRYPT(r, l, 30);
#endif
t = l;
l = r;
r = t;
}
l = ROTATE(l, 3) & 0xffffffffL;
r = ROTATE(r, 3) & 0xffffffffL;
PERM_OP(l, r, t, 1, 0x55555555L);
PERM_OP(r, l, t, 8, 0x00ff00ffL);
PERM_OP(l, r, t, 2, 0x33333333L);
PERM_OP(r, l, t, 16, 0x0000ffffL);
PERM_OP(l, r, t, 4, 0x0f0f0f0fL);
out[0] = r;
out[1] = l;
}