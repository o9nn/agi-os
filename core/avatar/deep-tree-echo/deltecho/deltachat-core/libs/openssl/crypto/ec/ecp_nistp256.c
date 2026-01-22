#include <openssl/opensslconf.h>
#ifndef OPENSSL_NO_EC_NISTP_64_GCC_128
# ifndef OPENSSL_SYS_VMS
#  include <stdint.h>
# else
#  include <inttypes.h>
# endif
# include <string.h>
# include <openssl/err.h>
# include "ec_lcl.h"
# if defined(__GNUC__) && (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))
typedef __uint128_t uint128_t;
typedef __int128_t int128_t;
# else
#  error "Need GCC 3.1 or later to define type uint128_t"
# endif
typedef uint8_t u8;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int64_t s64;
typedef u8 felem_bytearray[32];
static const felem_bytearray nistp256_curve_params[5] = {
{0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x01,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff},
{0xff, 0xff, 0xff, 0xff, 0x00, 0x00, 0x00, 0x01,
0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff,
0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xfc},
{0x5a, 0xc6, 0x35, 0xd8, 0xaa, 0x3a, 0x93, 0xe7,
0xb3, 0xeb, 0xbd, 0x55, 0x76, 0x98, 0x86, 0xbc,
0x65, 0x1d, 0x06, 0xb0, 0xcc, 0x53, 0xb0, 0xf6,
0x3b, 0xce, 0x3c, 0x3e, 0x27, 0xd2, 0x60, 0x4b},
{0x6b, 0x17, 0xd1, 0xf2, 0xe1, 0x2c, 0x42, 0x47,
0xf8, 0xbc, 0xe6, 0xe5, 0x63, 0xa4, 0x40, 0xf2,
0x77, 0x03, 0x7d, 0x81, 0x2d, 0xeb, 0x33, 0xa0,
0xf4, 0xa1, 0x39, 0x45, 0xd8, 0x98, 0xc2, 0x96},
{0x4f, 0xe3, 0x42, 0xe2, 0xfe, 0x1a, 0x7f, 0x9b,
0x8e, 0xe7, 0xeb, 0x4a, 0x7c, 0x0f, 0x9e, 0x16,
0x2b, 0xce, 0x33, 0x57, 0x6b, 0x31, 0x5e, 0xce,
0xcb, 0xb6, 0x40, 0x68, 0x37, 0xbf, 0x51, 0xf5}
};
# define NLIMBS 4
typedef uint128_t limb;
typedef limb felem[NLIMBS];
typedef limb longfelem[NLIMBS * 2];
typedef u64 smallfelem[NLIMBS];
static const u64 kPrime[4] =
{ 0xfffffffffffffffful, 0xffffffff, 0, 0xffffffff00000001ul };
static const u64 bottom63bits = 0x7ffffffffffffffful;
static void bin32_to_felem(felem out, const u8 in[32])
{
out[0] = *((u64 *)&in[0]);
out[1] = *((u64 *)&in[8]);
out[2] = *((u64 *)&in[16]);
out[3] = *((u64 *)&in[24]);
}
static void smallfelem_to_bin32(u8 out[32], const smallfelem in)
{
*((u64 *)&out[0]) = in[0];
*((u64 *)&out[8]) = in[1];
*((u64 *)&out[16]) = in[2];
*((u64 *)&out[24]) = in[3];
}
static void flip_endian(u8 *out, const u8 *in, unsigned len)
{
unsigned i;
for (i = 0; i < len; ++i)
out[i] = in[len - 1 - i];
}
static int BN_to_felem(felem out, const BIGNUM *bn)
{
felem_bytearray b_in;
felem_bytearray b_out;
unsigned num_bytes;
memset(b_out, 0, sizeof b_out);
num_bytes = BN_num_bytes(bn);
if (num_bytes > sizeof b_out) {
ECerr(EC_F_BN_TO_FELEM, EC_R_BIGNUM_OUT_OF_RANGE);
return 0;
}
if (BN_is_negative(bn)) {
ECerr(EC_F_BN_TO_FELEM, EC_R_BIGNUM_OUT_OF_RANGE);
return 0;
}
num_bytes = BN_bn2bin(bn, b_in);
flip_endian(b_out, b_in, num_bytes);
bin32_to_felem(out, b_out);
return 1;
}
static BIGNUM *smallfelem_to_BN(BIGNUM *out, const smallfelem in)
{
felem_bytearray b_in, b_out;
smallfelem_to_bin32(b_in, in);
flip_endian(b_out, b_in, sizeof b_out);
return BN_bin2bn(b_out, sizeof b_out, out);
}
static void smallfelem_one(smallfelem out)
{
out[0] = 1;
out[1] = 0;
out[2] = 0;
out[3] = 0;
}
static void smallfelem_assign(smallfelem out, const smallfelem in)
{
out[0] = in[0];
out[1] = in[1];
out[2] = in[2];
out[3] = in[3];
}
static void felem_assign(felem out, const felem in)
{
out[0] = in[0];
out[1] = in[1];
out[2] = in[2];
out[3] = in[3];
}
static void felem_sum(felem out, const felem in)
{
out[0] += in[0];
out[1] += in[1];
out[2] += in[2];
out[3] += in[3];
}
static void felem_small_sum(felem out, const smallfelem in)
{
out[0] += in[0];
out[1] += in[1];
out[2] += in[2];
out[3] += in[3];
}
static void felem_scalar(felem out, const u64 scalar)
{
out[0] *= scalar;
out[1] *= scalar;
out[2] *= scalar;
out[3] *= scalar;
}
static void longfelem_scalar(longfelem out, const u64 scalar)
{
out[0] *= scalar;
out[1] *= scalar;
out[2] *= scalar;
out[3] *= scalar;
out[4] *= scalar;
out[5] *= scalar;
out[6] *= scalar;
out[7] *= scalar;
}
# define two105m41m9 (((limb)1) << 105) - (((limb)1) << 41) - (((limb)1) << 9)
# define two105 (((limb)1) << 105)
# define two105m41p9 (((limb)1) << 105) - (((limb)1) << 41) + (((limb)1) << 9)
static const felem zero105 =
{ two105m41m9, two105, two105m41p9, two105m41p9 };
static void smallfelem_neg(felem out, const smallfelem small)
{
out[0] = zero105[0] - small[0];
out[1] = zero105[1] - small[1];
out[2] = zero105[2] - small[2];
out[3] = zero105[3] - small[3];
}
static void felem_diff(felem out, const felem in)
{
out[0] += zero105[0];
out[1] += zero105[1];
out[2] += zero105[2];
out[3] += zero105[3];
out[0] -= in[0];
out[1] -= in[1];
out[2] -= in[2];
out[3] -= in[3];
}
# define two107m43m11 (((limb)1) << 107) - (((limb)1) << 43) - (((limb)1) << 11)
# define two107 (((limb)1) << 107)
# define two107m43p11 (((limb)1) << 107) - (((limb)1) << 43) + (((limb)1) << 11)
static const felem zero107 =
{ two107m43m11, two107, two107m43p11, two107m43p11 };
static void felem_diff_zero107(felem out, const felem in)
{
out[0] += zero107[0];
out[1] += zero107[1];
out[2] += zero107[2];
out[3] += zero107[3];
out[0] -= in[0];
out[1] -= in[1];
out[2] -= in[2];
out[3] -= in[3];
}
static void longfelem_diff(longfelem out, const longfelem in)
{
static const limb two70m8p6 =
(((limb) 1) << 70) - (((limb) 1) << 8) + (((limb) 1) << 6);
static const limb two70p40 = (((limb) 1) << 70) + (((limb) 1) << 40);
static const limb two70 = (((limb) 1) << 70);
static const limb two70m40m38p6 =
(((limb) 1) << 70) - (((limb) 1) << 40) - (((limb) 1) << 38) +
(((limb) 1) << 6);
static const limb two70m6 = (((limb) 1) << 70) - (((limb) 1) << 6);
out[0] += two70m8p6;
out[1] += two70p40;
out[2] += two70;
out[3] += two70m40m38p6;
out[4] += two70m6;
out[5] += two70m6;
out[6] += two70m6;
out[7] += two70m6;
out[0] -= in[0];
out[1] -= in[1];
out[2] -= in[2];
out[3] -= in[3];
out[4] -= in[4];
out[5] -= in[5];
out[6] -= in[6];
out[7] -= in[7];
}
# define two64m0 (((limb)1) << 64) - 1
# define two110p32m0 (((limb)1) << 110) + (((limb)1) << 32) - 1
# define two64m46 (((limb)1) << 64) - (((limb)1) << 46)
# define two64m32 (((limb)1) << 64) - (((limb)1) << 32)
static const felem zero110 = { two64m0, two110p32m0, two64m46, two64m32 };
static void felem_shrink(smallfelem out, const felem in)
{
felem tmp;
u64 a, b, mask;
s64 high, low;
static const u64 kPrime3Test = 0x7fffffff00000001ul;
tmp[3] = zero110[3] + in[3] + ((u64)(in[2] >> 64));
tmp[2] = zero110[2] + (u64)in[2];
tmp[0] = zero110[0] + in[0];
tmp[1] = zero110[1] + in[1];
a = tmp[3] >> 64;
tmp[3] = (u64)tmp[3];
tmp[3] -= a;
tmp[3] += ((limb) a) << 32;
b = a;
a = tmp[3] >> 64;
b += a;
tmp[3] = (u64)tmp[3];
tmp[3] -= a;
tmp[3] += ((limb) a) << 32;
tmp[0] += b;
tmp[1] -= (((limb) b) << 32);
high = tmp[3] >> 64;
high <<= 63;
high >>= 63;
low = tmp[3];
mask = low >> 63;
low &= bottom63bits;
low -= kPrime3Test;
low = ~low;
low >>= 63;
mask = (mask & low) | high;
tmp[0] -= mask & kPrime[0];
tmp[1] -= mask & kPrime[1];
tmp[3] -= mask & kPrime[3];
tmp[1] += ((u64)(tmp[0] >> 64));
tmp[0] = (u64)tmp[0];
tmp[2] += ((u64)(tmp[1] >> 64));
tmp[1] = (u64)tmp[1];
tmp[3] += ((u64)(tmp[2] >> 64));
tmp[2] = (u64)tmp[2];
out[0] = tmp[0];
out[1] = tmp[1];
out[2] = tmp[2];
out[3] = tmp[3];
}
static void smallfelem_expand(felem out, const smallfelem in)
{
out[0] = in[0];
out[1] = in[1];
out[2] = in[2];
out[3] = in[3];
}
static void smallfelem_square(longfelem out, const smallfelem small)
{
limb a;
u64 high, low;
a = ((uint128_t) small[0]) * small[0];
low = a;
high = a >> 64;
out[0] = low;
out[1] = high;
a = ((uint128_t) small[0]) * small[1];
low = a;
high = a >> 64;
out[1] += low;
out[1] += low;
out[2] = high;
a = ((uint128_t) small[0]) * small[2];
low = a;
high = a >> 64;
out[2] += low;
out[2] *= 2;
out[3] = high;
a = ((uint128_t) small[0]) * small[3];
low = a;
high = a >> 64;
out[3] += low;
out[4] = high;
a = ((uint128_t) small[1]) * small[2];
low = a;
high = a >> 64;
out[3] += low;
out[3] *= 2;
out[4] += high;
a = ((uint128_t) small[1]) * small[1];
low = a;
high = a >> 64;
out[2] += low;
out[3] += high;
a = ((uint128_t) small[1]) * small[3];
low = a;
high = a >> 64;
out[4] += low;
out[4] *= 2;
out[5] = high;
a = ((uint128_t) small[2]) * small[3];
low = a;
high = a >> 64;
out[5] += low;
out[5] *= 2;
out[6] = high;
out[6] += high;
a = ((uint128_t) small[2]) * small[2];
low = a;
high = a >> 64;
out[4] += low;
out[5] += high;
a = ((uint128_t) small[3]) * small[3];
low = a;
high = a >> 64;
out[6] += low;
out[7] = high;
}
static void felem_square(longfelem out, const felem in)
{
u64 small[4];
felem_shrink(small, in);
smallfelem_square(out, small);
}
static void smallfelem_mul(longfelem out, const smallfelem small1,
const smallfelem small2)
{
limb a;
u64 high, low;
a = ((uint128_t) small1[0]) * small2[0];
low = a;
high = a >> 64;
out[0] = low;
out[1] = high;
a = ((uint128_t) small1[0]) * small2[1];
low = a;
high = a >> 64;
out[1] += low;
out[2] = high;
a = ((uint128_t) small1[1]) * small2[0];
low = a;
high = a >> 64;
out[1] += low;
out[2] += high;
a = ((uint128_t) small1[0]) * small2[2];
low = a;
high = a >> 64;
out[2] += low;
out[3] = high;
a = ((uint128_t) small1[1]) * small2[1];
low = a;
high = a >> 64;
out[2] += low;
out[3] += high;
a = ((uint128_t) small1[2]) * small2[0];
low = a;
high = a >> 64;
out[2] += low;
out[3] += high;
a = ((uint128_t) small1[0]) * small2[3];
low = a;
high = a >> 64;
out[3] += low;
out[4] = high;
a = ((uint128_t) small1[1]) * small2[2];
low = a;
high = a >> 64;
out[3] += low;
out[4] += high;
a = ((uint128_t) small1[2]) * small2[1];
low = a;
high = a >> 64;
out[3] += low;
out[4] += high;
a = ((uint128_t) small1[3]) * small2[0];
low = a;
high = a >> 64;
out[3] += low;
out[4] += high;
a = ((uint128_t) small1[1]) * small2[3];
low = a;
high = a >> 64;
out[4] += low;
out[5] = high;
a = ((uint128_t) small1[2]) * small2[2];
low = a;
high = a >> 64;
out[4] += low;
out[5] += high;
a = ((uint128_t) small1[3]) * small2[1];
low = a;
high = a >> 64;
out[4] += low;
out[5] += high;
a = ((uint128_t) small1[2]) * small2[3];
low = a;
high = a >> 64;
out[5] += low;
out[6] = high;
a = ((uint128_t) small1[3]) * small2[2];
low = a;
high = a >> 64;
out[5] += low;
out[6] += high;
a = ((uint128_t) small1[3]) * small2[3];
low = a;
high = a >> 64;
out[6] += low;
out[7] = high;
}
static void felem_mul(longfelem out, const felem in1, const felem in2)
{
smallfelem small1, small2;
felem_shrink(small1, in1);
felem_shrink(small2, in2);
smallfelem_mul(out, small1, small2);
}
static void felem_small_mul(longfelem out, const smallfelem small1,
const felem in2)
{
smallfelem small2;
felem_shrink(small2, in2);
smallfelem_mul(out, small1, small2);
}
# define two100m36m4 (((limb)1) << 100) - (((limb)1) << 36) - (((limb)1) << 4)
# define two100 (((limb)1) << 100)
# define two100m36p4 (((limb)1) << 100) - (((limb)1) << 36) + (((limb)1) << 4)
static const felem zero100 =
{ two100m36m4, two100, two100m36p4, two100m36p4 };
static void felem_reduce_(felem out, const longfelem in)
{
int128_t c;
c = in[4] + (in[5] << 32);
out[0] += c;
out[3] -= c;
c = in[5] - in[7];
out[1] += c;
out[2] -= c;
out[1] -= (in[4] << 32);
out[3] += (in[4] << 32);
out[2] -= (in[5] << 32);
out[0] -= in[6];
out[0] -= (in[6] << 32);
out[1] += (in[6] << 33);
out[2] += (in[6] * 2);
out[3] -= (in[6] << 32);
out[0] -= in[7];
out[0] -= (in[7] << 32);
out[2] += (in[7] << 33);
out[3] += (in[7] * 3);
}
static void felem_reduce(felem out, const longfelem in)
{
out[0] = zero100[0] + in[0];
out[1] = zero100[1] + in[1];
out[2] = zero100[2] + in[2];
out[3] = zero100[3] + in[3];
felem_reduce_(out, in);
}
static void felem_reduce_zero105(felem out, const longfelem in)
{
out[0] = zero105[0] + in[0];
out[1] = zero105[1] + in[1];
out[2] = zero105[2] + in[2];
out[3] = zero105[3] + in[3];
felem_reduce_(out, in);
}
static void subtract_u64(u64 *result, u64 *carry, u64 v)
{
uint128_t r = *result;
r -= v;
*carry = (r >> 64) & 1;
*result = (u64)r;
}
static void felem_contract(smallfelem out, const felem in)
{
unsigned i;
u64 all_equal_so_far = 0, result = 0, carry;
felem_shrink(out, in);
all_equal_so_far--;
for (i = 3; i < 4; i--) {
u64 equal;
uint128_t a = ((uint128_t) kPrime[i]) - out[i];
result |= all_equal_so_far & ((u64)(a >> 64));
equal = kPrime[i] ^ out[i];
equal--;
equal &= equal << 32;
equal &= equal << 16;
equal &= equal << 8;
equal &= equal << 4;
equal &= equal << 2;
equal &= equal << 1;
equal = ((s64) equal) >> 63;
all_equal_so_far &= equal;
}
result |= all_equal_so_far;
subtract_u64(&out[0], &carry, result & kPrime[0]);
subtract_u64(&out[1], &carry, carry);
subtract_u64(&out[2], &carry, carry);
subtract_u64(&out[3], &carry, carry);
subtract_u64(&out[1], &carry, result & kPrime[1]);
subtract_u64(&out[2], &carry, carry);
subtract_u64(&out[3], &carry, carry);
subtract_u64(&out[2], &carry, result & kPrime[2]);
subtract_u64(&out[3], &carry, carry);
subtract_u64(&out[3], &carry, result & kPrime[3]);
}
static void smallfelem_square_contract(smallfelem out, const smallfelem in)
{
longfelem longtmp;
felem tmp;
smallfelem_square(longtmp, in);
felem_reduce(tmp, longtmp);
felem_contract(out, tmp);
}
static void smallfelem_mul_contract(smallfelem out, const smallfelem in1,
const smallfelem in2)
{
longfelem longtmp;
felem tmp;
smallfelem_mul(longtmp, in1, in2);
felem_reduce(tmp, longtmp);
felem_contract(out, tmp);
}
static limb smallfelem_is_zero(const smallfelem small)
{
limb result;
u64 is_p;
u64 is_zero = small[0] | small[1] | small[2] | small[3];
is_zero--;
is_zero &= is_zero << 32;
is_zero &= is_zero << 16;
is_zero &= is_zero << 8;
is_zero &= is_zero << 4;
is_zero &= is_zero << 2;
is_zero &= is_zero << 1;
is_zero = ((s64) is_zero) >> 63;
is_p = (small[0] ^ kPrime[0]) |
(small[1] ^ kPrime[1]) |
(small[2] ^ kPrime[2]) | (small[3] ^ kPrime[3]);
is_p--;
is_p &= is_p << 32;
is_p &= is_p << 16;
is_p &= is_p << 8;
is_p &= is_p << 4;
is_p &= is_p << 2;
is_p &= is_p << 1;
is_p = ((s64) is_p) >> 63;
is_zero |= is_p;
result = is_zero;
result |= ((limb) is_zero) << 64;
return result;
}
static int smallfelem_is_zero_int(const smallfelem small)
{
return (int)(smallfelem_is_zero(small) & ((limb) 1));
}
static void felem_inv(felem out, const felem in)
{
felem ftmp, ftmp2;
felem e2, e4, e8, e16, e32, e64;
longfelem tmp;
unsigned i;
felem_square(tmp, in);
felem_reduce(ftmp, tmp);
felem_mul(tmp, in, ftmp);
felem_reduce(ftmp, tmp);
felem_assign(e2, ftmp);
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
felem_mul(tmp, ftmp, e2);
felem_reduce(ftmp, tmp);
felem_assign(e4, ftmp);
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
felem_mul(tmp, ftmp, e4);
felem_reduce(ftmp, tmp);
felem_assign(e8, ftmp);
for (i = 0; i < 8; i++) {
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
}
felem_mul(tmp, ftmp, e8);
felem_reduce(ftmp, tmp);
felem_assign(e16, ftmp);
for (i = 0; i < 16; i++) {
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
}
felem_mul(tmp, ftmp, e16);
felem_reduce(ftmp, tmp);
felem_assign(e32, ftmp);
for (i = 0; i < 32; i++) {
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
}
felem_assign(e64, ftmp);
felem_mul(tmp, ftmp, in);
felem_reduce(ftmp, tmp);
for (i = 0; i < 192; i++) {
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
}
felem_mul(tmp, e64, e32);
felem_reduce(ftmp2, tmp);
for (i = 0; i < 16; i++) {
felem_square(tmp, ftmp2);
felem_reduce(ftmp2, tmp);
}
felem_mul(tmp, ftmp2, e16);
felem_reduce(ftmp2, tmp);
for (i = 0; i < 8; i++) {
felem_square(tmp, ftmp2);
felem_reduce(ftmp2, tmp);
}
felem_mul(tmp, ftmp2, e8);
felem_reduce(ftmp2, tmp);
for (i = 0; i < 4; i++) {
felem_square(tmp, ftmp2);
felem_reduce(ftmp2, tmp);
}
felem_mul(tmp, ftmp2, e4);
felem_reduce(ftmp2, tmp);
felem_square(tmp, ftmp2);
felem_reduce(ftmp2, tmp);
felem_square(tmp, ftmp2);
felem_reduce(ftmp2, tmp);
felem_mul(tmp, ftmp2, e2);
felem_reduce(ftmp2, tmp);
felem_square(tmp, ftmp2);
felem_reduce(ftmp2, tmp);
felem_square(tmp, ftmp2);
felem_reduce(ftmp2, tmp);
felem_mul(tmp, ftmp2, in);
felem_reduce(ftmp2, tmp);
felem_mul(tmp, ftmp2, ftmp);
felem_reduce(out, tmp);
}
static void smallfelem_inv_contract(smallfelem out, const smallfelem in)
{
felem tmp;
smallfelem_expand(tmp, in);
felem_inv(tmp, tmp);
felem_contract(out, tmp);
}
static void
point_double(felem x_out, felem y_out, felem z_out,
const felem x_in, const felem y_in, const felem z_in)
{
longfelem tmp, tmp2;
felem delta, gamma, beta, alpha, ftmp, ftmp2;
smallfelem small1, small2;
felem_assign(ftmp, x_in);
felem_assign(ftmp2, x_in);
felem_square(tmp, z_in);
felem_reduce(delta, tmp);
felem_square(tmp, y_in);
felem_reduce(gamma, tmp);
felem_shrink(small1, gamma);
felem_small_mul(tmp, small1, x_in);
felem_reduce(beta, tmp);
felem_diff(ftmp, delta);
felem_sum(ftmp2, delta);
felem_scalar(ftmp2, 3);
felem_mul(tmp, ftmp, ftmp2);
felem_reduce(alpha, tmp);
felem_shrink(small2, alpha);
smallfelem_square(tmp, small2);
felem_reduce(x_out, tmp);
felem_assign(ftmp, beta);
felem_scalar(ftmp, 8);
felem_diff(x_out, ftmp);
felem_sum(delta, gamma);
felem_assign(ftmp, y_in);
felem_sum(ftmp, z_in);
felem_square(tmp, ftmp);
felem_reduce(z_out, tmp);
felem_diff(z_out, delta);
felem_scalar(beta, 4);
felem_diff_zero107(beta, x_out);
felem_small_mul(tmp, small2, beta);
smallfelem_square(tmp2, small1);
longfelem_scalar(tmp2, 8);
longfelem_diff(tmp, tmp2);
felem_reduce_zero105(y_out, tmp);
}
static void
point_double_small(smallfelem x_out, smallfelem y_out, smallfelem z_out,
const smallfelem x_in, const smallfelem y_in,
const smallfelem z_in)
{
felem felem_x_out, felem_y_out, felem_z_out;
felem felem_x_in, felem_y_in, felem_z_in;
smallfelem_expand(felem_x_in, x_in);
smallfelem_expand(felem_y_in, y_in);
smallfelem_expand(felem_z_in, z_in);
point_double(felem_x_out, felem_y_out, felem_z_out,
felem_x_in, felem_y_in, felem_z_in);
felem_shrink(x_out, felem_x_out);
felem_shrink(y_out, felem_y_out);
felem_shrink(z_out, felem_z_out);
}
static void copy_conditional(felem out, const felem in, limb mask)
{
unsigned i;
for (i = 0; i < NLIMBS; ++i) {
const limb tmp = mask & (in[i] ^ out[i]);
out[i] ^= tmp;
}
}
static void copy_small_conditional(felem out, const smallfelem in, limb mask)
{
unsigned i;
const u64 mask64 = mask;
for (i = 0; i < NLIMBS; ++i) {
out[i] = ((limb) (in[i] & mask64)) | (out[i] & ~mask);
}
}
static void point_add(felem x3, felem y3, felem z3,
const felem x1, const felem y1, const felem z1,
const int mixed, const smallfelem x2,
const smallfelem y2, const smallfelem z2)
{
felem ftmp, ftmp2, ftmp3, ftmp4, ftmp5, ftmp6, x_out, y_out, z_out;
longfelem tmp, tmp2;
smallfelem small1, small2, small3, small4, small5;
limb x_equal, y_equal, z1_is_zero, z2_is_zero;
felem_shrink(small3, z1);
z1_is_zero = smallfelem_is_zero(small3);
z2_is_zero = smallfelem_is_zero(z2);
smallfelem_square(tmp, small3);
felem_reduce(ftmp, tmp);
felem_shrink(small1, ftmp);
if (!mixed) {
smallfelem_square(tmp, z2);
felem_reduce(ftmp2, tmp);
felem_shrink(small2, ftmp2);
felem_shrink(small5, x1);
smallfelem_mul(tmp, small5, small2);
felem_reduce(ftmp3, tmp);
felem_assign(ftmp5, z1);
felem_small_sum(ftmp5, z2);
felem_square(tmp, ftmp5);
felem_reduce(ftmp5, tmp);
felem_sum(ftmp2, ftmp);
felem_diff(ftmp5, ftmp2);
smallfelem_mul(tmp, small2, z2);
felem_reduce(ftmp2, tmp);
felem_mul(tmp, y1, ftmp2);
felem_reduce(ftmp6, tmp);
} else {
felem_assign(ftmp3, x1);
felem_assign(ftmp5, z1);
felem_scalar(ftmp5, 2);
felem_assign(ftmp6, y1);
}
smallfelem_mul(tmp, x2, small1);
felem_reduce(ftmp4, tmp);
felem_diff_zero107(ftmp4, ftmp3);
felem_shrink(small4, ftmp4);
x_equal = smallfelem_is_zero(small4);
felem_small_mul(tmp, small4, ftmp5);
felem_reduce(z_out, tmp);
smallfelem_mul(tmp, small1, small3);
felem_reduce(ftmp, tmp);
felem_small_mul(tmp, y2, ftmp);
felem_reduce(ftmp5, tmp);
felem_diff_zero107(ftmp5, ftmp6);
felem_scalar(ftmp5, 2);
felem_shrink(small1, ftmp5);
y_equal = smallfelem_is_zero(small1);
if (x_equal && y_equal && !z1_is_zero && !z2_is_zero) {
point_double(x3, y3, z3, x1, y1, z1);
return;
}
felem_assign(ftmp, ftmp4);
felem_scalar(ftmp, 2);
felem_square(tmp, ftmp);
felem_reduce(ftmp, tmp);
felem_mul(tmp, ftmp4, ftmp);
felem_reduce(ftmp2, tmp);
felem_mul(tmp, ftmp3, ftmp);
felem_reduce(ftmp4, tmp);
smallfelem_square(tmp, small1);
felem_reduce(x_out, tmp);
felem_assign(ftmp3, ftmp4);
felem_scalar(ftmp4, 2);
felem_sum(ftmp4, ftmp2);
felem_diff(x_out, ftmp4);
felem_diff_zero107(ftmp3, x_out);
felem_small_mul(tmp, small1, ftmp3);
felem_mul(tmp2, ftmp6, ftmp2);
longfelem_scalar(tmp2, 2);
longfelem_diff(tmp, tmp2);
felem_reduce_zero105(y_out, tmp);
copy_small_conditional(x_out, x2, z1_is_zero);
copy_conditional(x_out, x1, z2_is_zero);
copy_small_conditional(y_out, y2, z1_is_zero);
copy_conditional(y_out, y1, z2_is_zero);
copy_small_conditional(z_out, z2, z1_is_zero);
copy_conditional(z_out, z1, z2_is_zero);
felem_assign(x3, x_out);
felem_assign(y3, y_out);
felem_assign(z3, z_out);
}
static void point_add_small(smallfelem x3, smallfelem y3, smallfelem z3,
smallfelem x1, smallfelem y1, smallfelem z1,
smallfelem x2, smallfelem y2, smallfelem z2)
{
felem felem_x3, felem_y3, felem_z3;
felem felem_x1, felem_y1, felem_z1;
smallfelem_expand(felem_x1, x1);
smallfelem_expand(felem_y1, y1);
smallfelem_expand(felem_z1, z1);
point_add(felem_x3, felem_y3, felem_z3, felem_x1, felem_y1, felem_z1, 0,
x2, y2, z2);
felem_shrink(x3, felem_x3);
felem_shrink(y3, felem_y3);
felem_shrink(z3, felem_z3);
}
static const smallfelem gmul[2][16][3] = {
{{{0, 0, 0, 0},
{0, 0, 0, 0},
{0, 0, 0, 0}},
{{0xf4a13945d898c296, 0x77037d812deb33a0, 0xf8bce6e563a440f2,
0x6b17d1f2e12c4247},
{0xcbb6406837bf51f5, 0x2bce33576b315ece, 0x8ee7eb4a7c0f9e16,
0x4fe342e2fe1a7f9b},
{1, 0, 0, 0}},
{{0x90e75cb48e14db63, 0x29493baaad651f7e, 0x8492592e326e25de,
0x0fa822bc2811aaa5},
{0xe41124545f462ee7, 0x34b1a65050fe82f5, 0x6f4ad4bcb3df188b,
0xbff44ae8f5dba80d},
{1, 0, 0, 0}},
{{0x93391ce2097992af, 0xe96c98fd0d35f1fa, 0xb257c0de95e02789,
0x300a4bbc89d6726f},
{0xaa54a291c08127a0, 0x5bb1eeada9d806a5, 0x7f1ddb25ff1e3c6f,
0x72aac7e0d09b4644},
{1, 0, 0, 0}},
{{0x57c84fc9d789bd85, 0xfc35ff7dc297eac3, 0xfb982fd588c6766e,
0x447d739beedb5e67},
{0x0c7e33c972e25b32, 0x3d349b95a7fae500, 0xe12e9d953a4aaff7,
0x2d4825ab834131ee},
{1, 0, 0, 0}},
{{0x13949c932a1d367f, 0xef7fbd2b1a0a11b7, 0xddc6068bb91dfc60,
0xef9519328a9c72ff},
{0x196035a77376d8a8, 0x23183b0895ca1740, 0xc1ee9807022c219c,
0x611e9fc37dbb2c9b},
{1, 0, 0, 0}},
{{0xcae2b1920b57f4bc, 0x2936df5ec6c9bc36, 0x7dea6482e11238bf,
0x550663797b51f5d8},
{0x44ffe216348a964c, 0x9fb3d576dbdefbe1, 0x0afa40018d9d50e5,
0x157164848aecb851},
{1, 0, 0, 0}},
{{0xe48ecafffc5cde01, 0x7ccd84e70d715f26, 0xa2e8f483f43e4391,
0xeb5d7745b21141ea},
{0xcac917e2731a3479, 0x85f22cfe2844b645, 0x0990e6a158006cee,
0xeafd72ebdbecc17b},
{1, 0, 0, 0}},
{{0x6cf20ffb313728be, 0x96439591a3c6b94a, 0x2736ff8344315fc5,
0xa6d39677a7849276},
{0xf2bab833c357f5f4, 0x824a920c2284059b, 0x66b8babd2d27ecdf,
0x674f84749b0b8816},
{1, 0, 0, 0}},
{{0x2df48c04677c8a3e, 0x74e02f080203a56b, 0x31855f7db8c7fedb,
0x4e769e7672c9ddad},
{0xa4c36165b824bbb0, 0xfb9ae16f3b9122a5, 0x1ec0057206947281,
0x42b99082de830663},
{1, 0, 0, 0}},
{{0x6ef95150dda868b9, 0xd1f89e799c0ce131, 0x7fdc1ca008a1c478,
0x78878ef61c6ce04d},
{0x9c62b9121fe0d976, 0x6ace570ebde08d4f, 0xde53142c12309def,
0xb6cb3f5d7b72c321},
{1, 0, 0, 0}},
{{0x7f991ed2c31a3573, 0x5b82dd5bd54fb496, 0x595c5220812ffcae,
0x0c88bc4d716b1287},
{0x3a57bf635f48aca8, 0x7c8181f4df2564f3, 0x18d1b5b39c04e6aa,
0xdd5ddea3f3901dc6},
{1, 0, 0, 0}},
{{0xe96a79fb3e72ad0c, 0x43a0a28c42ba792f, 0xefe0a423083e49f3,
0x68f344af6b317466},
{0xcdfe17db3fb24d4a, 0x668bfc2271f5c626, 0x604ed93c24d67ff3,
0x31b9c405f8540a20},
{1, 0, 0, 0}},
{{0xd36b4789a2582e7f, 0x0d1a10144ec39c28, 0x663c62c3edbad7a0,
0x4052bf4b6f461db9},
{0x235a27c3188d25eb, 0xe724f33999bfcc5b, 0x862be6bd71d70cc8,
0xfecf4d5190b0fc61},
{1, 0, 0, 0}},
{{0x74346c10a1d4cfac, 0xafdf5cc08526a7a4, 0x123202a8f62bff7a,
0x1eddbae2c802e41a},
{0x8fa0af2dd603f844, 0x36e06b7e4c701917, 0x0c45f45273db33a0,
0x43104d86560ebcfc},
{1, 0, 0, 0}},
{{0x9615b5110d1d78e5, 0x66b0de3225c4744b, 0x0a4a46fb6aaf363a,
0xb48e26b484f7a21c},
{0x06ebb0f621a01b2d, 0xc004e4048b7b0f98, 0x64131bcdfed6f668,
0xfac015404d4d3dab},
{1, 0, 0, 0}}},
{{{0, 0, 0, 0},
{0, 0, 0, 0},
{0, 0, 0, 0}},
{{0x3a5a9e22185a5943, 0x1ab919365c65dfb6, 0x21656b32262c71da,
0x7fe36b40af22af89},
{0xd50d152c699ca101, 0x74b3d5867b8af212, 0x9f09f40407dca6f1,
0xe697d45825b63624},
{1, 0, 0, 0}},
{{0xa84aa9397512218e, 0xe9a521b074ca0141, 0x57880b3a18a2e902,
0x4a5b506612a677a6},
{0x0beada7a4c4f3840, 0x626db15419e26d9d, 0xc42604fbe1627d40,
0xeb13461ceac089f1},
{1, 0, 0, 0}},
{{0xf9faed0927a43281, 0x5e52c4144103ecbc, 0xc342967aa815c857,
0x0781b8291c6a220a},
{0x5a8343ceeac55f80, 0x88f80eeee54a05e3, 0x97b2a14f12916434,
0x690cde8df0151593},
{1, 0, 0, 0}},
{{0xaee9c75df7f82f2a, 0x9e4c35874afdf43a, 0xf5622df437371326,
0x8a535f566ec73617},
{0xc5f9a0ac223094b7, 0xcde533864c8c7669, 0x37e02819085a92bf,
0x0455c08468b08bd7},
{1, 0, 0, 0}},
{{0x0c0a6e2c9477b5d9, 0xf9a4bf62876dc444, 0x5050a949b6cdc279,
0x06bada7ab77f8276},
{0xc8b4aed1ea48dac9, 0xdebd8a4b7ea1070f, 0x427d49101366eb70,
0x5b476dfd0e6cb18a},
{1, 0, 0, 0}},
{{0x7c5c3e44278c340a, 0x4d54606812d66f3b, 0x29a751b1ae23c5d8,
0x3e29864e8a2ec908},
{0x142d2a6626dbb850, 0xad1744c4765bd780, 0x1f150e68e322d1ed,
0x239b90ea3dc31e7e},
{1, 0, 0, 0}},
{{0x78c416527a53322a, 0x305dde6709776f8e, 0xdbcab759f8862ed4,
0x820f4dd949f72ff7},
{0x6cc544a62b5debd4, 0x75be5d937b4e8cc4, 0x1b481b1b215c14d3,
0x140406ec783a05ec},
{1, 0, 0, 0}},
{{0x6a703f10e895df07, 0xfd75f3fa01876bd8, 0xeb5b06e70ce08ffe,
0x68f6b8542783dfee},
{0x90c76f8a78712655, 0xcf5293d2f310bf7f, 0xfbc8044dfda45028,
0xcbe1feba92e40ce6},
{1, 0, 0, 0}},
{{0xe998ceea4396e4c1, 0xfc82ef0b6acea274, 0x230f729f2250e927,
0xd0b2f94d2f420109},
{0x4305adddb38d4966, 0x10b838f8624c3b45, 0x7db2636658954e7a,
0x971459828b0719e5},
{1, 0, 0, 0}},
{{0x4bd6b72623369fc9, 0x57f2929e53d0b876, 0xc2d5cba4f2340687,
0x961610004a866aba},
{0x49997bcd2e407a5e, 0x69ab197d92ddcb24, 0x2cf1f2438fe5131c,
0x7acb9fadcee75e44},
{1, 0, 0, 0}},
{{0x254e839423d2d4c0, 0xf57f0c917aea685b, 0xa60d880f6f75aaea,
0x24eb9acca333bf5b},
{0xe3de4ccb1cda5dea, 0xfeef9341c51a6b4f, 0x743125f88bac4c4d,
0x69f891c5acd079cc},
{1, 0, 0, 0}},
{{0xeee44b35702476b5, 0x7ed031a0e45c2258, 0xb422d1e7bd6f8514,
0xe51f547c5972a107},
{0xa25bcd6fc9cf343d, 0x8ca922ee097c184e, 0xa62f98b3a9fe9a06,
0x1c309a2b25bb1387},
{1, 0, 0, 0}},
{{0x9295dbeb1967c459, 0xb00148833472c98e, 0xc504977708011828,
0x20b87b8aa2c4e503},
{0x3063175de057c277, 0x1bd539338fe582dd, 0x0d11adef5f69a044,
0xf5c6fa49919776be},
{1, 0, 0, 0}},
{{0x8c944e760fd59e11, 0x3876cba1102fad5f, 0xa454c3fad83faa56,
0x1ed7d1b9332010b9},
{0xa1011a270024b889, 0x05e4d0dcac0cd344, 0x52b520f0eb6a2a24,
0x3a2b03f03217257a},
{1, 0, 0, 0}},
{{0xf20fc2afdf1d043d, 0xf330240db58d5a62, 0xfc7d229ca0058c3b,
0x15fee545c78dd9f6},
{0x501e82885bc98cda, 0x41ef80e5d046ac04, 0x557d9f49461210fb,
0x4ab5b6b2b8753f81},
{1, 0, 0, 0}}}
};
static void select_point(const u64 idx, unsigned int size,
const smallfelem pre_comp[16][3], smallfelem out[3])
{
unsigned i, j;
u64 *outlimbs = &out[0][0];
memset(outlimbs, 0, 3 * sizeof(smallfelem));
for (i = 0; i < size; i++) {
const u64 *inlimbs = (u64 *)&pre_comp[i][0][0];
u64 mask = i ^ idx;
mask |= mask >> 4;
mask |= mask >> 2;
mask |= mask >> 1;
mask &= 1;
mask--;
for (j = 0; j < NLIMBS * 3; j++)
outlimbs[j] |= inlimbs[j] & mask;
}
}
static char get_bit(const felem_bytearray in, int i)
{
if ((i < 0) || (i >= 256))
return 0;
return (in[i >> 3] >> (i & 7)) & 1;
}
static void batch_mul(felem x_out, felem y_out, felem z_out,
const felem_bytearray scalars[],
const unsigned num_points, const u8 *g_scalar,
const int mixed, const smallfelem pre_comp[][17][3],
const smallfelem g_pre_comp[2][16][3])
{
int i, skip;
unsigned num, gen_mul = (g_scalar != NULL);
felem nq[3], ftmp;
smallfelem tmp[3];
u64 bits;
u8 sign, digit;
memset(nq, 0, 3 * sizeof(felem));
skip = 1;
for (i = (num_points ? 255 : 31); i >= 0; --i) {
if (!skip)
point_double(nq[0], nq[1], nq[2], nq[0], nq[1], nq[2]);
if (gen_mul && (i <= 31)) {
bits = get_bit(g_scalar, i + 224) << 3;
bits |= get_bit(g_scalar, i + 160) << 2;
bits |= get_bit(g_scalar, i + 96) << 1;
bits |= get_bit(g_scalar, i + 32);
select_point(bits, 16, g_pre_comp[1], tmp);
if (!skip) {
point_add(nq[0], nq[1], nq[2],
nq[0], nq[1], nq[2], 1, tmp[0], tmp[1], tmp[2]);
} else {
smallfelem_expand(nq[0], tmp[0]);
smallfelem_expand(nq[1], tmp[1]);
smallfelem_expand(nq[2], tmp[2]);
skip = 0;
}
bits = get_bit(g_scalar, i + 192) << 3;
bits |= get_bit(g_scalar, i + 128) << 2;
bits |= get_bit(g_scalar, i + 64) << 1;
bits |= get_bit(g_scalar, i);
select_point(bits, 16, g_pre_comp[0], tmp);
point_add(nq[0], nq[1], nq[2],
nq[0], nq[1], nq[2], 1, tmp[0], tmp[1], tmp[2]);
}
if (num_points && (i % 5 == 0)) {
for (num = 0; num < num_points; ++num) {
bits = get_bit(scalars[num], i + 4) << 5;
bits |= get_bit(scalars[num], i + 3) << 4;
bits |= get_bit(scalars[num], i + 2) << 3;
bits |= get_bit(scalars[num], i + 1) << 2;
bits |= get_bit(scalars[num], i) << 1;
bits |= get_bit(scalars[num], i - 1);
ec_GFp_nistp_recode_scalar_bits(&sign, &digit, bits);
select_point(digit, 17, pre_comp[num], tmp);
smallfelem_neg(ftmp, tmp[1]);
copy_small_conditional(ftmp, tmp[1], (((limb) sign) - 1));
felem_contract(tmp[1], ftmp);
if (!skip) {
point_add(nq[0], nq[1], nq[2],
nq[0], nq[1], nq[2],
mixed, tmp[0], tmp[1], tmp[2]);
} else {
smallfelem_expand(nq[0], tmp[0]);
smallfelem_expand(nq[1], tmp[1]);
smallfelem_expand(nq[2], tmp[2]);
skip = 0;
}
}
}
}
felem_assign(x_out, nq[0]);
felem_assign(y_out, nq[1]);
felem_assign(z_out, nq[2]);
}
typedef struct {
smallfelem g_pre_comp[2][16][3];
int references;
} NISTP256_PRE_COMP;
const EC_METHOD *EC_GFp_nistp256_method(void)
{
static const EC_METHOD ret = {
EC_FLAGS_DEFAULT_OCT,
NID_X9_62_prime_field,
ec_GFp_nistp256_group_init,
ec_GFp_simple_group_finish,
ec_GFp_simple_group_clear_finish,
ec_GFp_nist_group_copy,
ec_GFp_nistp256_group_set_curve,
ec_GFp_simple_group_get_curve,
ec_GFp_simple_group_get_degree,
ec_GFp_simple_group_check_discriminant,
ec_GFp_simple_point_init,
ec_GFp_simple_point_finish,
ec_GFp_simple_point_clear_finish,
ec_GFp_simple_point_copy,
ec_GFp_simple_point_set_to_infinity,
ec_GFp_simple_set_Jprojective_coordinates_GFp,
ec_GFp_simple_get_Jprojective_coordinates_GFp,
ec_GFp_simple_point_set_affine_coordinates,
ec_GFp_nistp256_point_get_affine_coordinates,
0  ,
0  ,
0  ,
ec_GFp_simple_add,
ec_GFp_simple_dbl,
ec_GFp_simple_invert,
ec_GFp_simple_is_at_infinity,
ec_GFp_simple_is_on_curve,
ec_GFp_simple_cmp,
ec_GFp_simple_make_affine,
ec_GFp_simple_points_make_affine,
ec_GFp_nistp256_points_mul,
ec_GFp_nistp256_precompute_mult,
ec_GFp_nistp256_have_precompute_mult,
ec_GFp_nist_field_mul,
ec_GFp_nist_field_sqr,
0  ,
0  ,
0  ,
0
};
return &ret;
}
static NISTP256_PRE_COMP *nistp256_pre_comp_new()
{
NISTP256_PRE_COMP *ret = NULL;
ret = (NISTP256_PRE_COMP *) OPENSSL_malloc(sizeof *ret);
if (!ret) {
ECerr(EC_F_NISTP256_PRE_COMP_NEW, ERR_R_MALLOC_FAILURE);
return ret;
}
memset(ret->g_pre_comp, 0, sizeof(ret->g_pre_comp));
ret->references = 1;
return ret;
}
static void *nistp256_pre_comp_dup(void *src_)
{
NISTP256_PRE_COMP *src = src_;
CRYPTO_add(&src->references, 1, CRYPTO_LOCK_EC_PRE_COMP);
return src_;
}
static void nistp256_pre_comp_free(void *pre_)
{
int i;
NISTP256_PRE_COMP *pre = pre_;
if (!pre)
return;
i = CRYPTO_add(&pre->references, -1, CRYPTO_LOCK_EC_PRE_COMP);
if (i > 0)
return;
OPENSSL_free(pre);
}
static void nistp256_pre_comp_clear_free(void *pre_)
{
int i;
NISTP256_PRE_COMP *pre = pre_;
if (!pre)
return;
i = CRYPTO_add(&pre->references, -1, CRYPTO_LOCK_EC_PRE_COMP);
if (i > 0)
return;
OPENSSL_cleanse(pre, sizeof *pre);
OPENSSL_free(pre);
}
int ec_GFp_nistp256_group_init(EC_GROUP *group)
{
int ret;
ret = ec_GFp_simple_group_init(group);
group->a_is_minus3 = 1;
return ret;
}
int ec_GFp_nistp256_group_set_curve(EC_GROUP *group, const BIGNUM *p,
const BIGNUM *a, const BIGNUM *b,
BN_CTX *ctx)
{
int ret = 0;
BN_CTX *new_ctx = NULL;
BIGNUM *curve_p, *curve_a, *curve_b;
if (ctx == NULL)
if ((ctx = new_ctx = BN_CTX_new()) == NULL)
return 0;
BN_CTX_start(ctx);
if (((curve_p = BN_CTX_get(ctx)) == NULL) ||
((curve_a = BN_CTX_get(ctx)) == NULL) ||
((curve_b = BN_CTX_get(ctx)) == NULL))
goto err;
BN_bin2bn(nistp256_curve_params[0], sizeof(felem_bytearray), curve_p);
BN_bin2bn(nistp256_curve_params[1], sizeof(felem_bytearray), curve_a);
BN_bin2bn(nistp256_curve_params[2], sizeof(felem_bytearray), curve_b);
if ((BN_cmp(curve_p, p)) || (BN_cmp(curve_a, a)) || (BN_cmp(curve_b, b))) {
ECerr(EC_F_EC_GFP_NISTP256_GROUP_SET_CURVE,
EC_R_WRONG_CURVE_PARAMETERS);
goto err;
}
group->field_mod_func = BN_nist_mod_256;
ret = ec_GFp_simple_group_set_curve(group, p, a, b, ctx);
err:
BN_CTX_end(ctx);
if (new_ctx != NULL)
BN_CTX_free(new_ctx);
return ret;
}
int ec_GFp_nistp256_point_get_affine_coordinates(const EC_GROUP *group,
const EC_POINT *point,
BIGNUM *x, BIGNUM *y,
BN_CTX *ctx)
{
felem z1, z2, x_in, y_in;
smallfelem x_out, y_out;
longfelem tmp;
if (EC_POINT_is_at_infinity(group, point)) {
ECerr(EC_F_EC_GFP_NISTP256_POINT_GET_AFFINE_COORDINATES,
EC_R_POINT_AT_INFINITY);
return 0;
}
if ((!BN_to_felem(x_in, &point->X)) || (!BN_to_felem(y_in, &point->Y)) ||
(!BN_to_felem(z1, &point->Z)))
return 0;
felem_inv(z2, z1);
felem_square(tmp, z2);
felem_reduce(z1, tmp);
felem_mul(tmp, x_in, z1);
felem_reduce(x_in, tmp);
felem_contract(x_out, x_in);
if (x != NULL) {
if (!smallfelem_to_BN(x, x_out)) {
ECerr(EC_F_EC_GFP_NISTP256_POINT_GET_AFFINE_COORDINATES,
ERR_R_BN_LIB);
return 0;
}
}
felem_mul(tmp, z1, z2);
felem_reduce(z1, tmp);
felem_mul(tmp, y_in, z1);
felem_reduce(y_in, tmp);
felem_contract(y_out, y_in);
if (y != NULL) {
if (!smallfelem_to_BN(y, y_out)) {
ECerr(EC_F_EC_GFP_NISTP256_POINT_GET_AFFINE_COORDINATES,
ERR_R_BN_LIB);
return 0;
}
}
return 1;
}
static void make_points_affine(size_t num, smallfelem points[][3],
smallfelem tmp_smallfelems[])
{
ec_GFp_nistp_points_make_affine_internal(num,
points,
sizeof(smallfelem),
tmp_smallfelems,
(void (*)(void *))smallfelem_one,
(int (*)(const void *))
smallfelem_is_zero_int,
(void (*)(void *, const void *))
smallfelem_assign,
(void (*)(void *, const void *))
smallfelem_square_contract,
(void (*)
(void *, const void *,
const void *))
smallfelem_mul_contract,
(void (*)(void *, const void *))
smallfelem_inv_contract,
(void (*)(void *, const void *))
smallfelem_assign);
}
int ec_GFp_nistp256_points_mul(const EC_GROUP *group, EC_POINT *r,
const BIGNUM *scalar, size_t num,
const EC_POINT *points[],
const BIGNUM *scalars[], BN_CTX *ctx)
{
int ret = 0;
int j;
int mixed = 0;
BN_CTX *new_ctx = NULL;
BIGNUM *x, *y, *z, *tmp_scalar;
felem_bytearray g_secret;
felem_bytearray *secrets = NULL;
smallfelem(*pre_comp)[17][3] = NULL;
smallfelem *tmp_smallfelems = NULL;
felem_bytearray tmp;
unsigned i, num_bytes;
int have_pre_comp = 0;
size_t num_points = num;
smallfelem x_in, y_in, z_in;
felem x_out, y_out, z_out;
NISTP256_PRE_COMP *pre = NULL;
const smallfelem(*g_pre_comp)[16][3] = NULL;
EC_POINT *generator = NULL;
const EC_POINT *p = NULL;
const BIGNUM *p_scalar = NULL;
if (ctx == NULL)
if ((ctx = new_ctx = BN_CTX_new()) == NULL)
return 0;
BN_CTX_start(ctx);
if (((x = BN_CTX_get(ctx)) == NULL) ||
((y = BN_CTX_get(ctx)) == NULL) ||
((z = BN_CTX_get(ctx)) == NULL) ||
((tmp_scalar = BN_CTX_get(ctx)) == NULL))
goto err;
if (scalar != NULL) {
pre = EC_EX_DATA_get_data(group->extra_data,
nistp256_pre_comp_dup,
nistp256_pre_comp_free,
nistp256_pre_comp_clear_free);
if (pre)
g_pre_comp = (const smallfelem(*)[16][3])pre->g_pre_comp;
else
g_pre_comp = &gmul[0];
generator = EC_POINT_new(group);
if (generator == NULL)
goto err;
if (!smallfelem_to_BN(x, g_pre_comp[0][1][0]) ||
!smallfelem_to_BN(y, g_pre_comp[0][1][1]) ||
!smallfelem_to_BN(z, g_pre_comp[0][1][2])) {
ECerr(EC_F_EC_GFP_NISTP256_POINTS_MUL, ERR_R_BN_LIB);
goto err;
}
if (!EC_POINT_set_Jprojective_coordinates_GFp(group,
generator, x, y, z,
ctx))
goto err;
if (0 == EC_POINT_cmp(group, generator, group->generator, ctx))
have_pre_comp = 1;
else
num_points++;
}
if (num_points > 0) {
if (num_points >= 3) {
mixed = 1;
}
secrets = OPENSSL_malloc(num_points * sizeof(felem_bytearray));
pre_comp = OPENSSL_malloc(num_points * 17 * 3 * sizeof(smallfelem));
if (mixed)
tmp_smallfelems =
OPENSSL_malloc((num_points * 17 + 1) * sizeof(smallfelem));
if ((secrets == NULL) || (pre_comp == NULL)
|| (mixed && (tmp_smallfelems == NULL))) {
ECerr(EC_F_EC_GFP_NISTP256_POINTS_MUL, ERR_R_MALLOC_FAILURE);
goto err;
}
memset(secrets, 0, num_points * sizeof(felem_bytearray));
memset(pre_comp, 0, num_points * 17 * 3 * sizeof(smallfelem));
for (i = 0; i < num_points; ++i) {
if (i == num)
{
p = EC_GROUP_get0_generator(group);
p_scalar = scalar;
} else
{
p = points[i];
p_scalar = scalars[i];
}
if ((p_scalar != NULL) && (p != NULL)) {
if ((BN_num_bits(p_scalar) > 256)
|| (BN_is_negative(p_scalar))) {
if (!BN_nnmod(tmp_scalar, p_scalar, &group->order, ctx)) {
ECerr(EC_F_EC_GFP_NISTP256_POINTS_MUL, ERR_R_BN_LIB);
goto err;
}
num_bytes = BN_bn2bin(tmp_scalar, tmp);
} else
num_bytes = BN_bn2bin(p_scalar, tmp);
flip_endian(secrets[i], tmp, num_bytes);
if ((!BN_to_felem(x_out, &p->X)) ||
(!BN_to_felem(y_out, &p->Y)) ||
(!BN_to_felem(z_out, &p->Z)))
goto err;
felem_shrink(pre_comp[i][1][0], x_out);
felem_shrink(pre_comp[i][1][1], y_out);
felem_shrink(pre_comp[i][1][2], z_out);
for (j = 2; j <= 16; ++j) {
if (j & 1) {
point_add_small(pre_comp[i][j][0], pre_comp[i][j][1],
pre_comp[i][j][2], pre_comp[i][1][0],
pre_comp[i][1][1], pre_comp[i][1][2],
pre_comp[i][j - 1][0],
pre_comp[i][j - 1][1],
pre_comp[i][j - 1][2]);
} else {
point_double_small(pre_comp[i][j][0],
pre_comp[i][j][1],
pre_comp[i][j][2],
pre_comp[i][j / 2][0],
pre_comp[i][j / 2][1],
pre_comp[i][j / 2][2]);
}
}
}
}
if (mixed)
make_points_affine(num_points * 17, pre_comp[0], tmp_smallfelems);
}
if ((scalar != NULL) && (have_pre_comp)) {
memset(g_secret, 0, sizeof(g_secret));
if ((BN_num_bits(scalar) > 256) || (BN_is_negative(scalar))) {
if (!BN_nnmod(tmp_scalar, scalar, &group->order, ctx)) {
ECerr(EC_F_EC_GFP_NISTP256_POINTS_MUL, ERR_R_BN_LIB);
goto err;
}
num_bytes = BN_bn2bin(tmp_scalar, tmp);
} else
num_bytes = BN_bn2bin(scalar, tmp);
flip_endian(g_secret, tmp, num_bytes);
batch_mul(x_out, y_out, z_out,
(const felem_bytearray(*))secrets, num_points,
g_secret,
mixed, (const smallfelem(*)[17][3])pre_comp, g_pre_comp);
} else
batch_mul(x_out, y_out, z_out,
(const felem_bytearray(*))secrets, num_points,
NULL, mixed, (const smallfelem(*)[17][3])pre_comp, NULL);
felem_contract(x_in, x_out);
felem_contract(y_in, y_out);
felem_contract(z_in, z_out);
if ((!smallfelem_to_BN(x, x_in)) || (!smallfelem_to_BN(y, y_in)) ||
(!smallfelem_to_BN(z, z_in))) {
ECerr(EC_F_EC_GFP_NISTP256_POINTS_MUL, ERR_R_BN_LIB);
goto err;
}
ret = EC_POINT_set_Jprojective_coordinates_GFp(group, r, x, y, z, ctx);
err:
BN_CTX_end(ctx);
if (generator != NULL)
EC_POINT_free(generator);
if (new_ctx != NULL)
BN_CTX_free(new_ctx);
if (secrets != NULL)
OPENSSL_free(secrets);
if (pre_comp != NULL)
OPENSSL_free(pre_comp);
if (tmp_smallfelems != NULL)
OPENSSL_free(tmp_smallfelems);
return ret;
}
int ec_GFp_nistp256_precompute_mult(EC_GROUP *group, BN_CTX *ctx)
{
int ret = 0;
NISTP256_PRE_COMP *pre = NULL;
int i, j;
BN_CTX *new_ctx = NULL;
BIGNUM *x, *y;
EC_POINT *generator = NULL;
smallfelem tmp_smallfelems[32];
felem x_tmp, y_tmp, z_tmp;
EC_EX_DATA_free_data(&group->extra_data, nistp256_pre_comp_dup,
nistp256_pre_comp_free,
nistp256_pre_comp_clear_free);
if (ctx == NULL)
if ((ctx = new_ctx = BN_CTX_new()) == NULL)
return 0;
BN_CTX_start(ctx);
if (((x = BN_CTX_get(ctx)) == NULL) || ((y = BN_CTX_get(ctx)) == NULL))
goto err;
if (group->generator == NULL)
goto err;
generator = EC_POINT_new(group);
if (generator == NULL)
goto err;
BN_bin2bn(nistp256_curve_params[3], sizeof(felem_bytearray), x);
BN_bin2bn(nistp256_curve_params[4], sizeof(felem_bytearray), y);
if (!EC_POINT_set_affine_coordinates_GFp(group, generator, x, y, ctx))
goto err;
if ((pre = nistp256_pre_comp_new()) == NULL)
goto err;
if (0 == EC_POINT_cmp(group, generator, group->generator, ctx)) {
memcpy(pre->g_pre_comp, gmul, sizeof(pre->g_pre_comp));
ret = 1;
goto err;
}
if ((!BN_to_felem(x_tmp, &group->generator->X)) ||
(!BN_to_felem(y_tmp, &group->generator->Y)) ||
(!BN_to_felem(z_tmp, &group->generator->Z)))
goto err;
felem_shrink(pre->g_pre_comp[0][1][0], x_tmp);
felem_shrink(pre->g_pre_comp[0][1][1], y_tmp);
felem_shrink(pre->g_pre_comp[0][1][2], z_tmp);
for (i = 1; i <= 8; i <<= 1) {
point_double_small(pre->g_pre_comp[1][i][0], pre->g_pre_comp[1][i][1],
pre->g_pre_comp[1][i][2], pre->g_pre_comp[0][i][0],
pre->g_pre_comp[0][i][1],
pre->g_pre_comp[0][i][2]);
for (j = 0; j < 31; ++j) {
point_double_small(pre->g_pre_comp[1][i][0],
pre->g_pre_comp[1][i][1],
pre->g_pre_comp[1][i][2],
pre->g_pre_comp[1][i][0],
pre->g_pre_comp[1][i][1],
pre->g_pre_comp[1][i][2]);
}
if (i == 8)
break;
point_double_small(pre->g_pre_comp[0][2 * i][0],
pre->g_pre_comp[0][2 * i][1],
pre->g_pre_comp[0][2 * i][2],
pre->g_pre_comp[1][i][0], pre->g_pre_comp[1][i][1],
pre->g_pre_comp[1][i][2]);
for (j = 0; j < 31; ++j) {
point_double_small(pre->g_pre_comp[0][2 * i][0],
pre->g_pre_comp[0][2 * i][1],
pre->g_pre_comp[0][2 * i][2],
pre->g_pre_comp[0][2 * i][0],
pre->g_pre_comp[0][2 * i][1],
pre->g_pre_comp[0][2 * i][2]);
}
}
for (i = 0; i < 2; i++) {
memset(pre->g_pre_comp[i][0], 0, sizeof(pre->g_pre_comp[i][0]));
point_add_small(pre->g_pre_comp[i][6][0], pre->g_pre_comp[i][6][1],
pre->g_pre_comp[i][6][2], pre->g_pre_comp[i][4][0],
pre->g_pre_comp[i][4][1], pre->g_pre_comp[i][4][2],
pre->g_pre_comp[i][2][0], pre->g_pre_comp[i][2][1],
pre->g_pre_comp[i][2][2]);
point_add_small(pre->g_pre_comp[i][10][0], pre->g_pre_comp[i][10][1],
pre->g_pre_comp[i][10][2], pre->g_pre_comp[i][8][0],
pre->g_pre_comp[i][8][1], pre->g_pre_comp[i][8][2],
pre->g_pre_comp[i][2][0], pre->g_pre_comp[i][2][1],
pre->g_pre_comp[i][2][2]);
point_add_small(pre->g_pre_comp[i][12][0], pre->g_pre_comp[i][12][1],
pre->g_pre_comp[i][12][2], pre->g_pre_comp[i][8][0],
pre->g_pre_comp[i][8][1], pre->g_pre_comp[i][8][2],
pre->g_pre_comp[i][4][0], pre->g_pre_comp[i][4][1],
pre->g_pre_comp[i][4][2]);
point_add_small(pre->g_pre_comp[i][14][0], pre->g_pre_comp[i][14][1],
pre->g_pre_comp[i][14][2], pre->g_pre_comp[i][12][0],
pre->g_pre_comp[i][12][1], pre->g_pre_comp[i][12][2],
pre->g_pre_comp[i][2][0], pre->g_pre_comp[i][2][1],
pre->g_pre_comp[i][2][2]);
for (j = 1; j < 8; ++j) {
point_add_small(pre->g_pre_comp[i][2 * j + 1][0],
pre->g_pre_comp[i][2 * j + 1][1],
pre->g_pre_comp[i][2 * j + 1][2],
pre->g_pre_comp[i][2 * j][0],
pre->g_pre_comp[i][2 * j][1],
pre->g_pre_comp[i][2 * j][2],
pre->g_pre_comp[i][1][0],
pre->g_pre_comp[i][1][1],
pre->g_pre_comp[i][1][2]);
}
}
make_points_affine(31, &(pre->g_pre_comp[0][1]), tmp_smallfelems);
if (!EC_EX_DATA_set_data(&group->extra_data, pre, nistp256_pre_comp_dup,
nistp256_pre_comp_free,
nistp256_pre_comp_clear_free))
goto err;
ret = 1;
pre = NULL;
err:
BN_CTX_end(ctx);
if (generator != NULL)
EC_POINT_free(generator);
if (new_ctx != NULL)
BN_CTX_free(new_ctx);
if (pre)
nistp256_pre_comp_free(pre);
return ret;
}
int ec_GFp_nistp256_have_precompute_mult(const EC_GROUP *group)
{
if (EC_EX_DATA_get_data(group->extra_data, nistp256_pre_comp_dup,
nistp256_pre_comp_free,
nistp256_pre_comp_clear_free)
!= NULL)
return 1;
else
return 0;
}
#else
static void *dummy = &dummy;
#endif