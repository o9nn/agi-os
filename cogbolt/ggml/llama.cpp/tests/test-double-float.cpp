#undef NDEBUG
#include <cassert>
#if !defined(__riscv) && !defined(__s390__) && !defined(__ARM_NEON)
#include <immintrin.h>
#endif
#include <cmath>
#include <cstdint>
#include <cstring>
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdouble-promotion"
inline static uint8_t round_orig(float v0) { return ((int8_t) (round(v0))) + 8; }
inline static float silu_orig(float x) {
return x/(1.0 + exp(-x));
}
#pragma GCC diagnostic pop
inline static uint8_t round_float(float v0) { return (int8_t)roundf(v0) + 8; }
inline static float silu_float(float x) {
return x/(1.0f + expf(-x));
}
int main(void) {
uint32_t x = UINT32_MAX;
do {
float f;
memcpy(&f, &x, sizeof(x));
assert(!std::isfinite(f) || (round_orig(f) == round_float(f)));
} while (x--);
#ifdef __F16C__
for (x = 0; x <= UINT16_MAX; x++) {
float f = _cvtsh_ss(x);
const float so = silu_orig(f);
const float sf = silu_float(f);
assert(   (_cvtss_sh(so, 0) == _cvtss_sh(sf, 0))
|| (nextafterf(so, sf) == sf)
|| (nextafterf(sf, so) == so));
}
#endif
}