#define GGML_COMMON_IMPL_CPP
#define GGML_COMMON_DECL_CPP
#include "ggml-common.h"
#include "ggml-backend-impl.h"
#include "ggml-quants.h"
#include "ggml-impl.h"
#include "ggml-cpu.h"
#include "ggml-cpu-impl.h"
#include "ggml-cpu-traits.h"
#include <cmath>
#include <cstring>
#include <cassert>
#include <cfloat>
#include <cstdlib>
#include <cstdio>
#include "ggml-cpu-aarch64.h"
template <int K> constexpr int QK_0() {
if constexpr (K == 4) {
return QK4_0;
}
if constexpr (K == 8) {
return QK8_0;
}
return -1;
}
template <int K, int N> struct block {
ggml_half d[N];
int8_t    qs[(QK_0<K>() * N * K) / 8];
};
static_assert(sizeof(block<4, 4>) == 4 * sizeof(ggml_half) + QK8_0 * 2, "wrong block<4,4> size/padding");
static_assert(sizeof(block<4, 8>) == 8 * sizeof(ggml_half) + QK8_0 * 4, "wrong block<4,8> size/padding");
static_assert(sizeof(block<8, 4>) == 4 * sizeof(ggml_half) + QK8_0 * 4, "wrong block<8,4> size/padding");
static_assert(sizeof(block<8, 8>) == 8 * sizeof(ggml_half) + QK8_0 * 8, "wrong block<8,8> size/padding");
using block_q4_0x4 = block<4, 4>;
using block_q4_0x8 = block<4, 8>;
using block_q8_0x4 = block<8, 4>;
using block_q8_0x8 = block<8, 8>;
struct block_q4_Kx8 {
ggml_half d[8];
ggml_half dmin[8];
uint8_t scales[96];
uint8_t qs[1024];
};
static_assert(sizeof(block_q4_Kx8) == sizeof(ggml_half) * 16 + K_SCALE_SIZE * 8 + QK_K * 4, "wrong q4_K block size/padding");
struct block_q8_Kx4 {
float d[4];
int8_t qs[QK_K * 4];
int16_t bsums[QK_K / 4];
};
static_assert(sizeof(block_q8_Kx4) == sizeof(float) * 4 + QK_K * 4 + (QK_K / 4) * sizeof(int16_t), "wrong q8_K block size/padding");
struct block_iq4_nlx4 {
ggml_half d[4];
uint8_t   qs[QK4_NL * 2];
};
static_assert(sizeof(block_iq4_nlx4) == 4 * sizeof(ggml_half) + QK4_NL * 2, "wrong iq4_nlx4 block size/padding");
#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Woverlength-strings"
#endif
#define UNUSED GGML_UNUSED
static inline int nearest_int(float fval) {
assert(fabsf(fval) <= 4194303.f);
float val = fval + 12582912.f;
int i; memcpy(&i, &val, sizeof(int));
return (i & 0x007fffff) - 0x00400000;
}
#if defined(__AVX__)
#if defined(__F16C__)
#if defined(__AVX512F__)
#define GGML_F32Cx8x2_LOAD(x, y)     _mm512_cvtph_ps(_mm256_set_m128i(_mm_loadu_si128((const __m128i *)(y)), _mm_loadu_si128((const __m128i *)(x))))
#define GGML_F32Cx16_REPEAT_LOAD(x)  _mm512_cvtph_ps(_mm256_set_m128i(x, x))
#endif
#define GGML_F32Cx8_LOAD(x)     _mm256_cvtph_ps(_mm_loadu_si128((const __m128i *)(x)))
#define GGML_F32Cx8_REPEAT_LOAD(x, loadMask)     _mm256_cvtph_ps(_mm_shuffle_epi32(_mm_maskload_epi32((int const*)(x), loadMask), 68))
#define GGML_F32Cx8_REARRANGE_LOAD(x, arrangeMask)     _mm256_cvtph_ps(_mm_shuffle_epi8(_mm_loadu_si128((const __m128i *) x), arrangeMask))
#else
#if defined(__AVX512F__)
static inline __m512 __avx512_f32cx8x2_load(ggml_fp16_t *x, ggml_fp16_t *y) {
float tmp[16];
for (int i = 0; i < 8; i++) {
tmp[i] = GGML_FP16_TO_FP32(x[i]);
}
for (int i = 0; i < 8; i++) {
tmp[i + 8] = GGML_FP16_TO_FP32(y[i]);
}
return _mm512_loadu_ps(tmp);
}
static inline __m512 __avx512_repeat_f32cx16_load(__m128i x) {
float tmp[16];
uint16_t tmphalf[8];
_mm_storeu_si128((__m128i*)tmphalf, x);
for (int i = 0; i < 4; i++) {
tmp[i] = GGML_FP16_TO_FP32(tmphalf[i]);
tmp[i + 4] = GGML_FP16_TO_FP32(tmphalf[i]);
tmp[i + 8] = GGML_FP16_TO_FP32(tmphalf[i]);
tmp[i + 12] = GGML_FP16_TO_FP32(tmphalf[i]);
}
return _mm512_loadu_ps(tmp);
}
#endif
static inline __m256 __avx_f32cx8_load(ggml_fp16_t *x) {
float tmp[8];
for (int i = 0; i < 8; i++) {
tmp[i] = GGML_FP16_TO_FP32(x[i]);
}
return _mm256_loadu_ps(tmp);
}
static inline __m256 __avx_repeat_f32cx8_load(ggml_fp16_t *x) {
float tmp[8];
for (int i = 0; i < 4; i++) {
tmp[i] = GGML_FP16_TO_FP32(x[i]);
tmp[i + 4] = GGML_FP16_TO_FP32(x[i]);
}
return _mm256_loadu_ps(tmp);
}
static inline __m256 __avx_rearranged_f32cx8_load(ggml_fp16_t *x, __m128i arrangeMask) {
uint16_t tmphalf[8];
float tmp[8];
_mm_storeu_si128((__m128i*)tmphalf, _mm_shuffle_epi8(_mm_loadu_si128((const __m128i *) x), arrangeMask));
for (int i = 0; i < 8; i++) {
tmp[i] = GGML_FP16_TO_FP32(tmphalf[i]);
}
return _mm256_loadu_ps(tmp);
}
#define GGML_F32Cx8_LOAD(x)     __avx_f32cx8_load(x)
#define GGML_F32Cx8_REPEAT_LOAD(x, loadMask)     __avx_repeat_f32cx8_load(x)
#define GGML_F32Cx8_REARRANGE_LOAD(x, arrangeMask)     __avx_rearranged_f32cx8_load(x, arrangeMask)
#if defined(__AVX512F__)
#define GGML_F32Cx8x2_LOAD(x, y)     __avx512_f32cx8x2_load(x, y)
#define GGML_F32Cx16_REPEAT_LOAD(x)  __avx512_repeat_f32cx16_load(x)
#endif
#endif
#endif
#if defined(__AVX2__) || defined(__AVX512F__)
#if defined(__AVX512F__)
static inline __m512i sum_i16_pairs_acc_int32x16(const __m512i acc, const __m512i x) {
const __m512i ones = _mm512_set1_epi16(1);
return _mm512_add_epi32(acc, _mm512_madd_epi16(ones, x));
}
static inline __m512i mul_sum_us8_pairs_acc_int32x16(const __m512i acc, const __m512i ax, const __m512i sy) {
#if defined(__AVX512VNNI__)
return _mm512_dpbusd_epi32(acc, ax, sy);
#else
const __m512i dot = _mm512_maddubs_epi16(ax, sy);
return sum_i16_pairs_acc_int32x16(acc, dot);
#endif
}
static inline __m512i mul_sum_i8_pairs_acc_int32x16(const __m512i acc, const __m512i x, const __m512i y) {
const __m512i zero = _mm512_setzero_si512();
const __m512i ax = _mm512_abs_epi8(x);
__mmask64 blt0 = _mm512_movepi8_mask(x);
const __m512i sy = _mm512_mask_sub_epi8(y, blt0, zero, y);
return mul_sum_us8_pairs_acc_int32x16(acc, ax, sy);
}
#endif
static inline __m256i sum_i16_pairs_acc_int32x8(const __m256i acc, const __m256i x) {
const __m256i ones = _mm256_set1_epi16(1);
return _mm256_add_epi32(acc, _mm256_madd_epi16(ones, x));
}
static inline __m256i mul_sum_us8_pairs_acc_int32x8(const __m256i acc, const __m256i ax, const __m256i sy) {
#if defined(__AVX512VNNI__) && defined(__AVX512VL__)
return _mm256_dpbusd_epi32(acc, ax, sy);
#elif defined(__AVXVNNI__)
return _mm256_dpbusd_avx_epi32(acc, ax, sy);
#else
const __m256i dot = _mm256_maddubs_epi16(ax, sy);
return sum_i16_pairs_acc_int32x8(acc, dot);
#endif
}
static inline __m256i mul_sum_i8_pairs_acc_int32x8(const __m256i acc, const __m256i x, const __m256i y) {
#if defined(__AVXVNNIINT8__)
return _mm256_dpbssd_epi32(acc, x, y);
#else
const __m256i ax = _mm256_sign_epi8(x, x);
const __m256i sy = _mm256_sign_epi8(y, x);
return mul_sum_us8_pairs_acc_int32x8(acc, ax, sy);
#endif
}
#endif
static const int8_t kvalues_iq4nl[16] = {-127, -104, -83, -65, -49, -35, -22, -10, 1, 13, 25, 38, 53, 69, 89, 113};
static void ggml_quantize_mat_q8_0_4x4(const float * GGML_RESTRICT x, void * GGML_RESTRICT vy, int64_t k) {
assert(QK8_0 == 32);
assert(k % QK8_0 == 0);
const int nb = k / QK8_0;
block_q8_0x4 * GGML_RESTRICT y = (block_q8_0x4 *) vy;
#if defined(__ARM_NEON)
float32x4_t srcv[4][8];
float id[4];
for (int i = 0; i < nb; i++) {
float32x4_t asrcv[8];
float32x4_t amaxv[8];
for (int row_iter = 0; row_iter < 4; row_iter++) {
for (int j = 0; j < 8; j++) srcv[row_iter][j] = vld1q_f32(x + row_iter * k + i * 32 + 4 * j);
for (int j = 0; j < 8; j++) asrcv[j] = vabsq_f32(srcv[row_iter][j]);
for (int j = 0; j < 4; j++) amaxv[2 * j] = vmaxq_f32(asrcv[2 * j], asrcv[2 * j + 1]);
for (int j = 0; j < 2; j++) amaxv[4 * j] = vmaxq_f32(amaxv[4 * j], amaxv[4 * j + 2]);
for (int j = 0; j < 1; j++) amaxv[8 * j] = vmaxq_f32(amaxv[8 * j], amaxv[8 * j + 4]);
const float amax = vmaxvq_f32(amaxv[0]);
const float d = amax / ((1 << 7) - 1);
id[row_iter] = d ? 1.0f / d : 0.0f;
y[i].d[row_iter] = GGML_FP32_TO_FP16(d);
}
for (int j = 0; j < 8; j++) {
float32x4_t v = vmulq_n_f32(srcv[0][j], id[0]);
int32x4_t vi = vcvtnq_s32_f32(v);
y[i].qs[16 * j + 0] = vgetq_lane_s32(vi, 0);
y[i].qs[16 * j + 1] = vgetq_lane_s32(vi, 1);
y[i].qs[16 * j + 2] = vgetq_lane_s32(vi, 2);
y[i].qs[16 * j + 3] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[1][j], id[1]);
vi = vcvtnq_s32_f32(v);
y[i].qs[16 * j + 4] = vgetq_lane_s32(vi, 0);
y[i].qs[16 * j + 5] = vgetq_lane_s32(vi, 1);
y[i].qs[16 * j + 6] = vgetq_lane_s32(vi, 2);
y[i].qs[16 * j + 7] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[2][j], id[2]);
vi = vcvtnq_s32_f32(v);
y[i].qs[16 * j + 8] = vgetq_lane_s32(vi, 0);
y[i].qs[16 * j + 9] = vgetq_lane_s32(vi, 1);
y[i].qs[16 * j + 10] = vgetq_lane_s32(vi, 2);
y[i].qs[16 * j + 11] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[3][j], id[3]);
vi = vcvtnq_s32_f32(v);
y[i].qs[16 * j + 12] = vgetq_lane_s32(vi, 0);
y[i].qs[16 * j + 13] = vgetq_lane_s32(vi, 1);
y[i].qs[16 * j + 14] = vgetq_lane_s32(vi, 2);
y[i].qs[16 * j + 15] = vgetq_lane_s32(vi, 3);
}
}
#else
const int blck_size_interleave = 4;
float srcv[4][QK8_0];
float id[4];
for (int i = 0; i < nb; i++) {
for (int row_iter = 0; row_iter < 4; row_iter++) {
float amax = 0.0f;
for (int j = 0; j < QK8_0; j++) {
srcv[row_iter][j] = x[row_iter * k + i * QK8_0 + j];
amax = MAX(amax, fabsf(srcv[row_iter][j]));
}
const float d = amax / ((1 << 7) - 1);
id[row_iter] = d ? 1.0f / d : 0.0f;
y[i].d[row_iter] = GGML_FP32_TO_FP16(d);
}
for (int j = 0; j < QK8_0 * 4; j++) {
int src_offset = (j / (4 * blck_size_interleave)) * blck_size_interleave;
int src_id = (j % (4 * blck_size_interleave)) / blck_size_interleave;
src_offset += (j % blck_size_interleave);
float x0 = srcv[src_id][src_offset] * id[src_id];
y[i].qs[j] = roundf(x0);
}
}
#endif
}
static void ggml_quantize_mat_q8_0_4x8(const float * GGML_RESTRICT x, void * GGML_RESTRICT vy, int64_t k) {
assert(QK8_0 == 32);
assert(k % QK8_0 == 0);
const int nb = k / QK8_0;
block_q8_0x4 * GGML_RESTRICT y = (block_q8_0x4 *) vy;
#if defined(__ARM_NEON)
float32x4_t srcv[4][8];
float id[4];
for (int i = 0; i < nb; i++) {
float32x4_t asrcv[8];
float32x4_t amaxv[8];
for (int row_iter = 0; row_iter < 4; row_iter++) {
for (int j = 0; j < 8; j++) srcv[row_iter][j] = vld1q_f32(x + row_iter * k + i * 32 + 4 * j);
for (int j = 0; j < 8; j++) asrcv[j] = vabsq_f32(srcv[row_iter][j]);
for (int j = 0; j < 4; j++) amaxv[2 * j] = vmaxq_f32(asrcv[2 * j], asrcv[2 * j + 1]);
for (int j = 0; j < 2; j++) amaxv[4 * j] = vmaxq_f32(amaxv[4 * j], amaxv[4 * j + 2]);
for (int j = 0; j < 1; j++) amaxv[8 * j] = vmaxq_f32(amaxv[8 * j], amaxv[8 * j + 4]);
const float amax = vmaxvq_f32(amaxv[0]);
const float d = amax / ((1 << 7) - 1);
id[row_iter] = d ? 1.0f / d : 0.0f;
y[i].d[row_iter] = GGML_FP32_TO_FP16(d);
}
for (int j = 0; j < 4; j++) {
float32x4_t v = vmulq_n_f32(srcv[0][2 * j], id[0]);
int32x4_t vi = vcvtnq_s32_f32(v);
y[i].qs[32 * j + 0] = vgetq_lane_s32(vi, 0);
y[i].qs[32 * j + 1] = vgetq_lane_s32(vi, 1);
y[i].qs[32 * j + 2] = vgetq_lane_s32(vi, 2);
y[i].qs[32 * j + 3] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[0][2 * j + 1], id[0]);
vi = vcvtnq_s32_f32(v);
y[i].qs[32 * j + 4] = vgetq_lane_s32(vi, 0);
y[i].qs[32 * j + 5] = vgetq_lane_s32(vi, 1);
y[i].qs[32 * j + 6] = vgetq_lane_s32(vi, 2);
y[i].qs[32 * j + 7] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[1][2 * j], id[1]);
vi = vcvtnq_s32_f32(v);
y[i].qs[32 * j + 8] = vgetq_lane_s32(vi, 0);
y[i].qs[32 * j + 9] = vgetq_lane_s32(vi, 1);
y[i].qs[32 * j + 10] = vgetq_lane_s32(vi, 2);
y[i].qs[32 * j + 11] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[1][2 * j + 1], id[1]);
vi = vcvtnq_s32_f32(v);
y[i].qs[32 * j + 12] = vgetq_lane_s32(vi, 0);
y[i].qs[32 * j + 13] = vgetq_lane_s32(vi, 1);
y[i].qs[32 * j + 14] = vgetq_lane_s32(vi, 2);
y[i].qs[32 * j + 15] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[2][2 * j], id[2]);
vi = vcvtnq_s32_f32(v);
y[i].qs[32 * j + 16] = vgetq_lane_s32(vi, 0);
y[i].qs[32 * j + 17] = vgetq_lane_s32(vi, 1);
y[i].qs[32 * j + 18] = vgetq_lane_s32(vi, 2);
y[i].qs[32 * j + 19] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[2][2 * j + 1], id[2]);
vi = vcvtnq_s32_f32(v);
y[i].qs[32 * j + 20] = vgetq_lane_s32(vi, 0);
y[i].qs[32 * j + 21] = vgetq_lane_s32(vi, 1);
y[i].qs[32 * j + 22] = vgetq_lane_s32(vi, 2);
y[i].qs[32 * j + 23] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[3][2 * j], id[3]);
vi = vcvtnq_s32_f32(v);
y[i].qs[32 * j + 24] = vgetq_lane_s32(vi, 0);
y[i].qs[32 * j + 25] = vgetq_lane_s32(vi, 1);
y[i].qs[32 * j + 26] = vgetq_lane_s32(vi, 2);
y[i].qs[32 * j + 27] = vgetq_lane_s32(vi, 3);
v = vmulq_n_f32(srcv[3][2 * j + 1], id[3]);
vi = vcvtnq_s32_f32(v);
y[i].qs[32 * j + 28] = vgetq_lane_s32(vi, 0);
y[i].qs[32 * j + 29] = vgetq_lane_s32(vi, 1);
y[i].qs[32 * j + 30] = vgetq_lane_s32(vi, 2);
y[i].qs[32 * j + 31] = vgetq_lane_s32(vi, 3);
}
}
#elif defined(__AVX2__) || defined(__AVX__)
float id[4];
__m256 srcv[4][4];
__m256 idvec[4];
for (int i = 0; i < nb; i++) {
for (int row_iter = 0; row_iter < 4; row_iter++) {
__m256 v0 = _mm256_loadu_ps( x + row_iter * k + i * 32 );
__m256 v1 = _mm256_loadu_ps( x + row_iter * k + i * 32 + 8 );
__m256 v2 = _mm256_loadu_ps( x + row_iter * k + i * 32 + 16 );
__m256 v3 = _mm256_loadu_ps( x + row_iter * k + i * 32 + 24 );
const __m256 signBit = _mm256_set1_ps( -0.0f );
__m256 maxAbs = _mm256_andnot_ps( signBit, v0 );
maxAbs = _mm256_max_ps( maxAbs, _mm256_andnot_ps( signBit, v1 ) );
maxAbs = _mm256_max_ps( maxAbs, _mm256_andnot_ps( signBit, v2 ) );
maxAbs = _mm256_max_ps( maxAbs, _mm256_andnot_ps( signBit, v3 ) );
__m128 max4 = _mm_max_ps( _mm256_extractf128_ps( maxAbs, 1 ), _mm256_castps256_ps128( maxAbs ) );
max4 = _mm_max_ps( max4, _mm_movehl_ps( max4, max4 ) );
max4 = _mm_max_ss( max4, _mm_movehdup_ps( max4 ) );
const float maxScalar = _mm_cvtss_f32( max4 );
const float d = maxScalar  / 127.f;
id[row_iter] = ( maxScalar != 0.0f ) ? 127.f / maxScalar : 0.0f;
y[i].d[row_iter] = GGML_FP32_TO_FP16(d);
srcv[row_iter][0] = v0;
srcv[row_iter][1] = v1;
srcv[row_iter][2] = v2;
srcv[row_iter][3] = v3;
idvec[row_iter] = _mm256_set1_ps(id[row_iter]);
}
for (int j = 0; j < 4; j++) {
__m256 v0 = _mm256_mul_ps(srcv[0][j], idvec[0]);
__m256 v1 = _mm256_mul_ps(srcv[1][j], idvec[1]);
__m256 v2 = _mm256_mul_ps(srcv[2][j], idvec[2]);
__m256 v3 = _mm256_mul_ps(srcv[3][j], idvec[3]);
v0 = _mm256_round_ps( v0, _MM_ROUND_NEAREST );
v1 = _mm256_round_ps( v1, _MM_ROUND_NEAREST );
v2 = _mm256_round_ps( v2, _MM_ROUND_NEAREST );
v3 = _mm256_round_ps( v3, _MM_ROUND_NEAREST );
__m256i i0 = _mm256_cvtps_epi32( v0 );
__m256i i1 = _mm256_cvtps_epi32( v1 );
__m256i i2 = _mm256_cvtps_epi32( v2 );
__m256i i3 = _mm256_cvtps_epi32( v3 );
#if defined(__AVX2__)
i0 = _mm256_packs_epi32( i0, i1 );
i2 = _mm256_packs_epi32( i2, i3 );
i0 = _mm256_packs_epi16( i0, i2 );
const __m256i perm = _mm256_setr_epi32( 0, 4, 1, 5, 2, 6, 3, 7 );
i0 = _mm256_permutevar8x32_epi32( i0, perm );
_mm256_storeu_si256((__m256i *)(y[i].qs + 32 * j), i0);
#else
__m128i ni0 = _mm256_castsi256_si128( i0 );
__m128i ni1 = _mm256_extractf128_si256( i0, 1);
__m128i ni2 = _mm256_castsi256_si128( i1 );
__m128i ni3 = _mm256_extractf128_si256( i1, 1);
__m128i ni4 = _mm256_castsi256_si128( i2 );
__m128i ni5 = _mm256_extractf128_si256( i2, 1);
__m128i ni6 = _mm256_castsi256_si128( i3 );
__m128i ni7 = _mm256_extractf128_si256( i3, 1);
ni0 = _mm_packs_epi32( ni0, ni1 );
ni2 = _mm_packs_epi32( ni2, ni3 );
ni4 = _mm_packs_epi32( ni4, ni5 );
ni6 = _mm_packs_epi32( ni6, ni7 );
ni0 = _mm_packs_epi16( ni0, ni2 );
ni4 = _mm_packs_epi16( ni4, ni6 );
_mm_storeu_si128((__m128i *)(y[i].qs + 32 * j), ni0);
_mm_storeu_si128((__m128i *)(y[i].qs + 32 * j + 16), ni4);
#endif
}
}
#else
const int blck_size_interleave = 8;
float srcv[4][QK8_0];
float id[4];
for (int i = 0; i < nb; i++) {
for (int row_iter = 0; row_iter < 4; row_iter++) {
float amax = 0.0f;
for (int j = 0; j < QK8_0; j++) {
srcv[row_iter][j] = x[row_iter * k + i * QK8_0 + j];
amax = MAX(amax, fabsf(srcv[row_iter][j]));
}
const float d = amax / ((1 << 7) - 1);
id[row_iter] = d ? 1.0f / d : 0.0f;
y[i].d[row_iter] = GGML_FP32_TO_FP16(d);
}
for (int j = 0; j < QK8_0 * 4; j++) {
int src_offset = (j / (4 * blck_size_interleave)) * blck_size_interleave;
int src_id = (j % (4 * blck_size_interleave)) / blck_size_interleave;
src_offset += (j % blck_size_interleave);
float x0 = srcv[src_id][src_offset] * id[src_id];
y[i].qs[j] = roundf(x0);
}
}
#endif
}
static void ggml_quantize_mat_q8_K_4x8(const float * GGML_RESTRICT x, void * GGML_RESTRICT vy, int64_t k) {
assert(QK_K == 256);
assert(k % QK_K == 0);
const int nb = k / QK_K;
block_q8_Kx4 * GGML_RESTRICT y = (block_q8_Kx4 *) vy;
#if defined(__AVX2__)
float iscale[4];
__m256 srcv[4][32];
__m256 iscale_vec[4];
for (int i = 0; i < nb; i++) {
for (int row_iter = 0; row_iter < 4; row_iter++) {
__m256 v0 = _mm256_loadu_ps( x + row_iter * k + i * 256 );
__m256 v1 = _mm256_loadu_ps( x + row_iter * k + i * 256 + 8 );
__m256 v2 = _mm256_loadu_ps( x + row_iter * k + i * 256 + 16 );
__m256 v3 = _mm256_loadu_ps( x + row_iter * k + i * 256 + 24 );
const __m256 signBit = _mm256_set1_ps( -0.0f );
__m256 abs0 = _mm256_andnot_ps( signBit, v0 );
__m256 abs1 = _mm256_andnot_ps( signBit, v1 );
__m256 abs2 = _mm256_andnot_ps( signBit, v2 );
__m256 abs3 = _mm256_andnot_ps( signBit, v3 );
__m256 maxAbs = _mm256_max_ps( abs0, abs1 );
maxAbs = _mm256_max_ps( maxAbs, abs2 );
maxAbs = _mm256_max_ps( maxAbs, abs3 );
__m256 mask0 = _mm256_cmp_ps( maxAbs, v0, _CMP_EQ_OQ );
__m256 mask1 = _mm256_cmp_ps( maxAbs, v1, _CMP_EQ_OQ );
__m256 mask2 = _mm256_cmp_ps( maxAbs, v2, _CMP_EQ_OQ );
__m256 mask3 = _mm256_cmp_ps( maxAbs, v3, _CMP_EQ_OQ );
__m256 maskAbs = _mm256_or_ps(_mm256_or_ps(mask0, mask1),_mm256_or_ps(mask2, mask3));
srcv[row_iter][0] = v0;
srcv[row_iter][1] = v1;
srcv[row_iter][2] = v2;
srcv[row_iter][3] = v3;
for (int sb = 1; sb < 8; sb++) {
__m256 tempAbs = maxAbs;
__m256 v0 = _mm256_loadu_ps( x + row_iter * k + i * 256 + sb * 32);
__m256 v1 = _mm256_loadu_ps( x + row_iter * k + i * 256 + sb * 32 + 8 );
__m256 v2 = _mm256_loadu_ps( x + row_iter * k + i * 256 + sb * 32 + 16 );
__m256 v3 = _mm256_loadu_ps( x + row_iter * k + i * 256 + sb * 32 + 24 );
__m256 abs0 = _mm256_andnot_ps( signBit, v0 );
__m256 abs1 = _mm256_andnot_ps( signBit, v1 );
__m256 abs2 = _mm256_andnot_ps( signBit, v2 );
__m256 abs3 = _mm256_andnot_ps( signBit, v3 );
maxAbs = _mm256_max_ps( maxAbs, abs0 );
maxAbs = _mm256_max_ps( maxAbs, abs1 );
maxAbs = _mm256_max_ps( maxAbs, abs2 );
maxAbs = _mm256_max_ps( maxAbs, abs3 );
__m256 mask_prev = _mm256_cmp_ps( tempAbs, maxAbs, _CMP_EQ_OQ );
maskAbs = _mm256_and_ps( maskAbs, mask_prev );
mask0 = _mm256_cmp_ps( maxAbs, v0, _CMP_EQ_OQ );
mask1 = _mm256_cmp_ps( maxAbs, v1, _CMP_EQ_OQ );
mask2 = _mm256_cmp_ps( maxAbs, v2, _CMP_EQ_OQ );
mask3 = _mm256_cmp_ps( maxAbs, v3, _CMP_EQ_OQ );
__m256 mask_curr = _mm256_or_ps(_mm256_or_ps(mask0, mask1),_mm256_or_ps(mask2, mask3));
maskAbs =  _mm256_or_ps(maskAbs, mask_curr);
srcv[row_iter][sb * 4] = v0;
srcv[row_iter][sb * 4 + 1] = v1;
srcv[row_iter][sb * 4 + 2] = v2;
srcv[row_iter][sb * 4 + 3] = v3;
}
__m128 max4 = _mm_max_ps( _mm256_extractf128_ps( maxAbs, 1 ), _mm256_castps256_ps128( maxAbs ) );
max4 = _mm_max_ps( max4, _mm_movehl_ps( max4, max4 ) );
max4 = _mm_max_ss( max4, _mm_movehdup_ps( max4 ) );
const float maxScalar = _mm_cvtss_f32( max4 );
__m256 maxScalarVec = _mm256_set1_ps(maxScalar);
__m256 mask_next = _mm256_cmp_ps( maxScalarVec, maxAbs, _CMP_EQ_OQ );
__m256 finalMask = _mm256_and_ps(maskAbs, mask_next);
const int mask = _mm256_movemask_ps(finalMask);
iscale[row_iter] = ( maxScalar != 0.0f ) ? 127.f / maxScalar : 0.0f;
if(mask) {
iscale[row_iter] = ( maxScalar != 0.0f ) ? -127.f / maxScalar: 0.0f;
}
y[i].d[row_iter] = maxScalar ? 1/iscale[row_iter] : 0;
iscale_vec[row_iter] = _mm256_set1_ps(iscale[row_iter]);
}
__m256i quants_interleaved[32];
for (int j = 0; j < 32; j++) {
__m256 v0 = _mm256_mul_ps(srcv[0][j], iscale_vec[0]);
__m256 v1 = _mm256_mul_ps(srcv[1][j], iscale_vec[1]);
__m256 v2 = _mm256_mul_ps(srcv[2][j], iscale_vec[2]);
__m256 v3 = _mm256_mul_ps(srcv[3][j], iscale_vec[3]);
v0 = _mm256_round_ps( v0, _MM_ROUND_NEAREST );
v1 = _mm256_round_ps( v1, _MM_ROUND_NEAREST );
v2 = _mm256_round_ps( v2, _MM_ROUND_NEAREST );
v3 = _mm256_round_ps( v3, _MM_ROUND_NEAREST );
__m256i i0 = _mm256_cvtps_epi32( v0 );
__m256i i1 = _mm256_cvtps_epi32( v1 );
__m256i i2 = _mm256_cvtps_epi32( v2 );
__m256i i3 = _mm256_cvtps_epi32( v3 );
i0 = _mm256_packs_epi32( i0, i1 );
i2 = _mm256_packs_epi32( i2, i3 );
i0 = _mm256_packs_epi16( i0, i2 );
const __m256i perm = _mm256_setr_epi32( 0, 4, 1, 5, 2, 6, 3, 7 );
i0 = _mm256_permutevar8x32_epi32( i0, perm );
_mm256_storeu_si256((__m256i *)(y[i].qs + 32 * j), i0);
quants_interleaved[j] = i0;
}
__m256i shuffle_mask_sb2 = _mm256_castsi128_si256(_mm_setr_epi8(0, 1, 0, 1, 4, 5, 6, 7, 8, 9, 8, 9, 12, 13, 14, 15));
shuffle_mask_sb2 = _mm256_permute2f128_si256(shuffle_mask_sb2, shuffle_mask_sb2, 0);
__m256i shuffle_mask_sb3 = _mm256_castsi128_si256(_mm_setr_epi8(0, 1, 2, 3, 0, 1, 6, 7, 8, 9, 10, 11, 8, 9, 14, 15));
shuffle_mask_sb3 = _mm256_permute2f128_si256(shuffle_mask_sb3, shuffle_mask_sb3, 0);
__m256i shuffle_mask_sb4 = _mm256_castsi128_si256(_mm_setr_epi8(0, 1, 2, 3, 4, 5, 0, 1, 8, 9, 10, 11, 12, 13, 8, 9));
shuffle_mask_sb4 = _mm256_permute2f128_si256(shuffle_mask_sb4, shuffle_mask_sb4, 0);
for (int k = 0; k < 4; k++) {
__m256i q0 = quants_interleaved[k * 8 + 0];
__m256i q1 = quants_interleaved[k * 8 + 1];
__m256i q2 = quants_interleaved[k * 8 + 2];
__m256i q3 = quants_interleaved[k * 8 + 3];
__m256i q4 = quants_interleaved[k * 8 + 4];
__m256i q5 = quants_interleaved[k * 8 + 5];
__m256i q6 = quants_interleaved[k * 8 + 6];
__m256i q7 = quants_interleaved[k * 8 + 7];
__m256i sb2_h1_shuffled = _mm256_shuffle_epi8(q2, shuffle_mask_sb2);
__m256i sb_h1_interleaved = _mm256_blend_epi16(q0, sb2_h1_shuffled, 34);
__m256i sb3_h1_shuffled = _mm256_shuffle_epi8(q4, shuffle_mask_sb3);
sb_h1_interleaved = _mm256_blend_epi16(sb_h1_interleaved, sb3_h1_shuffled, 68);
__m256i sb4_h1_shuffled = _mm256_shuffle_epi8(q6, shuffle_mask_sb4);
sb_h1_interleaved = _mm256_blend_epi16(sb_h1_interleaved, sb4_h1_shuffled, 136);
__m256i one = _mm256_set1_epi8(1);
__m256i bsums_r1 = _mm256_maddubs_epi16(one, sb_h1_interleaved);
for (int l = 0; l < 3; l++) {
q0 = _mm256_srli_epi64(q0, 16);
q2 = _mm256_srli_epi64(q2, 16);
q4 = _mm256_srli_epi64(q4, 16);
q6 = _mm256_srli_epi64(q6, 16);
sb2_h1_shuffled = _mm256_shuffle_epi8(q2, shuffle_mask_sb2);
sb_h1_interleaved = _mm256_blend_epi16(q0, sb2_h1_shuffled, 34);
sb3_h1_shuffled = _mm256_shuffle_epi8(q4, shuffle_mask_sb3);
sb_h1_interleaved = _mm256_blend_epi16(sb_h1_interleaved, sb3_h1_shuffled, 68);
sb4_h1_shuffled = _mm256_shuffle_epi8(q6, shuffle_mask_sb4);
sb_h1_interleaved = _mm256_blend_epi16(sb_h1_interleaved, sb4_h1_shuffled, 136);
bsums_r1 = _mm256_add_epi16(bsums_r1, _mm256_maddubs_epi16(one, sb_h1_interleaved));
}
__m256i sb2_h2_shuffled = _mm256_shuffle_epi8(q3, shuffle_mask_sb2);
__m256i sb_h2_interleaved = _mm256_blend_epi16(q1, sb2_h2_shuffled, 34);
__m256i sb3_h2_shuffled = _mm256_shuffle_epi8(q5, shuffle_mask_sb3);
sb_h2_interleaved = _mm256_blend_epi16(sb_h2_interleaved, sb3_h2_shuffled, 68);
__m256i sb4_h2_shuffled = _mm256_shuffle_epi8(q7, shuffle_mask_sb4);
sb_h2_interleaved = _mm256_blend_epi16(sb_h2_interleaved, sb4_h2_shuffled, 136);
__m256i bsums_r2 = _mm256_maddubs_epi16(one, sb_h2_interleaved);
for (int l = 0; l < 3; l++) {
q1 = _mm256_srli_epi64(q1, 16);
q3 = _mm256_srli_epi64(q3, 16);
q5 = _mm256_srli_epi64(q5, 16);
q7 = _mm256_srli_epi64(q7, 16);
sb2_h2_shuffled = _mm256_shuffle_epi8(q3, shuffle_mask_sb2);
sb_h2_interleaved = _mm256_blend_epi16(q1, sb2_h2_shuffled, 34);
sb3_h2_shuffled = _mm256_shuffle_epi8(q5, shuffle_mask_sb3);
sb_h2_interleaved = _mm256_blend_epi16(sb_h2_interleaved, sb3_h2_shuffled, 68);
sb4_h2_shuffled = _mm256_shuffle_epi8(q7, shuffle_mask_sb4);
sb_h2_interleaved = _mm256_blend_epi16(sb_h2_interleaved, sb4_h2_shuffled, 136);
bsums_r2 = _mm256_add_epi16(bsums_r2, _mm256_maddubs_epi16(one, sb_h2_interleaved));
}
__m256i bsums_r = _mm256_add_epi16(bsums_r1, bsums_r2);
_mm256_storeu_si256((__m256i *)(y[i].bsums + 16 * k), bsums_r);
}
}
#else
const int blck_size_interleave = 8;
float srcv[4][QK_K];
float iscale[4];
for (int i = 0; i < nb; i++) {
for (int row_iter = 0; row_iter < 4; row_iter++) {
float amax = 0.0f;
float max = 0;
for (int j = 0; j < QK_K; j++) {
srcv[row_iter][j] = x[row_iter * k + i * QK_K + j];
if(amax < fabsf(srcv[row_iter][j])) {
amax = fabsf(srcv[row_iter][j]);
max = srcv[row_iter][j];
}
}
iscale[row_iter] = amax ? -127.f/max : 0;
y[i].d[row_iter] = amax ? 1/iscale[row_iter] : 0;
}
for (int j = 0; j < QK_K / 4; j++) {
y[i].bsums[j] = 0;
}
for (int j = 0; j < QK_K * 4; j++) {
int src_offset = (j / (4 * blck_size_interleave)) * blck_size_interleave;
int src_id     = (j % (4 * blck_size_interleave)) / blck_size_interleave;
src_offset += (j % blck_size_interleave);
int index = (((j & 31) >> 3) << 2) + ((j >> 8) << 4) + ((j >> 6) & 3);
float x0 = srcv[src_id][src_offset] * iscale[src_id];
y[i].qs[j] = nearest_int(x0);
y[i].bsums[index] += y[i].qs[j];
}
}
#endif
}
template <int64_t INTER_SIZE, ggml_type PARAM_TYPE>
void ggml_quantize_mat_t(const float * GGML_RESTRICT x, void * GGML_RESTRICT vy, int64_t nrow, int64_t n_per_row);
template <> void ggml_quantize_mat_t<4, GGML_TYPE_Q8_0>(const float * GGML_RESTRICT x, void * GGML_RESTRICT vy, int64_t nrow, int64_t n_per_row) {
assert(nrow == 4);
UNUSED(nrow);
ggml_quantize_mat_q8_0_4x4(x, vy, n_per_row);
}
template <> void ggml_quantize_mat_t<8, GGML_TYPE_Q8_0>(const float * GGML_RESTRICT x, void * GGML_RESTRICT vy, int64_t nrow, int64_t n_per_row) {
assert(nrow == 4);
UNUSED(nrow);
ggml_quantize_mat_q8_0_4x8(x, vy, n_per_row);
}
template <> void ggml_quantize_mat_t<8, GGML_TYPE_Q8_K>(const float * GGML_RESTRICT x, void * GGML_RESTRICT vy, int64_t nrow, int64_t n_per_row) {
assert(nrow == 4);
UNUSED(nrow);
ggml_quantize_mat_q8_K_4x8(x, vy, n_per_row);
}
static void ggml_gemv_q4_0_4x4_q8_0(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK8_0;
const int nb = n / qk;
const int ncols_interleaved = 4;
const int blocklen = 4;
assert (n % qk == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if ! ((defined(_MSC_VER)) && ! defined(__clang__)) && defined(__aarch64__) && defined(__ARM_NEON) && defined(__ARM_FEATURE_DOTPROD)
if (ggml_cpu_has_neon() && ggml_cpu_has_dotprod()) {
const block_q4_0x4 * b_ptr = (const block_q4_0x4 *) vx;
for (int c = 0; c < nc; c += ncols_interleaved) {
const block_q8_0 * a_ptr = (const block_q8_0 *) vy;
float32x4_t acc = vdupq_n_f32(0);
for (int b = 0; b < nb; b++) {
int8x16_t b0 = vld1q_s8((const int8_t *) b_ptr->qs);
int8x16_t b1 = vld1q_s8((const int8_t *) b_ptr->qs + 16);
int8x16_t b2 = vld1q_s8((const int8_t *) b_ptr->qs + 32);
int8x16_t b3 = vld1q_s8((const int8_t *) b_ptr->qs + 48);
float16x4_t bd = vld1_f16((const __fp16 *) b_ptr->d);
int8x16_t a0 = vld1q_s8(a_ptr->qs);
int8x16_t a1 = vld1q_s8(a_ptr->qs + qk/2);
float16x4_t ad = vld1_dup_f16((const __fp16 *) &a_ptr->d);
int32x4_t ret = vdupq_n_s32(0);
ret = vdotq_laneq_s32(ret, b0 << 4, a0, 0);
ret = vdotq_laneq_s32(ret, b1 << 4, a0, 1);
ret = vdotq_laneq_s32(ret, b2 << 4, a0, 2);
ret = vdotq_laneq_s32(ret, b3 << 4, a0, 3);
ret = vdotq_laneq_s32(ret, b0 & 0xf0U, a1, 0);
ret = vdotq_laneq_s32(ret, b1 & 0xf0U, a1, 1);
ret = vdotq_laneq_s32(ret, b2 & 0xf0U, a1, 2);
ret = vdotq_laneq_s32(ret, b3 & 0xf0U, a1, 3);
acc = vfmaq_f32(acc, vcvtq_n_f32_s32(ret, 4),
vmulq_f32(vcvt_f32_f16(ad), vcvt_f32_f16(bd)));
a_ptr++;
b_ptr++;
}
vst1q_f32(s, acc);
s += ncols_interleaved;
}
return;
}
#endif
float sumf[4];
int sumi;
const block_q8_0 * a_ptr = (const block_q8_0 *) vy;
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_0x4 * b_ptr = (const block_q4_0x4 *) vx + (x * nb);
for (int j = 0; j < ncols_interleaved; j++) sumf[j] = 0.0;
for (int l = 0; l < nb; l++) {
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] << 4);
const int v1 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0xF0);
sumi += ((v0 * a_ptr[l].qs[k * blocklen + i]) + (v1 * a_ptr[l].qs[k * blocklen + i + qk / 2])) >> 4;
}
sumf[j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * GGML_FP16_TO_FP32(a_ptr[l].d);
}
}
}
for (int j = 0; j < ncols_interleaved; j++) s[x * ncols_interleaved + j] = sumf[j];
}
}
static void ggml_gemv_q4_0_4x8_q8_0(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK8_0;
const int nb = n / qk;
const int ncols_interleaved = 4;
const int blocklen = 8;
assert (n % qk == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if ! ((defined(_MSC_VER)) && ! defined(__clang__)) && defined(__aarch64__) && defined(__ARM_NEON) && defined(__ARM_FEATURE_DOTPROD)
if (ggml_cpu_has_neon() && ggml_cpu_has_dotprod()) {
const block_q4_0x4 * b_ptr = (const block_q4_0x4 *) vx;
for (int c = 0; c < nc; c += ncols_interleaved) {
const block_q8_0 * a_ptr = (const block_q8_0 *) vy;
float32x4_t acc = vdupq_n_f32(0);
for (int b = 0; b < nb; b++) {
int8x16_t b0 = vld1q_s8((const int8_t *) b_ptr->qs);
int8x16_t b1 = vld1q_s8((const int8_t *) b_ptr->qs + 16);
int8x16_t b2 = vld1q_s8((const int8_t *) b_ptr->qs + 32);
int8x16_t b3 = vld1q_s8((const int8_t *) b_ptr->qs + 48);
float16x4_t bd = vld1_f16((const __fp16 *) b_ptr->d);
int8x16_t a0 = (int8x16_t) vld1q_dup_s64((const int64_t *) a_ptr->qs);
int8x16_t a1 = (int8x16_t) vld1q_dup_s64((const int64_t *) a_ptr->qs + 1);
int8x16_t a2 = (int8x16_t) vld1q_dup_s64((const int64_t *) a_ptr->qs + 2);
int8x16_t a3 = (int8x16_t) vld1q_dup_s64((const int64_t *) a_ptr->qs + 3);
float16x4_t ad = vld1_dup_f16((const __fp16 *) &a_ptr->d);
int32x4_t ret0 = vdupq_n_s32(0);
int32x4_t ret1 = vdupq_n_s32(0);
ret0 = vdotq_s32(ret0, b0 << 4, a0);
ret1 = vdotq_s32(ret1, b1 << 4, a0);
ret0 = vdotq_s32(ret0, b2 << 4, a1);
ret1 = vdotq_s32(ret1, b3 << 4, a1);
ret0 = vdotq_s32(ret0, b0 & 0xf0U, a2);
ret1 = vdotq_s32(ret1, b1 & 0xf0U, a2);
ret0 = vdotq_s32(ret0, b2 & 0xf0U, a3);
ret1 = vdotq_s32(ret1, b3 & 0xf0U, a3);
int32x4_t ret = vpaddq_s32(ret0, ret1);
acc = vfmaq_f32(acc, vcvtq_n_f32_s32(ret, 4),
vmulq_f32(vcvt_f32_f16(ad), vcvt_f32_f16(bd)));
a_ptr++;
b_ptr++;
}
vst1q_f32(s, acc);
s += ncols_interleaved;
}
return;
}
#endif
float sumf[4];
int sumi;
const block_q8_0 * a_ptr = (const block_q8_0 *) vy;
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_0x4 * b_ptr = (const block_q4_0x4 *) vx + (x * nb);
for (int j = 0; j < ncols_interleaved; j++) sumf[j] = 0.0;
for (int l = 0; l < nb; l++) {
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] << 4);
const int v1 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0xF0);
sumi += ((v0 * a_ptr[l].qs[k * blocklen + i]) + (v1 * a_ptr[l].qs[k * blocklen + i + qk / 2])) >> 4;
}
sumf[j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * GGML_FP16_TO_FP32(a_ptr[l].d);
}
}
}
for (int j = 0; j < ncols_interleaved; j++) s[x * ncols_interleaved + j] = sumf[j];
}
}
static void ggml_gemv_q4_0_8x8_q8_0(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK8_0;
const int nb = n / qk;
const int ncols_interleaved = 8;
const int blocklen = 8;
assert (n % qk == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if ! ((defined(_MSC_VER)) && ! defined(__clang__)) && defined(__aarch64__)
#if defined(__ARM_FEATURE_SVE)
if (ggml_cpu_has_sve() && ggml_cpu_get_sve_cnt() == QK8_0) {
const void * b_ptr = vx;
const void * a_ptr = vy;
float * res_ptr = s;
__asm__ __volatile__(
"ptrue p0.b\n"
"add %x[b_ptr], %x[b_ptr], #0x10\n"
"1:"
"add x22, %x[a_ptr], #0x2\n"
"mov z31.b, #0x0\n"
"mov x21, %x[nb]\n"
"2:"
"ld1b { z30.b }, p0/Z, [%x[b_ptr]]\n"
"ld1b { z29.b }, p0/Z, [%x[b_ptr], #1, MUL VL]\n"
"mov z28.s, #0x0\n"
"mov z27.s, #0x0\n"
"ld1rd { z26.d }, p0/Z, [x22]\n"
"ld1b { z25.b }, p0/Z, [%x[b_ptr], #2, MUL VL]\n"
"sub x20, x22, #0x2\n"
"sub x21, x21, #0x1\n"
"ld1b { z24.b }, p0/Z, [%x[b_ptr], #3, MUL VL]\n"
"ld1rd { z23.d }, p0/Z, [x22, #8]\n"
"lsl z22.b, z30.b, #0x4\n"
"lsl z16.b, z29.b, #0x4\n"
"and z30.b, z30.b, #0xf0\n"
"and z29.b, z29.b, #0xf0\n"
"ld1rd { z21.d }, p0/Z, [x22, #16]\n"
"ld1rd { z20.d }, p0/Z, [x22, #24]\n"
"lsl z19.b, z25.b, #0x4\n"
"and z25.b, z25.b, #0xf0\n"
"ld1rh { z17.h }, p0/Z, [x20]\n"
"ld1h { z18.s }, p0/Z, [%x[b_ptr], #-1, MUL VL]\n"
"sdot z28.s, z22.b, z26.b\n"
"sdot z27.s, z16.b, z26.b\n"
"lsl z16.b, z24.b, #0x4\n"
"add x22, x22, #0x22\n"
"and z24.b, z24.b, #0xf0\n"
"add %x[b_ptr], %x[b_ptr], #0x90\n"
"fcvt z17.s, p0/m, z17.h\n"
"fcvt z18.s, p0/m, z18.h\n"
"sdot z28.s, z19.b, z23.b\n"
"sdot z27.s, z16.b, z23.b\n"
"fmul z18.s, z18.s, z17.s\n"
"sdot z28.s, z30.b, z21.b\n"
"sdot z27.s, z29.b, z21.b\n"
"sdot z28.s, z25.b, z20.b\n"
"sdot z27.s, z24.b, z20.b\n"
"uzp1 z17.s, z28.s, z27.s\n"
"uzp2 z16.s, z28.s, z27.s\n"
"add z17.s, z17.s, z16.s\n"
"asr z17.s, z17.s, #0x4\n"
"scvtf z17.s, p0/m, z17.s\n"
"fmla z31.s, p0/M, z17.s, z18.s\n"
"cbnz x21, 2b\n"
"sub %x[nc], %x[nc], #0x8\n"
"st1w { z31.s }, p0, [%x[res_ptr]]\n"
"add %x[res_ptr], %x[res_ptr], #0x20\n"
"cbnz %x[nc], 1b\n"
: [b_ptr] "+&r" (b_ptr), [res_ptr] "+&r" (res_ptr), [nc] "+&r" (nc)
: [a_ptr] "r" (a_ptr), [nb] "r" (nb)
: "memory", "p0", "x20", "x21", "x22", "z16", "z17", "z18", "z19", "z20", "z21", "z22", "z23", "z24", "z25", "z26", "z27", "z28", "z29", "z30", "z31"
);
return;
}
#endif
#elif defined(__AVX2__)
__m256i signextendlut = _mm256_castsi128_si256(_mm_set_epi8(-1, -2, -3, -4, -5, -6, -7, -8, 7, 6, 5, 4, 3, 2, 1, 0));
signextendlut = _mm256_permute2f128_si256(signextendlut, signextendlut, 0);
__m128i changemask = _mm_set_epi8(15, 14, 7, 6, 13, 12, 5, 4, 11, 10, 3, 2, 9, 8, 1, 0);
__m256i finalpermutemask = _mm256_set_epi32(7, 5, 3, 1, 6, 4, 2, 0);
const __m256i m4b = _mm256_set1_epi8(0x0F);
int64_t b_nb = n / QK4_0;
const block_q4_0x8 * b_ptr_start = (const block_q4_0x8 *)vx;
const block_q8_0 * a_ptr_start = (const block_q8_0 *)vy;
for (int64_t y = 0; y < nr; y++) {
const block_q8_0 * a_ptr = a_ptr_start + (y * nb);
for (int64_t x = 0; x < nc / 8; x++) {
const block_q4_0x8 * b_ptr = b_ptr_start + (x * b_nb);
__m256 acc_row = _mm256_setzero_ps();
for (int64_t b = 0; b < nb; b++) {
const __m256i rhs_raw_vec_0123_0 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs));
const __m256i rhs_raw_vec_4567_0 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs) + 1);
const __m256i rhs_raw_vec_0123_1 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs) + 2);
const __m256i rhs_raw_vec_4567_1 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs) + 3);
const __m256i rhs_vec_0123_0 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_vec_0123_0, m4b));
const __m256i rhs_vec_4567_0 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_vec_4567_0, m4b));
const __m256i rhs_vec_0123_1 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_vec_0123_1, m4b));
const __m256i rhs_vec_4567_1 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_vec_4567_1, m4b));
const __m256i rhs_vec_0123_2 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_0123_0, 4), m4b));
const __m256i rhs_vec_4567_2 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_4567_0, 4), m4b));
const __m256i rhs_vec_0123_3 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_0123_1, 4), m4b));
const __m256i rhs_vec_4567_3 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_4567_1, 4), m4b));
const __m256 col_scale_f32 = GGML_F32Cx8_REARRANGE_LOAD(b_ptr[b].d, changemask);
const __m256 row_scale_f32 = _mm256_set1_ps(GGML_FP16_TO_FP32(a_ptr[b].d));
__m256i lhs_vec_0 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i *)a_ptr[b].qs));
__m256i lhs_vec_1 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i *)(a_ptr[b].qs + 16)));
lhs_vec_0 = _mm256_permute2f128_si256(lhs_vec_0, lhs_vec_0, 0);
lhs_vec_1 = _mm256_permute2f128_si256(lhs_vec_1, lhs_vec_1, 0);
__m256i iacc = _mm256_setzero_si256();
iacc = mul_sum_i8_pairs_acc_int32x8(iacc, _mm256_blend_epi32(rhs_vec_0123_0 ,_mm256_shuffle_epi32(rhs_vec_4567_0, 177), 170), _mm256_shuffle_epi32(lhs_vec_0, 0));
iacc = mul_sum_i8_pairs_acc_int32x8(iacc, _mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_0, 177) ,rhs_vec_4567_0, 170), _mm256_shuffle_epi32(lhs_vec_0, 85));
iacc = mul_sum_i8_pairs_acc_int32x8(iacc, _mm256_blend_epi32(rhs_vec_0123_1 ,_mm256_shuffle_epi32(rhs_vec_4567_1, 177), 170), _mm256_shuffle_epi32(lhs_vec_0, 170));
iacc = mul_sum_i8_pairs_acc_int32x8(iacc, _mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_1, 177) ,rhs_vec_4567_1, 170), _mm256_shuffle_epi32(lhs_vec_0, 255));
iacc = mul_sum_i8_pairs_acc_int32x8(iacc, _mm256_blend_epi32(rhs_vec_0123_2 ,_mm256_shuffle_epi32(rhs_vec_4567_2, 177), 170), _mm256_shuffle_epi32(lhs_vec_1, 0));
iacc = mul_sum_i8_pairs_acc_int32x8(iacc, _mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_2, 177) ,rhs_vec_4567_2, 170), _mm256_shuffle_epi32(lhs_vec_1, 85));
iacc = mul_sum_i8_pairs_acc_int32x8(iacc, _mm256_blend_epi32(rhs_vec_0123_3 ,_mm256_shuffle_epi32(rhs_vec_4567_3, 177), 170), _mm256_shuffle_epi32(lhs_vec_1, 170));
iacc = mul_sum_i8_pairs_acc_int32x8(iacc, _mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_3, 177) ,rhs_vec_4567_3, 170), _mm256_shuffle_epi32(lhs_vec_1, 255));
acc_row = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc), _mm256_mul_ps(col_scale_f32, row_scale_f32), acc_row);
}
acc_row = _mm256_permutevar8x32_ps(acc_row, finalpermutemask);
_mm256_storeu_ps(s + (y * nr + x * 8), acc_row);
}
}
return;
#elif defined(__riscv_v_intrinsic)
if (__riscv_vlenb() >= QK4_0) {
const size_t vl = QK4_0;
const block_q8_0 * a_ptr = (const block_q8_0 *) vy;
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_0x8 * b_ptr = (const block_q4_0x8 *) vx + (x * nb);
vfloat32m1_t sumf = __riscv_vfmv_v_f_f32m1(0.0, vl / 4);
for (int l = 0; l < nb; l++) {
const int64_t a0 = *(const int64_t *)&a_ptr[l].qs[0];
const int64_t a1 = *(const int64_t *)&a_ptr[l].qs[8];
const int64_t a2 = *(const int64_t *)&a_ptr[l].qs[16];
const int64_t a3 = *(const int64_t *)&a_ptr[l].qs[24];
__asm__ __volatile__("" ::: "memory");
const vint8m2_t lhs_0_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(a0, vl / 4));
const vint8m2_t lhs_1_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(a1, vl / 4));
const vint8m2_t lhs_2_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(a2, vl / 4));
const vint8m2_t lhs_3_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(a3, vl / 4));
const vint8m4_t rhs_raw_vec = __riscv_vle8_v_i8m4((const int8_t *)b_ptr[l].qs, vl * 4);
const vint8m4_t rhs_vec_lo = __riscv_vsra_vx_i8m4(__riscv_vsll_vx_i8m4(rhs_raw_vec, 4, vl * 4), 4, vl * 4);
const vint8m4_t rhs_vec_hi = __riscv_vsra_vx_i8m4(rhs_raw_vec, 4, vl * 4);
const vint8m2_t rhs_vec_lo_0 = __riscv_vget_v_i8m4_i8m2(rhs_vec_lo, 0);
const vint8m2_t rhs_vec_lo_1 = __riscv_vget_v_i8m4_i8m2(rhs_vec_lo, 1);
const vint8m2_t rhs_vec_hi_0 = __riscv_vget_v_i8m4_i8m2(rhs_vec_hi, 0);
const vint8m2_t rhs_vec_hi_1 = __riscv_vget_v_i8m4_i8m2(rhs_vec_hi, 1);
const vint16m4_t sumi_lo_0 = __riscv_vwmul_vv_i16m4(rhs_vec_lo_0, lhs_0_8, vl * 2);
const vint16m4_t sumi_lo_1 = __riscv_vwmacc_vv_i16m4(sumi_lo_0, rhs_vec_lo_1, lhs_1_8, vl * 2);
const vint16m4_t sumi_hi_0 = __riscv_vwmacc_vv_i16m4(sumi_lo_1, rhs_vec_hi_0, lhs_2_8, vl * 2);
const vint16m4_t sumi_hi_m = __riscv_vwmacc_vv_i16m4(sumi_hi_0, rhs_vec_hi_1, lhs_3_8, vl * 2);
const vuint32m4_t sumi_i32 = __riscv_vreinterpret_v_i32m4_u32m4(__riscv_vreinterpret_v_i16m4_i32m4(sumi_hi_m));
const vuint16m2_t sumi_h2_0 = __riscv_vnsrl_wx_u16m2(sumi_i32, 0, vl);
const vuint16m2_t sumi_h2_1 = __riscv_vnsrl_wx_u16m2(sumi_i32, 16, vl);
const vuint16m2_t sumi_h2 = __riscv_vadd_vv_u16m2(sumi_h2_0, sumi_h2_1, vl);
const vuint32m2_t sumi_h2_i32 = __riscv_vreinterpret_v_u16m2_u32m2(sumi_h2);
const vuint16m1_t sumi_h4_0 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 0, vl / 2);
const vuint16m1_t sumi_h4_1 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 16, vl / 2);
const vuint16m1_t sumi_h4 = __riscv_vadd_vv_u16m1(sumi_h4_0, sumi_h4_1, vl / 2);
const vuint32m1_t sumi_h4_i32 = __riscv_vreinterpret_v_u16m1_u32m1(sumi_h4);
const vint16mf2_t sumi_h8_0 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 0, vl / 4));
const vint16mf2_t sumi_h8_1 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 16, vl / 4));
const vint32m1_t sumi_h8 = __riscv_vwadd_vv_i32m1(sumi_h8_0, sumi_h8_1, vl / 4);
const vfloat32m1_t facc = __riscv_vfcvt_f_x_v_f32m1(sumi_h8, vl / 4);
const float a_scale = GGML_FP16_TO_FP32(a_ptr[l].d);
const float b_scales[8] = {
GGML_FP16_TO_FP32(b_ptr[l].d[0]),
GGML_FP16_TO_FP32(b_ptr[l].d[1]),
GGML_FP16_TO_FP32(b_ptr[l].d[2]),
GGML_FP16_TO_FP32(b_ptr[l].d[3]),
GGML_FP16_TO_FP32(b_ptr[l].d[4]),
GGML_FP16_TO_FP32(b_ptr[l].d[5]),
GGML_FP16_TO_FP32(b_ptr[l].d[6]),
GGML_FP16_TO_FP32(b_ptr[l].d[7])
};
const vfloat32m1_t b_scales_vec = __riscv_vle32_v_f32m1(b_scales, vl / 4);
const vfloat32m1_t tmp1 = __riscv_vfmul_vf_f32m1(facc, a_scale, vl / 4);
sumf = __riscv_vfmacc_vv_f32m1(sumf, tmp1, b_scales_vec, vl / 4);
}
__riscv_vse32_v_f32m1(s + x * ncols_interleaved, sumf, vl / 4);
}
return;
}
#endif
{
float sumf[8];
int sumi;
const block_q8_0 * a_ptr = (const block_q8_0 *) vy;
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_0x8 * b_ptr = (const block_q4_0x8 *) vx + (x * nb);
for (int j = 0; j < ncols_interleaved; j++) sumf[j] = 0.0;
for (int l = 0; l < nb; l++) {
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] << 4);
const int v1 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0xF0);
sumi += ((v0 * a_ptr[l].qs[k * blocklen + i]) + (v1 * a_ptr[l].qs[k * blocklen + i + qk / 2])) >> 4;
}
sumf[j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * GGML_FP16_TO_FP32(a_ptr[l].d);
}
}
}
for (int j = 0; j < ncols_interleaved; j++) s[x * ncols_interleaved + j] = sumf[j];
}
}
}
static void ggml_gemv_q4_K_8x8_q8_K(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK_K;
const int nb = n / qk;
const int ncols_interleaved = 8;
const int blocklen = 8;
static const uint32_t kmask1 = 0x3f3f3f3f;
static const uint32_t kmask2 = 0x0f0f0f0f;
static const uint32_t kmask3 = 0x03030303;
assert (n % qk == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if defined(__AVX2__)
__m256i signextendlut = _mm256_castsi128_si256(_mm_set_epi8(-1, -2, -3, -4, -5, -6, -7, -8, 7, 6, 5, 4, 3, 2, 1, 0));
signextendlut = _mm256_permute2f128_si256(signextendlut, signextendlut, 0);
__m128i deltamask = _mm_set_epi8(15, 14, 7, 6, 13, 12, 5, 4, 11, 10, 3, 2, 9, 8, 1, 0);
__m128i scalemask = _mm_set_epi8(7, 7, 3, 3, 6, 6, 2, 2, 5, 5, 1, 1, 4, 4, 0, 0);
__m256i finalpermutemask = _mm256_set_epi32(7, 5, 3, 1, 6, 4, 2, 0);
const __m256i m4b = _mm256_set1_epi8(0x0F);
int64_t b_nb = n / QK_K;
const block_q4_Kx8 * b_ptr_start = (const block_q4_Kx8 *)vx;
const block_q8_K * a_ptr_start = (const block_q8_K *)vy;
for (int64_t y = 0; y < nr; y++) {
const block_q8_K * a_ptr = a_ptr_start + (y * nb);
for (int64_t x = 0; x < nc / 8; x++) {
const block_q4_Kx8 * b_ptr = b_ptr_start + (x * b_nb);
__m256 acc_row = _mm256_setzero_ps();
__m256 acc_min_rows = _mm256_setzero_ps();
for (int64_t b = 0; b < nb; b++) {
const __m256 row_scale_f32 = _mm256_set1_ps((a_ptr[b].d));
const __m256 col_scale_f32 = GGML_F32Cx8_REARRANGE_LOAD(b_ptr[b].d, deltamask);
const __m256 col_dmin_f32 = GGML_F32Cx8_LOAD(b_ptr[b].dmin);
__m256i iacc_b = _mm256_setzero_si256();
__m256i iacc_min_b = _mm256_setzero_si256();
const __m256i q8sums = _mm256_loadu_si256((const __m256i * )(a_ptr[b].bsums));
__m256i q8s = _mm256_castsi128_si256(_mm_hadd_epi16(_mm256_castsi256_si128(q8sums), _mm256_extracti128_si256(q8sums, 1)));
q8s = _mm256_permute2f128_si256(q8s, q8s, 0);
for (int sb = 0; sb < QK_K / 64; sb++) {
const __m256i rhs_raw_vec_0123_0 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + sb * 256));
const __m256i rhs_raw_vec_4567_0 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 32 + sb * 256));
const __m256i rhs_raw_vec_0123_1 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 64 + sb * 256));
const __m256i rhs_raw_vec_4567_1 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 96 + sb * 256));
const __m256i rhs_raw_vec_0123_2 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 128 + sb * 256));
const __m256i rhs_raw_vec_4567_2 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 160 + sb * 256));
const __m256i rhs_raw_vec_0123_3 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 192 + sb * 256));
const __m256i rhs_raw_vec_4567_3 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 224 + sb * 256));
const __m256i rhs_vec_0123_00 = _mm256_and_si256(rhs_raw_vec_0123_0, m4b);
const __m256i rhs_vec_4567_00 = _mm256_and_si256(rhs_raw_vec_4567_0, m4b);
const __m256i rhs_vec_0123_01 = _mm256_and_si256(rhs_raw_vec_0123_1, m4b);
const __m256i rhs_vec_4567_01 = _mm256_and_si256(rhs_raw_vec_4567_1, m4b);
const __m256i rhs_vec_0123_02 = _mm256_and_si256(rhs_raw_vec_0123_2, m4b);
const __m256i rhs_vec_4567_02 = _mm256_and_si256(rhs_raw_vec_4567_2, m4b);
const __m256i rhs_vec_0123_03 = _mm256_and_si256(rhs_raw_vec_0123_3, m4b);
const __m256i rhs_vec_4567_03 = _mm256_and_si256(rhs_raw_vec_4567_3, m4b);
const __m256i rhs_vec_0123_10 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_0123_0, 4), m4b);
const __m256i rhs_vec_4567_10 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_4567_0, 4), m4b);
const __m256i rhs_vec_0123_11 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_0123_1, 4), m4b);
const __m256i rhs_vec_4567_11 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_4567_1, 4), m4b);
const __m256i rhs_vec_0123_12 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_0123_2, 4), m4b);
const __m256i rhs_vec_4567_12 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_4567_2, 4), m4b);
const __m256i rhs_vec_0123_13 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_0123_3, 4), m4b);
const __m256i rhs_vec_4567_13 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_vec_4567_3, 4), m4b);
uint32_t utmp_0[4], utmp_1[4];
memcpy(utmp_0, b_ptr[b].scales + 24 * sb, 12);
utmp_0[3] = ((utmp_0[2] >> 4) & kmask2) | (((utmp_0[1] >> 6) & kmask3) << 4);
const uint32_t uaux_0 = utmp_0[1] & kmask1;
utmp_0[1] = (utmp_0[2] & kmask2) | (((utmp_0[0] >> 6) & kmask3) << 4);
utmp_0[2] = uaux_0;
utmp_0[0] &= kmask1;
memcpy(utmp_1, b_ptr[b].scales + 12 + sb * 24, 12);
utmp_1[3] = ((utmp_1[2] >> 4) & kmask2) | (((utmp_1[1] >> 6) & kmask3) << 4);
const uint32_t uaux_1 = utmp_1[1] & kmask1;
utmp_1[1] = (utmp_1[2] & kmask2) | (((utmp_1[0] >> 6) & kmask3) << 4);
utmp_1[2] = uaux_1;
utmp_1[0] &= kmask1;
const __m128i mins_and_scales_0 = _mm_set_epi32(utmp_0[3], utmp_0[2], utmp_0[1], utmp_0[0]);
__m128i scales_rearrange_0 = _mm_shuffle_epi8(mins_and_scales_0, scalemask);
__m256i scales_0 = _mm256_cvtepu8_epi16(scales_rearrange_0);
__m128i mins_and_scales_1 = _mm_set_epi32(utmp_1[3], utmp_1[2], utmp_1[1], utmp_1[0]);
__m128i scales_rearrange_1 = _mm_shuffle_epi8(mins_and_scales_1, scalemask);
__m256i scales_1 = _mm256_cvtepu8_epi16(scales_rearrange_1);
__m256i mins_01 = _mm256_cvtepu8_epi16(_mm_unpacklo_epi8(_mm_shuffle_epi32(mins_and_scales_0, 78), _mm_shuffle_epi32(mins_and_scales_1, 78)));
__m256i lhs_vec_00 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i *)(a_ptr[b].qs + sb * 64)));
__m256i lhs_vec_01 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i *)(a_ptr[b].qs + 16 + sb * 64)));
__m256i lhs_vec_10 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i *)(a_ptr[b].qs + 32 + sb * 64)));
__m256i lhs_vec_11 = _mm256_castsi128_si256(_mm_loadu_si128((const __m128i *)(a_ptr[b].qs + 48 + sb * 64)));
lhs_vec_00 = _mm256_permute2f128_si256(lhs_vec_00, lhs_vec_00, 0);
lhs_vec_01 = _mm256_permute2f128_si256(lhs_vec_01, lhs_vec_01, 0);
lhs_vec_10 = _mm256_permute2f128_si256(lhs_vec_10, lhs_vec_10, 0);
lhs_vec_11 = _mm256_permute2f128_si256(lhs_vec_11, lhs_vec_11, 0);
__m256i iacc_0 = _mm256_setzero_si256();
__m256i iacc_1 = _mm256_setzero_si256();
iacc_0 = _mm256_add_epi16(iacc_0, _mm256_maddubs_epi16(_mm256_blend_epi32(rhs_vec_0123_00 ,_mm256_shuffle_epi32(rhs_vec_4567_00, 177), 170), _mm256_shuffle_epi32(lhs_vec_00, 0)));
iacc_0 = _mm256_add_epi16(iacc_0, _mm256_maddubs_epi16(_mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_00, 177) ,rhs_vec_4567_00, 170), _mm256_shuffle_epi32(lhs_vec_00, 85)));
iacc_0 = _mm256_add_epi16(iacc_0, _mm256_maddubs_epi16(_mm256_blend_epi32(rhs_vec_0123_01 ,_mm256_shuffle_epi32(rhs_vec_4567_01, 177), 170), _mm256_shuffle_epi32(lhs_vec_00, 170)));
iacc_0 = _mm256_add_epi16(iacc_0, _mm256_maddubs_epi16(_mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_01, 177) ,rhs_vec_4567_01, 170), _mm256_shuffle_epi32(lhs_vec_00, 255)));
iacc_0 = _mm256_add_epi16(iacc_0, _mm256_maddubs_epi16(_mm256_blend_epi32(rhs_vec_0123_02 ,_mm256_shuffle_epi32(rhs_vec_4567_02, 177), 170), _mm256_shuffle_epi32(lhs_vec_01, 0)));
iacc_0 = _mm256_add_epi16(iacc_0, _mm256_maddubs_epi16(_mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_02, 177) ,rhs_vec_4567_02, 170), _mm256_shuffle_epi32(lhs_vec_01, 85)));
iacc_0 = _mm256_add_epi16(iacc_0, _mm256_maddubs_epi16(_mm256_blend_epi32(rhs_vec_0123_03 ,_mm256_shuffle_epi32(rhs_vec_4567_03, 177), 170), _mm256_shuffle_epi32(lhs_vec_01, 170)));
iacc_0 = _mm256_add_epi16(iacc_0, _mm256_maddubs_epi16(_mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_03, 177) ,rhs_vec_4567_03, 170), _mm256_shuffle_epi32(lhs_vec_01, 255)));
iacc_0 = _mm256_madd_epi16(iacc_0, scales_0);
iacc_1 = _mm256_add_epi16(iacc_1, _mm256_maddubs_epi16(_mm256_blend_epi32(rhs_vec_0123_10 ,_mm256_shuffle_epi32(rhs_vec_4567_10, 177), 170), _mm256_shuffle_epi32(lhs_vec_10, 0)));
iacc_1 = _mm256_add_epi16(iacc_1, _mm256_maddubs_epi16(_mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_10, 177) ,rhs_vec_4567_10, 170), _mm256_shuffle_epi32(lhs_vec_10, 85)));
iacc_1 = _mm256_add_epi16(iacc_1, _mm256_maddubs_epi16(_mm256_blend_epi32(rhs_vec_0123_11 ,_mm256_shuffle_epi32(rhs_vec_4567_11, 177), 170), _mm256_shuffle_epi32(lhs_vec_10, 170)));
iacc_1 = _mm256_add_epi16(iacc_1, _mm256_maddubs_epi16(_mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_11, 177) ,rhs_vec_4567_11, 170), _mm256_shuffle_epi32(lhs_vec_10, 255)));
iacc_1 = _mm256_add_epi16(iacc_1, _mm256_maddubs_epi16(_mm256_blend_epi32(rhs_vec_0123_12 ,_mm256_shuffle_epi32(rhs_vec_4567_12, 177), 170), _mm256_shuffle_epi32(lhs_vec_11, 0)));
iacc_1 = _mm256_add_epi16(iacc_1, _mm256_maddubs_epi16(_mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_12, 177) ,rhs_vec_4567_12, 170), _mm256_shuffle_epi32(lhs_vec_11, 85)));
iacc_1 = _mm256_add_epi16(iacc_1, _mm256_maddubs_epi16(_mm256_blend_epi32(rhs_vec_0123_13 ,_mm256_shuffle_epi32(rhs_vec_4567_13, 177), 170), _mm256_shuffle_epi32(lhs_vec_11, 170)));
iacc_1 = _mm256_add_epi16(iacc_1, _mm256_maddubs_epi16(_mm256_blend_epi32(_mm256_shuffle_epi32(rhs_vec_0123_13, 177) ,rhs_vec_4567_13, 170), _mm256_shuffle_epi32(lhs_vec_11, 255)));
iacc_1 = _mm256_madd_epi16(iacc_1, scales_1);
__m256i iacc_sb = _mm256_add_epi32(iacc_0, iacc_1);
__m256i q8s_sb = _mm256_shuffle_epi32(q8s, 0);
__m256i iacc_min_sb = _mm256_madd_epi16(q8s_sb, mins_01);
q8s = _mm256_bsrli_epi128(q8s, 4);
iacc_b = _mm256_add_epi32(iacc_b, iacc_sb);
iacc_min_b = _mm256_add_epi32(iacc_min_b, iacc_min_sb);
}
acc_row = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_b), _mm256_mul_ps(col_scale_f32, row_scale_f32), acc_row);
acc_min_rows = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_min_b), _mm256_mul_ps(col_dmin_f32, row_scale_f32), acc_min_rows);
}
acc_row = _mm256_permutevar8x32_ps(acc_row, finalpermutemask);
_mm256_storeu_ps(s + (y * nr + x * 8), _mm256_sub_ps(acc_row, acc_min_rows));
}
}
#else
float sumf[8];
float sum_minf[8];
uint32_t utmp[32];
int sumi1;
int sumi2;
int sumi;
const block_q8_K * a_ptr = (const block_q8_K *) vy;
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_Kx8 * b_ptr = (const block_q4_Kx8 *) vx + (x * nb);
for (int j = 0; j < ncols_interleaved; j++) {
sumf[j] = 0.0;
sum_minf[j] = 0.0;
}
for (int l = 0; l < nb; l++) {
for (int sb = 0; sb < 8; sb++) {
memcpy(utmp + sb * 4, b_ptr[l].scales + sb * 12, 12);
utmp[sb * 4 + 3] = ((utmp[sb * 4 + 2] >> 4) & kmask2) | (((utmp[sb * 4 + 1] >> 6) & kmask3) << 4);
const uint32_t uaux_0 = utmp[sb * 4 + 1] & kmask1;
utmp[sb * 4 + 1] = (utmp[sb * 4 + 2] & kmask2) | (((utmp[sb * 4 + 0] >> 6) & kmask3) << 4);
utmp[sb * 4 + 2] = uaux_0;
utmp[sb * 4 + 0] &= kmask1;
}
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
uint8_t *scales_0 = (uint8_t*) utmp + (k / 4) * 32;
uint8_t *scales_1 = (uint8_t*) utmp + (k / 4) * 32 + 16;
for (int j = 0; j < ncols_interleaved; j++) {
sumi1 = 0;
sumi2 = 0;
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0xF);
const int v1 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] >> 4);
sumi1 = (v0 * a_ptr[l].qs[(k >> 2) * 64 + (k % 4) * blocklen + i]);
sumi2 = (v1 * a_ptr[l].qs[(k >> 2) * 64 + (k % 4) * blocklen + i + 32]);
sumi1 = sumi1 * scales_0[j];
sumi2 = sumi2 * scales_1[j];
sumi += sumi1 + sumi2;
}
sumf[j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * a_ptr[l].d;
}
}
for (int sb = 0; sb < 8; sb++) {
uint8_t *mins = (uint8_t*) utmp + 8 + sb * 16;
for (int j = 0; j < ncols_interleaved; j++) {
sum_minf[j] += mins[j] * (a_ptr[l].bsums[sb * 2] + a_ptr[l].bsums[sb * 2 + 1]) * GGML_FP16_TO_FP32(b_ptr[l].dmin[j]) * a_ptr[l].d;
}
}
}
for (int j = 0; j < ncols_interleaved; j++) {
s[x * ncols_interleaved + j] = sumf[j] - sum_minf[j];
}
}
#endif
}
static void ggml_gemv_iq4_nl_4x4_q8_0(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK8_0;
const int nb = n / qk;
const int ncols_interleaved = 4;
const int blocklen = 4;
assert (n % qk == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if ! ((defined(_MSC_VER)) && ! defined(__clang__)) && defined(__aarch64__) && defined(__ARM_NEON) && defined(__ARM_FEATURE_DOTPROD)
if (ggml_cpu_has_neon() && ggml_cpu_has_dotprod()) {
const int8x16_t kvalues = vld1q_s8(kvalues_iq4nl);
const block_q8_0 * a_ptr = (const block_q8_0 *) vy;
float * res_ptr = s;
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_iq4_nlx4 * b_ptr = (const block_iq4_nlx4 *) vx + (x * nb);
float32x4_t sumf = vdupq_n_f32(0);
for (int l = 0; l < nb; l++) {
uint8x16_t b_0 = vld1q_u8(b_ptr[l].qs + 0);
uint8x16_t b_1 = vld1q_u8(b_ptr[l].qs + 16);
uint8x16_t b_2 = vld1q_u8(b_ptr[l].qs + 32);
uint8x16_t b_3 = vld1q_u8(b_ptr[l].qs + 48);
int8x16_t b_0_hi = vqtbl1q_s8(kvalues, b_0 >> 4);
int8x16_t b_0_lo = vqtbl1q_s8(kvalues, b_0 & 0x0F);
int8x16_t b_1_hi = vqtbl1q_s8(kvalues, b_1 >> 4);
int8x16_t b_1_lo = vqtbl1q_s8(kvalues, b_1 & 0x0F);
int8x16_t b_2_hi = vqtbl1q_s8(kvalues, b_2 >> 4);
int8x16_t b_2_lo = vqtbl1q_s8(kvalues, b_2 & 0x0F);
int8x16_t b_3_hi = vqtbl1q_s8(kvalues, b_3 >> 4);
int8x16_t b_3_lo = vqtbl1q_s8(kvalues, b_3 & 0x0F);
int8x16_t a_0 = vld1q_s8(a_ptr[l].qs + 0);
int8x16_t a_1 = vld1q_s8(a_ptr[l].qs + 16);
int32x4_t sumi = vdupq_n_s32(0);
sumi = vdotq_laneq_s32(sumi, b_0_lo, a_0, 0);
sumi = vdotq_laneq_s32(sumi, b_0_hi, a_1, 0);
sumi = vdotq_laneq_s32(sumi, b_1_lo, a_0, 1);
sumi = vdotq_laneq_s32(sumi, b_1_hi, a_1, 1);
sumi = vdotq_laneq_s32(sumi, b_2_lo, a_0, 2);
sumi = vdotq_laneq_s32(sumi, b_2_hi, a_1, 2);
sumi = vdotq_laneq_s32(sumi, b_3_lo, a_0, 3);
sumi = vdotq_laneq_s32(sumi, b_3_hi, a_1, 3);
float32x4_t a_d = vcvt_f32_f16(vld1_dup_f16((const float16_t *)&a_ptr[l].d));
float32x4_t b_d = vcvt_f32_f16(vld1_f16((const float16_t *)b_ptr[l].d));
float32x4_t d = a_d * b_d;
sumf = vmlaq_f32(sumf, d, vcvtq_f32_s32(sumi));
}
vst1q_f32(res_ptr + x * 4, sumf);
}
return;
}
#endif
{
float sumf[4];
int sumi;
const block_q8_0 * a_ptr = (const block_q8_0 *) vy;
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_iq4_nlx4 * b_ptr = (const block_iq4_nlx4 *) vx + (x * nb);
for (int j = 0; j < ncols_interleaved; j++) sumf[j] = 0.0;
for (int l = 0; l < nb; l++) {
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = kvalues_iq4nl[b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0x0F];
const int v1 = kvalues_iq4nl[b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] >> 4];
sumi += ((v0 * a_ptr[l].qs[k * blocklen + i]) + (v1 * a_ptr[l].qs[k * blocklen + i + qk / 2]));
}
sumf[j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * GGML_FP16_TO_FP32(a_ptr[l].d);
}
}
}
for (int j = 0; j < ncols_interleaved; j++) s[x * ncols_interleaved + j] = sumf[j];
}
}
}
static void ggml_gemm_q4_0_4x4_q8_0(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK8_0;
const int nb = n / qk;
const int ncols_interleaved = 4;
const int blocklen = 4;
assert (n % qk == 0);
assert (nr % 4 == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if ! ((defined(_MSC_VER)) && ! defined(__clang__)) && defined(__aarch64__) && defined(__ARM_NEON)
if (ggml_cpu_has_neon() && ggml_cpu_has_dotprod()) {
const void * b_ptr = vx;
const void * a_ptr = vy;
float * res_ptr = s;
size_t res_stride = bs * sizeof(float);
__asm__ __volatile__(
"mov x10, %x[nr]\n"
"mov x9, #0x88\n"
"cmp x10, #0x10\n"
"mul x9, %x[nb], x9\n"
"blt 4f\n"
"1:"
"add x28, %x[b_ptr], #0x8\n"
"mov x27, %x[nc]\n"
"add x26, %x[res_ptr], %x[res_stride], LSL #4\n"
"2:"
"add x25, %x[a_ptr], #0x8\n"
"movi v15.16b, #0x0\n"
"movi v19.16b, #0x0\n"
"mov x24, %x[nb]\n"
"add x23, x25, x9\n"
"movi v18.16b, #0x0\n"
"movi v14.16b, #0x0\n"
"add x22, x23, x9\n"
"movi v11.16b, #0x0\n"
"movi v13.16b, #0x0\n"
"add x21, x22, x9\n"
"movi v23.16b, #0x0\n"
"movi v16.16b, #0x0\n"
"movi v25.16b, #0x0\n"
"movi v7.16b, #0x0\n"
"movi v0.16b, #0x0\n"
"movi v4.16b, #0x0\n"
"movi v5.16b, #0x0\n"
"movi v21.16b, #0x0\n"
"movi v8.16b, #0x0\n"
"movi v1.16b, #0x0\n"
"3:"
"ldr q3, [x28, #0x0]\n"
"ldr q31, [x25, #0x0]\n"
"movi v28.16b, #0x4\n"
"movi v10.4s, #0x0\n"
"ldr q22, [x28, #0x10]\n"
"ldr q6, [x25, #0x10]\n"
"movi v29.4s, #0x0\n"
"movi v9.4s, #0x0\n"
"ldr q27, [x28, #0x20]\n"
"ldr q30, [x28, #0x30]\n"
"movi v20.4s, #0x0\n"
"movi v24.16b, #0xf0\n"
"ldr d2, [x25, #-0x8]\n"
"ldr d26, [x23, #-0x8]\n"
"sshl v12.16b, v3.16b, v28.16b\n"
"sub x20, x28, #0x8\n"
"ldr d17, [x20, #0x0]\n"
"and v3.16b, v3.16b, v24.16b\n"
"subs x24, x24, #0x1\n"
"add x28, x28, #0x48\n"
".inst 0x4f9fe18a
".inst 0x4fbfe19d
".inst 0x4f9fe989
".inst 0x4fbfe994
"sshl v31.16b, v22.16b, v28.16b\n"
"and v22.16b, v22.16b, v24.16b\n"
"fcvtl v17.4s, v17.4h\n"
"fcvtl v2.4s, v2.4h\n"
"fcvtl v26.4s, v26.4h\n"
".inst 0x4f86e3ea
".inst 0x4fa6e3fd
".inst 0x4f86ebe9
".inst 0x4fa6ebf4
"sshl v6.16b, v27.16b, v28.16b\n"
"sshl v28.16b, v30.16b, v28.16b\n"
"and v27.16b, v27.16b, v24.16b\n"
"and v30.16b, v30.16b, v24.16b\n"
"ldr q24, [x25, #0x20]\n"
".inst 0x4f98e0ca
".inst 0x4fb8e0dd
".inst 0x4f98e8c9
".inst 0x4fb8e8d4
"ldr q24, [x25, #0x30]\n"
".inst 0x4f98e38a
".inst 0x4fb8e39d
".inst 0x4f98eb89
".inst 0x4fb8eb94
"ldr q24, [x25, #0x40]\n"
".inst 0x4f98e06a
".inst 0x4fb8e07d
".inst 0x4f98e869
".inst 0x4fb8e874
"ldr q24, [x25, #0x50]\n"
".inst 0x4f98e2ca
".inst 0x4fb8e2dd
".inst 0x4f98eac9
".inst 0x4fb8ead4
"ldr q24, [x25, #0x60]\n"
".inst 0x4f98e36a
".inst 0x4fb8e37d
".inst 0x4f98eb69
".inst 0x4fb8eb74
"ldr q24, [x25, #0x70]\n"
"add x25, x25, #0x88\n"
".inst 0x4f98e3ca
".inst 0x4fb8e3dd
".inst 0x4f98ebc9
".inst 0x4fb8ebd4
"fmul v24.4s, v17.4s, v2.s[0]\n"
"scvtf v10.4s, v10.4s, #0x4\n"
"scvtf v29.4s, v29.4s, #0x4\n"
"scvtf v9.4s, v9.4s, #0x4\n"
"scvtf v20.4s, v20.4s, #0x4\n"
"fmla v15.4s, v10.4s, v24.4s\n"
"ldr q24, [x23, #0x0]\n"
"fmul v10.4s, v17.4s, v2.s[1]\n"
"fmla v19.4s, v29.4s, v10.4s\n"
"ldr q10, [x23, #0x10]\n"
"fmul v29.4s, v17.4s, v2.s[2]\n"
"fmul v2.4s, v17.4s, v2.s[3]\n"
"fmla v18.4s, v9.4s, v29.4s\n"
"movi v9.4s, #0x0\n"
"movi v29.4s, #0x0\n"
".inst 0x4f98e189
".inst 0x4fb8e19d
"fmla v14.4s, v20.4s, v2.4s\n"
"movi v20.4s, #0x0\n"
"movi v2.4s, #0x0\n"
".inst 0x4f98e994
".inst 0x4fb8e982
"ldr q24, [x23, #0x20]\n"
".inst 0x4f8ae3e9
".inst 0x4faae3fd
".inst 0x4f8aebf4
".inst 0x4faaebe2
"ldr q10, [x23, #0x30]\n"
".inst 0x4f98e0c9
".inst 0x4fb8e0dd
".inst 0x4f98e8d4
".inst 0x4fb8e8c2
"ldr q24, [x23, #0x40]\n"
".inst 0x4f8ae389
".inst 0x4faae39d
".inst 0x4f8aeb94
".inst 0x4faaeb82
"ldr q10, [x23, #0x50]\n"
".inst 0x4f98e069
".inst 0x4fb8e07d
".inst 0x4f98e874
".inst 0x4fb8e862
"ldr q24, [x23, #0x60]\n"
".inst 0x4f8ae2c9
".inst 0x4faae2dd
".inst 0x4f8aead4
".inst 0x4faaeac2
"ldr q10, [x23, #0x70]\n"
"add x23, x23, #0x88\n"
".inst 0x4f98e369
".inst 0x4fb8e37d
".inst 0x4f98eb74
".inst 0x4fb8eb62
"ldr q24, [x22, #0x0]\n"
".inst 0x4f8ae3c9
".inst 0x4faae3dd
".inst 0x4f8aebd4
".inst 0x4faaebc2
"fmul v10.4s, v17.4s, v26.s[0]\n"
"scvtf v9.4s, v9.4s, #0x4\n"
"scvtf v29.4s, v29.4s, #0x4\n"
"scvtf v20.4s, v20.4s, #0x4\n"
"scvtf v2.4s, v2.4s, #0x4\n"
"fmla v11.4s, v9.4s, v10.4s\n"
"ldr q9, [x22, #0x10]\n"
"fmul v10.4s, v17.4s, v26.s[1]\n"
"fmla v13.4s, v29.4s, v10.4s\n"
"ldr d29, [x22, #-0x8]\n"
"fmul v10.4s, v17.4s, v26.s[2]\n"
"fmul v26.4s, v17.4s, v26.s[3]\n"
"fcvtl v29.4s, v29.4h\n"
"fmla v23.4s, v20.4s, v10.4s\n"
"movi v20.4s, #0x0\n"
"movi v10.4s, #0x0\n"
"fmla v16.4s, v2.4s, v26.4s\n"
"movi v26.4s, #0x0\n"
"movi v2.4s, #0x0\n"
".inst 0x4f98e194
".inst 0x4fb8e18a
".inst 0x4f98e99a
".inst 0x4fb8e982
"ldr q24, [x22, #0x20]\n"
".inst 0x4f89e3f4
".inst 0x4fa9e3ea
".inst 0x4f89ebfa
".inst 0x4fa9ebe2
"ldr q9, [x22, #0x30]\n"
".inst 0x4f98e0d4
".inst 0x4fb8e0ca
".inst 0x4f98e8da
".inst 0x4fb8e8c2
"ldr q24, [x22, #0x40]\n"
".inst 0x4f89e394
".inst 0x4fa9e38a
".inst 0x4f89eb9a
".inst 0x4fa9eb82
"ldr q9, [x22, #0x50]\n"
".inst 0x4f98e074
".inst 0x4fb8e06a
".inst 0x4f98e87a
".inst 0x4fb8e862
"ldr q24, [x22, #0x60]\n"
".inst 0x4f89e2d4
".inst 0x4fa9e2ca
".inst 0x4f89eada
".inst 0x4fa9eac2
"ldr q9, [x22, #0x70]\n"
"add x22, x22, #0x88\n"
".inst 0x4f98e374
".inst 0x4fb8e36a
".inst 0x4f98eb7a
".inst 0x4fb8eb62
"ldr q24, [x21, #0x0]\n"
".inst 0x4f89e3d4
".inst 0x4fa9e3ca
".inst 0x4f89ebda
".inst 0x4fa9ebc2
"fmul v9.4s, v17.4s, v29.s[0]\n"
"scvtf v20.4s, v20.4s, #0x4\n"
"scvtf v10.4s, v10.4s, #0x4\n"
"scvtf v26.4s, v26.4s, #0x4\n"
"scvtf v2.4s, v2.4s, #0x4\n"
"fmla v25.4s, v20.4s, v9.4s\n"
"ldr q9, [x21, #0x10]\n"
"fmul v20.4s, v17.4s, v29.s[1]\n"
"fmla v7.4s, v10.4s, v20.4s\n"
"ldr d20, [x21, #-0x8]\n"
"fmul v10.4s, v17.4s, v29.s[2]\n"
"fmul v29.4s, v17.4s, v29.s[3]\n"
"fcvtl v20.4s, v20.4h\n"
"fmla v0.4s, v26.4s, v10.4s\n"
"movi v26.4s, #0x0\n"
"movi v10.4s, #0x0\n"
"fmla v4.4s, v2.4s, v29.4s\n"
"movi v2.4s, #0x0\n"
"movi v29.4s, #0x0\n"
".inst 0x4f98e19a
".inst 0x4fb8e18a
".inst 0x4f98e982
".inst 0x4fb8e99d
"ldr q12, [x21, #0x20]\n"
"fmul v24.4s, v17.4s, v20.s[0]\n"
".inst 0x4f89e3fa
".inst 0x4fa9e3ea
".inst 0x4f89ebe2
".inst 0x4fa9ebfd
"ldr q9, [x21, #0x30]\n"
"fmul v31.4s, v17.4s, v20.s[1]\n"
".inst 0x4f8ce0da
".inst 0x4face0ca
".inst 0x4f8ce8c2
".inst 0x4face8dd
"ldr q12, [x21, #0x40]\n"
"fmul v6.4s, v17.4s, v20.s[2]\n"
"fmul v20.4s, v17.4s, v20.s[3]\n"
".inst 0x4f89e39a
".inst 0x4fa9e38a
".inst 0x4f89eb82
".inst 0x4fa9eb9d
"ldr q9, [x21, #0x50]\n"
".inst 0x4f8ce07a
".inst 0x4face06a
".inst 0x4f8ce862
".inst 0x4face87d
"ldr q12, [x21, #0x60]\n"
".inst 0x4f89e2da
".inst 0x4fa9e2ca
".inst 0x4f89eac2
".inst 0x4fa9eadd
"ldr q17, [x21, #0x70]\n"
"add x21, x21, #0x88\n"
".inst 0x4f8ce37a
".inst 0x4face36a
".inst 0x4f8ceb62
".inst 0x4faceb7d
".inst 0x4f91e3da
".inst 0x4fb1e3ca
".inst 0x4f91ebc2
".inst 0x4fb1ebdd
"scvtf v26.4s, v26.4s, #0x4\n"
"scvtf v10.4s, v10.4s, #0x4\n"
"fmla v5.4s, v26.4s, v24.4s\n"
"scvtf v2.4s, v2.4s, #0x4\n"
"scvtf v29.4s, v29.4s, #0x4\n"
"fmla v21.4s, v10.4s, v31.4s\n"
"fmla v8.4s, v2.4s, v6.4s\n"
"fmla v1.4s, v29.4s, v20.4s\n"
"bgt 3b\n"
"mov x20, %x[res_ptr]\n"
"subs x27, x27, #0x4\n"
"add %x[res_ptr], %x[res_ptr], #0x10\n"
"str q15, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q19, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q18, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q14, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q11, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q13, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q23, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q16, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q25, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q7, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q0, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q4, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q5, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q21, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q8, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q1, [x20, #0x0]\n"
"bne 2b\n"
"mov x20, #0x4\n"
"sub x10, x10, #0x10\n"
"cmp x10, #0x10\n"
"mov %x[res_ptr], x26\n"
"madd %x[a_ptr], x20, x9, %x[a_ptr]\n"
"bge 1b\n"
"4:"
"cbz x10, 9f\n"
"5:"
"add x24, %x[b_ptr], #0x8\n"
"mov x23, %x[nc]\n"
"add x22, %x[res_ptr], %x[res_stride], LSL #2\n"
"6:"
"movi v15.16b, #0x0\n"
"movi v19.16b, #0x0\n"
"add x25, %x[a_ptr], #0x8\n"
"mov x21, %x[nb]\n"
"movi v18.16b, #0x0\n"
"movi v14.16b, #0x0\n"
"7:"
"ldr q7, [x24, #0x0]\n"
"ldr q5, [x25, #0x0]\n"
"movi v9.16b, #0x4\n"
"movi v4.4s, #0x0\n"
"ldr q3, [x24, #0x10]\n"
"ldr q2, [x25, #0x10]\n"
"movi v1.4s, #0x0\n"
"movi v0.4s, #0x0\n"
"ldr q13, [x24, #0x20]\n"
"ldr q31, [x25, #0x20]\n"
"movi v30.4s, #0x0\n"
"movi v29.16b, #0xf0\n"
"ldr q28, [x24, #0x30]\n"
"ldr q27, [x25, #0x30]\n"
"sshl v20.16b, v7.16b, v9.16b\n"
"sub x20, x24, #0x8\n"
"ldr q26, [x25, #0x40]\n"
"ldr q25, [x25, #0x50]\n"
"sshl v17.16b, v3.16b, v9.16b\n"
"and v7.16b, v7.16b, v29.16b\n"
"ldr q24, [x25, #0x60]\n"
"ldr q16, [x25, #0x70]\n"
"sshl v22.16b, v13.16b, v9.16b\n"
"and v3.16b, v3.16b, v29.16b\n"
"ldr d21, [x20, #0x0]\n"
"ldr d12, [x25, #-0x8]\n"
".inst 0x4f85e284
".inst 0x4fa5e281
".inst 0x4f85ea80
".inst 0x4fa5ea9e
"sshl v9.16b, v28.16b, v9.16b\n"
"subs x21, x21, #0x1\n"
"and v13.16b, v13.16b, v29.16b\n"
"and v28.16b, v28.16b, v29.16b\n"
"add x25, x25, #0x88\n"
"add x24, x24, #0x48\n"
"fcvtl v21.4s, v21.4h\n"
"fcvtl v12.4s, v12.4h\n"
".inst 0x4f82e224
".inst 0x4fa2e221
".inst 0x4f82ea20
".inst 0x4fa2ea3e
"fmul v11.4s, v21.4s, v12.s[0]\n"
"fmul v23.4s, v21.4s, v12.s[1]\n"
"fmul v17.4s, v21.4s, v12.s[2]\n"
".inst 0x4f9fe2c4
"fmul v6.4s, v21.4s, v12.s[3]\n"
".inst 0x4fbfe2c1
".inst 0x4f9feac0
".inst 0x4fbfeade
".inst 0x4f9be124
".inst 0x4fbbe121
".inst 0x4f9be920
".inst 0x4fbbe93e
".inst 0x4f9ae0e4
".inst 0x4fbae0e1
".inst 0x4f9ae8e0
".inst 0x4fbae8fe
".inst 0x4f99e064
".inst 0x4fb9e061
".inst 0x4f99e860
".inst 0x4fb9e87e
".inst 0x4f98e1a4
".inst 0x4fb8e1a1
".inst 0x4f98e9a0
".inst 0x4fb8e9be
".inst 0x4f90e384
".inst 0x4fb0e381
".inst 0x4f90eb80
".inst 0x4fb0eb9e
"scvtf v4.4s, v4.4s, #0x4\n"
"scvtf v1.4s, v1.4s, #0x4\n"
"scvtf v0.4s, v0.4s, #0x4\n"
"fmla v15.4s, v4.4s, v11.4s\n"
"scvtf v30.4s, v30.4s, #0x4\n"
"fmla v19.4s, v1.4s, v23.4s\n"
"fmla v18.4s, v0.4s, v17.4s\n"
"fmla v14.4s, v30.4s, v6.4s\n"
"bgt 7b\n"
"mov x20, %x[res_ptr]\n"
"cmp x10, #0x1\n"
"str q15, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"cmp x10, #0x2\n"
"str q19, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"cmp x10, #0x3\n"
"str q18, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"str q14, [x20, #0x0]\n"
"8:"
"subs x23, x23, #0x4\n"
"add %x[res_ptr], %x[res_ptr], #0x10\n"
"bne 6b\n"
"subs x10, x10, #0x4\n"
"add %x[a_ptr], %x[a_ptr], x9\n"
"mov %x[res_ptr], x22\n"
"bgt 5b\n"
"9:"
: [a_ptr] "+&r" (a_ptr), [res_ptr] "+&r" (res_ptr)
: [b_ptr] "r" (b_ptr), [nr] "r" (nr), [nb] "r" (nb), [res_stride] "r" (res_stride), [nc] "r" (nc)
: "cc", "memory", "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31", "x9", "x10", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28"
);
return;
}
#endif
{
float sumf[4][4];
int sumi;
for (int y = 0; y < nr / 4; y++) {
const block_q8_0x4 * a_ptr = (const block_q8_0x4 *) vy + (y * nb);
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_0x4 * b_ptr = (const block_q4_0x4 *) vx + (x * nb);
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) sumf[m][j] = 0.0;
}
for (int l = 0; l < nb; l++) {
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] << 4);
const int v1 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0xF0);
sumi += ((v0 * a_ptr[l].qs[k * 4 * blocklen + m * blocklen + i]) +
(v1 * a_ptr[l].qs[k * 4 * blocklen + m * blocklen + i + qk / 2 * 4])) >> 4;
}
sumf[m][j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * GGML_FP16_TO_FP32(a_ptr[l].d[m]);
}
}
}
}
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++)
s[(y * 4 + m) * bs + x * ncols_interleaved + j] = sumf[m][j];
}
}
}
}
}
static void ggml_gemm_q4_0_4x8_q8_0(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK8_0;
const int nb = n / qk;
const int ncols_interleaved = 4;
const int blocklen = 8;
assert (n % qk == 0);
assert (nr % 4 == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if ! ((defined(_MSC_VER)) && ! defined(__clang__)) && defined(__aarch64__) && defined(__ARM_NEON) && defined(__ARM_FEATURE_MATMUL_INT8)
if (ggml_cpu_has_neon() && ggml_cpu_has_matmul_int8()) {
const void * b_ptr = vx;
const void * a_ptr = vy;
float * res_ptr = s;
size_t res_stride = bs * sizeof(float);
__asm__ __volatile__(
"mov x10, %x[nr]\n"
"mov x9, #0x88\n"
"cmp x10, #0x10\n"
"mul x9, %x[nb], x9\n"
"blt 4f\n"
"1:"
"add x28, %x[b_ptr], #0x8\n"
"mov x27, %x[nc]\n"
"add x26, %x[res_ptr], %x[res_stride], LSL #4\n"
"2:"
"add x25, %x[a_ptr], #0x8\n"
"movi v2.16b, #0x0\n"
"movi v10.16b, #0x0\n"
"mov x24, %x[nb]\n"
"add x23, x25, x9\n"
"movi v12.16b, #0x0\n"
"movi v28.16b, #0x0\n"
"add x22, x23, x9\n"
"movi v11.16b, #0x0\n"
"movi v13.16b, #0x0\n"
"add x21, x22, x9\n"
"movi v22.16b, #0x0\n"
"movi v23.16b, #0x0\n"
"movi v25.16b, #0x0\n"
"movi v5.16b, #0x0\n"
"movi v7.16b, #0x0\n"
"movi v4.16b, #0x0\n"
"movi v6.16b, #0x0\n"
"movi v30.16b, #0x0\n"
"movi v24.16b, #0x0\n"
"movi v14.16b, #0x0\n"
"3:"
"ldr q21, [x28, #0x0]\n"
"ldr q16, [x28, #0x10]\n"
"movi v1.16b, #0x4\n"
"movi v19.4s, #0x0\n"
"ldr q27, [x25, #0x0]\n"
"ldr q15, [x25, #0x10]\n"
"movi v26.4s, #0x0\n"
"movi v18.4s, #0x0\n"
"ldr q29, [x28, #0x20]\n"
"ldr q3, [x28, #0x30]\n"
"movi v17.4s, #0x0\n"
"movi v0.16b, #0xf0\n"
"ldr d20, [x25, #-0x8]\n"
"ldr d9, [x23, #-0x8]\n"
"sshl v8.16b, v21.16b, v1.16b\n"
"sshl v31.16b, v16.16b, v1.16b\n"
"and v21.16b, v21.16b, v0.16b\n"
"and v16.16b, v16.16b, v0.16b\n"
"sub x20, x28, #0x8\n"
"subs x24, x24, #0x1\n"
"add x28, x28, #0x48\n"
".inst 0x4e88a773
".inst 0x4e9fa77a
"ldr q27, [x25, #0x20]\n"
".inst 0x4e88a5f2
".inst 0x4e9fa5f1
"sshl v15.16b, v29.16b, v1.16b\n"
"sshl v1.16b, v3.16b, v1.16b\n"
"and v29.16b, v29.16b, v0.16b\n"
"and v3.16b, v3.16b, v0.16b\n"
"ldr q0, [x25, #0x30]\n"
"fcvtl v20.4s, v20.4h\n"
".inst 0x4e8fa773
"fcvtl v9.4s, v9.4h\n"
".inst 0x4e81a77a
"ldr q27, [x25, #0x40]\n"
".inst 0x4e8fa412
".inst 0x4e81a411
"ldr q0, [x25, #0x50]\n"
".inst 0x4e95a773
".inst 0x4e90a77a
"ldr q27, [x25, #0x60]\n"
".inst 0x4e95a412
".inst 0x4e90a411
"ldr q0, [x25, #0x70]\n"
"add x25, x25, #0x88\n"
".inst 0x4e9da773
".inst 0x4e83a77a
"ldr d27, [x20, #0x0]\n"
".inst 0x4e9da412
".inst 0x4e83a411
"fcvtl v27.4s, v27.4h\n"
"uzp1 v0.2d, v19.2d, v26.2d\n"
"uzp2 v26.2d, v19.2d, v26.2d\n"
"fmul v19.4s, v27.4s, v20.s[0]\n"
"scvtf v0.4s, v0.4s, #0x4\n"
"scvtf v26.4s, v26.4s, #0x4\n"
"fmla v2.4s, v0.4s, v19.4s\n"
"ldr q19, [x23, #0x0]\n"
"uzp1 v0.2d, v18.2d, v17.2d\n"
"uzp2 v18.2d, v18.2d, v17.2d\n"
"fmul v17.4s, v27.4s, v20.s[1]\n"
"scvtf v0.4s, v0.4s, #0x4\n"
"scvtf v18.4s, v18.4s, #0x4\n"
"fmla v10.4s, v26.4s, v17.4s\n"
"ldr q17, [x23, #0x10]\n"
"fmul v26.4s, v27.4s, v20.s[2]\n"
"fmul v20.4s, v27.4s, v20.s[3]\n"
"fmla v12.4s, v0.4s, v26.4s\n"
"ldr d0, [x22, #-0x8]\n"
"ldr d26, [x21, #-0x8]\n"
"fcvtl v0.4s, v0.4h\n"
"fmla v28.4s, v18.4s, v20.4s\n"
"movi v20.4s, #0x0\n"
"movi v18.4s, #0x0\n"
".inst 0x4e88a674
".inst 0x4e9fa672
"ldr q19, [x23, #0x20]\n"
"fcvtl v26.4s, v26.4h\n"
".inst 0x4e8fa674
".inst 0x4e81a672
"ldr q19, [x23, #0x40]\n"
".inst 0x4e95a674
".inst 0x4e90a672
"ldr q19, [x23, #0x60]\n"
".inst 0x4e9da674
".inst 0x4e83a672
"uzp1 v19.2d, v20.2d, v18.2d\n"
"scvtf v19.4s, v19.4s, #0x4\n"
"uzp2 v20.2d, v20.2d, v18.2d\n"
"fmul v18.4s, v27.4s, v9.s[0]\n"
"scvtf v20.4s, v20.4s, #0x4\n"
"fmla v11.4s, v19.4s, v18.4s\n"
"ldr q18, [x22, #0x0]\n"
"fmul v19.4s, v27.4s, v9.s[1]\n"
"fmla v13.4s, v20.4s, v19.4s\n"
"movi v19.4s, #0x0\n"
"movi v20.4s, #0x0\n"
".inst 0x4e88a633
".inst 0x4e9fa634
"ldr q17, [x23, #0x30]\n"
".inst 0x4e8fa633
".inst 0x4e81a634
"ldr q17, [x23, #0x50]\n"
".inst 0x4e95a633
".inst 0x4e90a634
"ldr q17, [x23, #0x70]\n"
"add x23, x23, #0x88\n"
".inst 0x4e9da633
".inst 0x4e83a634
"uzp1 v17.2d, v19.2d, v20.2d\n"
"scvtf v17.4s, v17.4s, #0x4\n"
"uzp2 v20.2d, v19.2d, v20.2d\n"
"fmul v19.4s, v27.4s, v9.s[2]\n"
"fmul v9.4s, v27.4s, v9.s[3]\n"
"scvtf v20.4s, v20.4s, #0x4\n"
"fmla v22.4s, v17.4s, v19.4s\n"
"ldr q17, [x22, #0x10]\n"
"movi v19.4s, #0x0\n"
".inst 0x4e88a653
"fmla v23.4s, v20.4s, v9.4s\n"
"movi v20.4s, #0x0\n"
"movi v9.4s, #0x0\n"
".inst 0x4e9fa654
"ldr q18, [x22, #0x20]\n"
".inst 0x4e88a629
".inst 0x4e8fa653
".inst 0x4e81a654
"ldr q18, [x22, #0x40]\n"
".inst 0x4e95a653
".inst 0x4e90a654
"ldr q18, [x22, #0x60]\n"
".inst 0x4e9da653
".inst 0x4e83a654
"movi v18.4s, #0x0\n"
".inst 0x4e9fa632
"ldr q17, [x22, #0x30]\n"
".inst 0x4e8fa629
".inst 0x4e81a632
"ldr q17, [x22, #0x50]\n"
".inst 0x4e95a629
".inst 0x4e90a632
"ldr q17, [x22, #0x70]\n"
"add x22, x22, #0x88\n"
".inst 0x4e9da629
".inst 0x4e83a632
"uzp1 v17.2d, v19.2d, v20.2d\n"
"uzp2 v20.2d, v19.2d, v20.2d\n"
"fmul v19.4s, v27.4s, v0.s[0]\n"
"scvtf v17.4s, v17.4s, #0x4\n"
"scvtf v20.4s, v20.4s, #0x4\n"
"fmla v25.4s, v17.4s, v19.4s\n"
"ldr q19, [x21, #0x0]\n"
"fmul v17.4s, v27.4s, v0.s[1]\n"
"fmla v5.4s, v20.4s, v17.4s\n"
"ldr q17, [x21, #0x10]\n"
"uzp1 v20.2d, v9.2d, v18.2d\n"
"uzp2 v9.2d, v9.2d, v18.2d\n"
"fmul v18.4s, v27.4s, v0.s[2]\n"
"fmul v0.4s, v27.4s, v0.s[3]\n"
"scvtf v20.4s, v20.4s, #0x4\n"
"scvtf v9.4s, v9.4s, #0x4\n"
"fmla v7.4s, v20.4s, v18.4s\n"
"movi v20.4s, #0x0\n"
"movi v18.4s, #0x0\n"
".inst 0x4e88a674
".inst 0x4e9fa672
"ldr q19, [x21, #0x20]\n"
"fmla v4.4s, v9.4s, v0.4s\n"
"movi v9.4s, #0x0\n"
"movi v0.4s, #0x0\n"
".inst 0x4e88a629
"fmul v8.4s, v27.4s, v26.s[0]\n"
".inst 0x4e9fa620
"ldr q17, [x21, #0x30]\n"
".inst 0x4e8fa674
"fmul v31.4s, v27.4s, v26.s[1]\n"
".inst 0x4e81a672
"ldr q19, [x21, #0x40]\n"
".inst 0x4e8fa629
"fmul v15.4s, v27.4s, v26.s[2]\n"
"fmul v27.4s, v27.4s, v26.s[3]\n"
".inst 0x4e81a620
"ldr q1, [x21, #0x50]\n"
".inst 0x4e95a674
".inst 0x4e90a672
"ldr q26, [x21, #0x60]\n"
".inst 0x4e95a429
".inst 0x4e90a420
"ldr q21, [x21, #0x70]\n"
"add x21, x21, #0x88\n"
".inst 0x4e9da754
".inst 0x4e83a752
".inst 0x4e9da6a9
".inst 0x4e83a6a0
"uzp1 v29.2d, v20.2d, v18.2d\n"
"uzp2 v21.2d, v20.2d, v18.2d\n"
"scvtf v29.4s, v29.4s, #0x4\n"
"uzp1 v18.2d, v9.2d, v0.2d\n"
"uzp2 v16.2d, v9.2d, v0.2d\n"
"scvtf v21.4s, v21.4s, #0x4\n"
"fmla v6.4s, v29.4s, v8.4s\n"
"scvtf v18.4s, v18.4s, #0x4\n"
"scvtf v16.4s, v16.4s, #0x4\n"
"fmla v30.4s, v21.4s, v31.4s\n"
"fmla v24.4s, v18.4s, v15.4s\n"
"fmla v14.4s, v16.4s, v27.4s\n"
"bgt 3b\n"
"mov x20, %x[res_ptr]\n"
"subs x27, x27, #0x4\n"
"add %x[res_ptr], %x[res_ptr], #0x10\n"
"str q2, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q10, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q12, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q28, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q11, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q13, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q22, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q23, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q25, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q5, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q7, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q4, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q6, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q30, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q24, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"str q14, [x20, #0x0]\n"
"bne 2b\n"
"mov x20, #0x4\n"
"sub x10, x10, #0x10\n"
"cmp x10, #0x10\n"
"mov %x[res_ptr], x26\n"
"madd %x[a_ptr], x20, x9, %x[a_ptr]\n"
"bge 1b\n"
"4:"
"cbz x10, 9f\n"
"5:"
"add x24, %x[b_ptr], #0x8\n"
"mov x23, %x[nc]\n"
"add x22, %x[res_ptr], %x[res_stride], LSL #2\n"
"6:"
"movi v2.16b, #0x0\n"
"movi v10.16b, #0x0\n"
"add x25, %x[a_ptr], #0x8\n"
"mov x21, %x[nb]\n"
"movi v12.16b, #0x0\n"
"movi v28.16b, #0x0\n"
"7:"
"ldr q6, [x24, #0x0]\n"
"ldr q5, [x24, #0x10]\n"
"movi v17.16b, #0x4\n"
"movi v8.4s, #0x0\n"
"ldr q4, [x25, #0x0]\n"
"ldr q13, [x25, #0x10]\n"
"movi v27.4s, #0x0\n"
"movi v0.4s, #0x0\n"
"ldr q31, [x24, #0x20]\n"
"ldr q14, [x24, #0x30]\n"
"movi v29.4s, #0x0\n"
"movi v22.16b, #0xf0\n"
"ldr q11, [x25, #0x20]\n"
"ldr q23, [x25, #0x30]\n"
"sshl v21.16b, v6.16b, v17.16b\n"
"sshl v16.16b, v5.16b, v17.16b\n"
"ldr q20, [x25, #0x40]\n"
"ldr q26, [x25, #0x50]\n"
"and v6.16b, v6.16b, v22.16b\n"
"and v5.16b, v5.16b, v22.16b\n"
"ldr q25, [x25, #0x60]\n"
"ldr q3, [x25, #0x70]\n"
"sshl v19.16b, v31.16b, v17.16b\n"
"sshl v18.16b, v14.16b, v17.16b\n"
"ldr d17, [x25, #-0x8]\n"
".inst 0x4e95a488
".inst 0x4e90a49b
"and v31.16b, v31.16b, v22.16b\n"
".inst 0x4e95a5a0
".inst 0x4e90a5bd
"and v14.16b, v14.16b, v22.16b\n"
"sub x20, x24, #0x8\n"
"ldr d16, [x20, #0x0]\n"
"subs x21, x21, #0x1\n"
"add x25, x25, #0x88\n"
"fcvtl v17.4s, v17.4h\n"
"add x24, x24, #0x48\n"
".inst 0x4e93a568
".inst 0x4e92a57b
".inst 0x4e93a6e0
".inst 0x4e92a6fd
"fcvtl v16.4s, v16.4h\n"
".inst 0x4e86a688
".inst 0x4e85a69b
"fmul v23.4s, v16.4s, v17.s[0]\n"
"fmul v21.4s, v16.4s, v17.s[1]\n"
"fmul v1.4s, v16.4s, v17.s[2]\n"
"fmul v20.4s, v16.4s, v17.s[3]\n"
".inst 0x4e86a740
".inst 0x4e85a75d
".inst 0x4e9fa728
".inst 0x4e8ea73b
".inst 0x4e9fa460
".inst 0x4e8ea47d
"uzp1 v19.2d, v8.2d, v27.2d\n"
"uzp2 v18.2d, v8.2d, v27.2d\n"
"scvtf v19.4s, v19.4s, #0x4\n"
"uzp1 v17.2d, v0.2d, v29.2d\n"
"uzp2 v16.2d, v0.2d, v29.2d\n"
"scvtf v18.4s, v18.4s, #0x4\n"
"fmla v2.4s, v19.4s, v23.4s\n"
"scvtf v17.4s, v17.4s, #0x4\n"
"scvtf v16.4s, v16.4s, #0x4\n"
"fmla v10.4s, v18.4s, v21.4s\n"
"fmla v12.4s, v17.4s, v1.4s\n"
"fmla v28.4s, v16.4s, v20.4s\n"
"bgt 7b\n"
"mov x20, %x[res_ptr]\n"
"cmp x10, #0x1\n"
"str q2, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"cmp x10, #0x2\n"
"str q10, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"cmp x10, #0x3\n"
"str q12, [x20, #0x0]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"str q28, [x20, #0x0]\n"
"8:"
"subs x23, x23, #0x4\n"
"add %x[res_ptr], %x[res_ptr], #0x10\n"
"bne 6b\n"
"subs x10, x10, #0x4\n"
"add %x[a_ptr], %x[a_ptr], x9\n"
"mov %x[res_ptr], x22\n"
"bgt 5b\n"
"9:"
: [a_ptr] "+&r" (a_ptr), [res_ptr] "+&r" (res_ptr)
: [b_ptr] "r" (b_ptr), [nr] "r" (nr), [nb] "r" (nb), [res_stride] "r" (res_stride), [nc] "r" (nc)
: "cc", "memory", "v0", "v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13", "v14", "v15", "v16", "v17", "v18", "v19", "v20", "v21", "v22", "v23", "v24", "v25", "v26", "v27", "v28", "v29", "v30", "v31", "x9", "x10", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28"
);
return;
}
#endif
float sumf[4][4];
int sumi;
for (int y = 0; y < nr / 4; y++) {
const block_q8_0x4 * a_ptr = (const block_q8_0x4 *) vy + (y * nb);
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_0x4 * b_ptr = (const block_q4_0x4 *) vx + (x * nb);
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) sumf[m][j] = 0.0;
}
for (int l = 0; l < nb; l++) {
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] << 4);
const int v1 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0xF0);
sumi += ((v0 * a_ptr[l].qs[k * 4 * blocklen + m * blocklen + i]) +
(v1 * a_ptr[l].qs[k * 4 * blocklen + m * blocklen + i + qk / 2 * 4])) >> 4;
}
sumf[m][j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * GGML_FP16_TO_FP32(a_ptr[l].d[m]);
}
}
}
}
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++)
s[(y * 4 + m) * bs + x * ncols_interleaved + j] = sumf[m][j];
}
}
}
}
static void ggml_gemm_q4_0_8x8_q8_0(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK8_0;
const int nb = n / qk;
const int ncols_interleaved = 8;
const int blocklen = 8;
assert (n % qk == 0);
assert (nr % 4 == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if ! ((defined(_MSC_VER)) && ! defined(__clang__)) && defined(__aarch64__)
#if defined(__ARM_FEATURE_SVE) && defined(__ARM_FEATURE_MATMUL_INT8)
if (ggml_cpu_has_sve() && ggml_cpu_has_matmul_int8() && ggml_cpu_get_sve_cnt() == QK8_0) {
const void * b_ptr = vx;
const void * a_ptr = vy;
float * res_ptr = s;
size_t res_stride = bs * sizeof(float);
__asm__ __volatile__(
"mov x20, #0x4\n"
"mov x13, %x[nr]\n"
"mov z28.s, #-0x4\n"
"mov x12, #0x88\n"
"ptrue p1.b\n"
"whilelt p0.s, XZR, x20\n"
"cmp x13, #0x10\n"
"mul x12, %x[nb], x12\n"
"blt 4f\n"
"1:"
"add x11, %x[b_ptr], #0x10\n"
"mov x10, %x[nc]\n"
"add x9, %x[res_ptr], %x[res_stride], LSL #4\n"
"2:"
"add x28, %x[a_ptr], #0x8\n"
"mov z24.b, #0x0\n"
"mov z15.b, #0x0\n"
"mov x27, %x[nb]\n"
"add x26, x28, x12\n"
"mov z12.b, #0x0\n"
"mov z0.b, #0x0\n"
"add x25, x26, x12\n"
"mov z13.b, #0x0\n"
"mov z1.b, #0x0\n"
"add x24, x25, x12\n"
"mov z20.b, #0x0\n"
"mov z25.b, #0x0\n"
"mov z11.b, #0x0\n"
"mov z16.b, #0x0\n"
"mov z19.b, #0x0\n"
"mov z26.b, #0x0\n"
"mov z8.b, #0x0\n"
"mov z29.b, #0x0\n"
"mov z27.b, #0x0\n"
"mov z10.b, #0x0\n"
"3:"
"ld1b { z30.b }, p1/Z, [x11]\n"
"ld1b { z21.b }, p1/Z, [x11, #1, MUL VL]\n"
"mov z18.s, #0x0\n"
"mov z7.s, #0x0\n"
"ld1rqb { z3.b }, p1/Z, [x28]\n"
"ld1rqb { z5.b }, p1/Z, [x28, #16]\n"
"mov z9.s, #0x0\n"
"mov z22.s, #0x0\n"
"ld1b { z4.b }, p1/Z, [x11, #2, MUL VL]\n"
"ld1b { z17.b }, p1/Z, [x11, #3, MUL VL]\n"
"sub x20, x11, #0x10\n"
"sub x23, x28, #0x8\n"
"lsl z31.b, z30.b, #0x4\n"
"lsl z6.b, z21.b, #0x4\n"
"ld1h { z23.s }, p1/Z, [x20]\n"
"sub x22, x26, #0x8\n"
"and z30.b, z30.b, #0xf0\n"
"and z21.b, z21.b, #0xf0\n"
"sub x21, x25, #0x8\n"
"sub x20, x24, #0x8\n"
"lsl z14.b, z4.b, #0x4\n"
"lsl z2.b, z17.b, #0x4\n"
"subs x27, x27, #0x1\n"
"add x11, x11, #0x90\n"
".inst 0x451f9872
".inst 0x45069867
"ld1rqb { z3.b }, p1/Z, [x28, #32]\n"
"and z4.b, z4.b, #0xf0\n"
".inst 0x451f98a9
".inst 0x450698b6
"ld1rqb { z5.b }, p1/Z, [x28, #48]\n"
"and z17.b, z17.b, #0xf0\n"
"fcvt z23.s, p1/m, z23.h\n"
".inst 0x450e9872
".inst 0x45029867
"ld1rqb { z3.b }, p1/Z, [x28, #64]\n"
".inst 0x450e98a9
".inst 0x450298b6
"ld1rqb { z5.b }, p1/Z, [x28, #80]\n"
"fscale z23.s, p1/m, z23.s, z28.s\n"
".inst 0x451e9872
".inst 0x45159867
"ld1rqb { z3.b }, p1/Z, [x28, #96]\n"
".inst 0x451e98a9
".inst 0x451598b6
"ld1rqb { z5.b }, p1/Z, [x28, #112]\n"
"add x28, x28, #0x88\n"
".inst 0x45049872
".inst 0x45119867
"ld1h { z3.s }, p0/Z, [x23]\n"
".inst 0x450498a9
".inst 0x451198b6
"fcvt z3.s, p1/m, z3.h\n"
"uzp1 z5.d, z18.d, z7.d\n"
"uzp2 z18.d, z18.d, z7.d\n"
"mov z3.q, z3.q[0]\n"
"uzp1 z7.d, z9.d, z22.d\n"
"uzp2 z22.d, z9.d, z22.d\n"
"fmul z9.s, z23.s, z3.s[0]\n"
"scvtf z5.s, p1/m, z5.s\n"
"scvtf z18.s, p1/m, z18.s\n"
"scvtf z7.s, p1/m, z7.s\n"
"scvtf z22.s, p1/m, z22.s\n"
"fmla z24.s, p1/M, z5.s, z9.s\n"
"ld1rqb { z5.b }, p1/Z, [x26]\n"
"fmul z9.s, z23.s, z3.s[1]\n"
"fmla z15.s, p1/M, z18.s, z9.s\n"
"ld1rqb { z18.b }, p1/Z, [x26, #16]\n"
"fmul z9.s, z23.s, z3.s[2]\n"
"fmul z3.s, z23.s, z3.s[3]\n"
"fmla z12.s, p1/M, z7.s, z9.s\n"
"mov z9.s, #0x0\n"
"ld1h { z7.s }, p0/Z, [x22]\n"
".inst 0x451f98a9
"fmla z0.s, p1/M, z22.s, z3.s\n"
"mov z22.s, #0x0\n"
"ld1h { z3.s }, p0/Z, [x21]\n"
".inst 0x450698b6
"ld1rqb { z5.b }, p1/Z, [x26, #32]\n"
"fcvt z7.s, p1/m, z7.h\n"
"fcvt z3.s, p1/m, z3.h\n"
".inst 0x450e98a9
".inst 0x450298b6
"ld1rqb { z5.b }, p1/Z, [x26, #64]\n"
"mov z7.q, z7.q[0]\n"
"mov z3.q, z3.q[0]\n"
".inst 0x451e98a9
".inst 0x451598b6
"ld1rqb { z5.b }, p1/Z, [x26, #96]\n"
".inst 0x450498a9
".inst 0x451198b6
"uzp1 z5.d, z9.d, z22.d\n"
"scvtf z5.s, p1/m, z5.s\n"
"uzp2 z22.d, z9.d, z22.d\n"
"fmul z9.s, z23.s, z7.s[0]\n"
"scvtf z22.s, p1/m, z22.s\n"
"fmla z13.s, p1/M, z5.s, z9.s\n"
"ld1rqb { z9.b }, p1/Z, [x25]\n"
"fmul z5.s, z23.s, z7.s[1]\n"
"fmla z1.s, p1/M, z22.s, z5.s\n"
"mov z5.s, #0x0\n"
"mov z22.s, #0x0\n"
".inst 0x451f9a45
".inst 0x45069a56
"ld1rqb { z18.b }, p1/Z, [x26, #48]\n"
".inst 0x450e9a45
".inst 0x45029a56
"ld1rqb { z18.b }, p1/Z, [x26, #80]\n"
".inst 0x451e9a45
".inst 0x45159a56
"ld1rqb { z18.b }, p1/Z, [x26, #112]\n"
"add x26, x26, #0x88\n"
".inst 0x45049a45
".inst 0x45119a56
"uzp1 z18.d, z5.d, z22.d\n"
"scvtf z18.s, p1/m, z18.s\n"
"uzp2 z22.d, z5.d, z22.d\n"
"fmul z5.s, z23.s, z7.s[2]\n"
"fmul z7.s, z23.s, z7.s[3]\n"
"scvtf z22.s, p1/m, z22.s\n"
"fmla z20.s, p1/M, z18.s, z5.s\n"
"ld1rqb { z18.b }, p1/Z, [x25, #16]\n"
"ld1h { z5.s }, p0/Z, [x20]\n"
"fcvt z5.s, p1/m, z5.h\n"
"fmla z25.s, p1/M, z22.s, z7.s\n"
"mov z22.s, #0x0\n"
"mov z7.s, #0x0\n"
".inst 0x451f9936
".inst 0x45069927
"ld1rqb { z9.b }, p1/Z, [x25, #32]\n"
"mov z5.q, z5.q[0]\n"
".inst 0x450e9936
".inst 0x45029927
"ld1rqb { z9.b }, p1/Z, [x25, #64]\n"
".inst 0x451e9936
".inst 0x45159927
"ld1rqb { z9.b }, p1/Z, [x25, #96]\n"
".inst 0x45049936
".inst 0x45119927
"uzp1 z9.d, z22.d, z7.d\n"
"scvtf z9.s, p1/m, z9.s\n"
"uzp2 z22.d, z22.d, z7.d\n"
"fmul z7.s, z23.s, z3.s[0]\n"
"scvtf z22.s, p1/m, z22.s\n"
"fmla z11.s, p1/M, z9.s, z7.s\n"
"ld1rqb { z9.b }, p1/Z, [x24]\n"
"fmul z7.s, z23.s, z3.s[1]\n"
"fmla z16.s, p1/M, z22.s, z7.s\n"
"mov z22.s, #0x0\n"
"mov z7.s, #0x0\n"
".inst 0x451f9a56
".inst 0x45069a47
"ld1rqb { z18.b }, p1/Z, [x25, #48]\n"
".inst 0x450e9a56
".inst 0x45029a47
"ld1rqb { z18.b }, p1/Z, [x25, #80]\n"
".inst 0x451e9a56
".inst 0x45159a47
"ld1rqb { z18.b }, p1/Z, [x25, #112]\n"
"add x25, x25, #0x88\n"
".inst 0x45049a56
".inst 0x45119a47
"uzp1 z18.d, z22.d, z7.d\n"
"scvtf z18.s, p1/m, z18.s\n"
"uzp2 z7.d, z22.d, z7.d\n"
"fmul z22.s, z23.s, z3.s[2]\n"
"fmul z3.s, z23.s, z3.s[3]\n"
"scvtf z7.s, p1/m, z7.s\n"
"fmla z19.s, p1/M, z18.s, z22.s\n"
"ld1rqb { z18.b }, p1/Z, [x24, #16]\n"
"fmul z22.s, z23.s, z5.s[0]\n"
"fmla z26.s, p1/M, z7.s, z3.s\n"
"mov z3.s, #0x0\n"
"mov z7.s, #0x0\n"
".inst 0x451f9923
".inst 0x45069927
"ld1rqb { z9.b }, p1/Z, [x24, #32]\n"
".inst 0x450e9923
".inst 0x45029927
"mov z9.s, #0x0\n"
".inst 0x451f9a49
"mov z31.s, #0x0\n"
".inst 0x45069a5f
"ld1rqb { z6.b }, p1/Z, [x24, #48]\n"
"ld1rqb { z18.b }, p1/Z, [x24, #64]\n"
".inst 0x450e98c9
"fmul z14.s, z23.s, z5.s[1]\n"
".inst 0x450298df
"ld1rqb { z6.b }, p1/Z, [x24, #80]\n"
"fmul z2.s, z23.s, z5.s[2]\n"
"fmul z23.s, z23.s, z5.s[3]\n"
".inst 0x451e9a43
".inst 0x45159a47
"ld1rqb { z5.b }, p1/Z, [x24, #96]\n"
".inst 0x451e98c9
".inst 0x451598df
"ld1rqb { z18.b }, p1/Z, [x24, #112]\n"
"add x24, x24, #0x88\n"
".inst 0x450498a3
".inst 0x451198a7
".inst 0x45049a49
".inst 0x45119a5f
"uzp1 z18.d, z3.d, z7.d\n"
"uzp2 z5.d, z3.d, z7.d\n"
"scvtf z18.s, p1/m, z18.s\n"
"uzp1 z6.d, z9.d, z31.d\n"
"uzp2 z9.d, z9.d, z31.d\n"
"scvtf z5.s, p1/m, z5.s\n"
"fmla z8.s, p1/M, z18.s, z22.s\n"
"scvtf z6.s, p1/m, z6.s\n"
"scvtf z9.s, p1/m, z9.s\n"
"fmla z29.s, p1/M, z5.s, z14.s\n"
"fmla z27.s, p1/M, z6.s, z2.s\n"
"fmla z10.s, p1/M, z9.s, z23.s\n"
"bgt 3b\n"
"mov x20, %x[res_ptr]\n"
"subs x10, x10, #0x8\n"
"add %x[res_ptr], %x[res_ptr], #0x20\n"
"st1w { z24.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z15.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z12.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z0.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z13.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z1.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z20.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z25.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z11.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z16.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z19.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z26.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z8.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z29.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z27.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"st1w { z10.s }, p1, [x20]\n"
"bne 2b\n"
"mov x20, #0x4\n"
"sub x13, x13, #0x10\n"
"cmp x13, #0x10\n"
"mov %x[res_ptr], x9\n"
"madd %x[a_ptr], x20, x12, %x[a_ptr]\n"
"bge 1b\n"
"4:"
"cbz x13, 9f\n"
"5:"
"add x25, %x[b_ptr], #0x10\n"
"mov x24, %x[nc]\n"
"add x23, %x[res_ptr], %x[res_stride], LSL #2\n"
"6:"
"mov z24.b, #0x0\n"
"mov z15.b, #0x0\n"
"add x28, %x[a_ptr], #0x8\n"
"mov x22, %x[nb]\n"
"mov z12.b, #0x0\n"
"mov z0.b, #0x0\n"
"7:"
"ld1b { z3.b }, p1/Z, [x25]\n"
"ld1b { z6.b }, p1/Z, [x25, #1, MUL VL]\n"
"mov z2.s, #0x0\n"
"mov z25.s, #0x0\n"
"ld1rqb { z26.b }, p1/Z, [x28]\n"
"ld1rqb { z21.b }, p1/Z, [x28, #16]\n"
"mov z27.s, #0x0\n"
"mov z19.s, #0x0\n"
"ld1b { z29.b }, p1/Z, [x25, #2, MUL VL]\n"
"ld1b { z16.b }, p1/Z, [x25, #3, MUL VL]\n"
"sub x21, x25, #0x10\n"
"sub x20, x28, #0x8\n"
"lsl z20.b, z3.b, #0x4\n"
"lsl z4.b, z6.b, #0x4\n"
"ld1rqb { z10.b }, p1/Z, [x28, #32]\n"
"ld1rqb { z23.b }, p1/Z, [x28, #48]\n"
"and z3.b, z3.b, #0xf0\n"
"and z6.b, z6.b, #0xf0\n"
"ld1rqb { z11.b }, p1/Z, [x28, #64]\n"
"ld1rqb { z7.b }, p1/Z, [x28, #80]\n"
"lsl z8.b, z29.b, #0x4\n"
"lsl z14.b, z16.b, #0x4\n"
"ld1rqb { z18.b }, p1/Z, [x28, #96]\n"
"ld1rqb { z30.b }, p1/Z, [x28, #112]\n"
".inst 0x45149b42
".inst 0x45049b59
"and z29.b, z29.b, #0xf0\n"
"ld1h { z17.s }, p1/Z, [x21]\n"
".inst 0x45149abb
".inst 0x45049ab3
"and z16.b, z16.b, #0xf0\n"
"ld1h { z4.s }, p0/Z, [x20]\n"
"subs x22, x22, #0x1\n"
"add x28, x28, #0x88\n"
"fcvt z17.s, p1/m, z17.h\n"
"add x25, x25, #0x90\n"
".inst 0x45089942
".inst 0x450e9959
"fcvt z4.s, p1/m, z4.h\n"
".inst 0x45089afb
".inst 0x450e9af3
"fscale z17.s, p1/m, z17.s, z28.s\n"
"mov z4.q, z4.q[0]\n"
".inst 0x45039962
".inst 0x45069979
"fmul z23.s, z17.s, z4.s[0]\n"
"fmul z9.s, z17.s, z4.s[1]\n"
"fmul z21.s, z17.s, z4.s[2]\n"
"fmul z4.s, z17.s, z4.s[3]\n"
".inst 0x450398fb
".inst 0x450698f3
".inst 0x451d9a42
".inst 0x45109a59
".inst 0x451d9bdb
".inst 0x45109bd3
"uzp1 z31.d, z2.d, z25.d\n"
"uzp2 z13.d, z2.d, z25.d\n"
"scvtf z31.s, p1/m, z31.s\n"
"uzp1 z17.d, z27.d, z19.d\n"
"uzp2 z18.d, z27.d, z19.d\n"
"scvtf z13.s, p1/m, z13.s\n"
"fmla z24.s, p1/M, z31.s, z23.s\n"
"scvtf z17.s, p1/m, z17.s\n"
"scvtf z18.s, p1/m, z18.s\n"
"fmla z15.s, p1/M, z13.s, z9.s\n"
"fmla z12.s, p1/M, z17.s, z21.s\n"
"fmla z0.s, p1/M, z18.s, z4.s\n"
"bgt 7b\n"
"mov x20, %x[res_ptr]\n"
"cmp x13, #0x1\n"
"st1w { z24.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"cmp x13, #0x2\n"
"st1w { z15.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"cmp x13, #0x3\n"
"st1w { z12.s }, p1, [x20]\n"
"add x20, x20, %x[res_stride]\n"
"ble 8f\n"
"st1w { z0.s }, p1, [x20]\n"
"8:"
"subs x24, x24, #0x8\n"
"add %x[res_ptr], %x[res_ptr], #0x20\n"
"bne 6b\n"
"subs x13, x13, #0x4\n"
"add %x[a_ptr], %x[a_ptr], x12\n"
"mov %x[res_ptr], x23\n"
"bgt 5b\n"
"9:"
: [a_ptr] "+&r" (a_ptr), [res_ptr] "+&r" (res_ptr)
: [b_ptr] "r" (b_ptr), [nr] "r" (nr), [nb] "r" (nb), [res_stride] "r" (res_stride), [nc] "r" (nc)
: "cc", "memory", "p0", "p1", "x9", "x10", "x11", "x12", "x13", "x20", "x21", "x22", "x23", "x24", "x25", "x26", "x27", "x28", "z0", "z1", "z2", "z3", "z4", "z5", "z6", "z7", "z8", "z9", "z10", "z11", "z12", "z13", "z14", "z15", "z16", "z17", "z18", "z19", "z20", "z21", "z22", "z23", "z24", "z25", "z26", "z27", "z28", "z29", "z30", "z31"
);
return;
}
#endif
#elif defined(__AVX2__) || defined(__AVX512F__)
{
const block_q4_0x8 * b_ptr_start = (const block_q4_0x8 *)vx;
const block_q8_0x4 * a_ptr_start = (const block_q8_0x4 *)vy;
int64_t b_nb = n / QK4_0;
int64_t y = 0;
const __m256i m4b = _mm256_set1_epi8(0x0F);
const __m128i loadMask = _mm_blend_epi32(_mm_setzero_si128(), _mm_set1_epi32(0xFFFFFFFF), 3);
__m256i signextendlut = _mm256_castsi128_si256(_mm_set_epi8(-1, -2, -3, -4, -5, -6, -7, -8, 7, 6, 5, 4, 3, 2, 1, 0));
signextendlut = _mm256_permute2f128_si256(signextendlut, signextendlut, 0);
__m256i requiredOrder = _mm256_set_epi32(3, 2, 1, 0, 7, 6, 5, 4);
int64_t xstart = 0;
int anr = nr - nr%16;
#ifdef __AVX512F__
int anc = nc - nc%16;
const __m512i m4bexpanded = _mm512_set1_epi8(0x0F);
__m512i signextendlutexpanded = _mm512_inserti32x8(_mm512_castsi256_si512(signextendlut), signextendlut, 1);
for (; y < anr / 4; y += 4) {
const block_q8_0x4 * a_ptrs[4];
a_ptrs[0] = a_ptr_start + (y * nb);
for (int i = 0; i < 3; ++i) {
a_ptrs[i + 1] = a_ptrs[i] + nb;
}
for (int64_t x = 0; x < anc / 8; x += 2) {
const block_q4_0x8 * b_ptr_0 = b_ptr_start + ((x)     * b_nb);
const block_q4_0x8 * b_ptr_1 = b_ptr_start + ((x + 1) * b_nb);
__m512 acc_rows[16];
for (int i = 0; i < 16; i++) {
acc_rows[i] = _mm512_setzero_ps();
}
for (int64_t b = 0; b < nb; b++) {
const __m256i rhs_raw_mat_0123_0 = _mm256_loadu_si256((const __m256i *)(b_ptr_0[b].qs));
const __m256i rhs_raw_mat_4567_0 = _mm256_loadu_si256((const __m256i *)(b_ptr_0[b].qs + 32));
const __m256i rhs_raw_mat_0123_1 = _mm256_loadu_si256((const __m256i *)(b_ptr_0[b].qs + 64));
const __m256i rhs_raw_mat_4567_1 = _mm256_loadu_si256((const __m256i *)(b_ptr_0[b].qs + 96));
const __m256i rhs_raw_mat_89AB_0 = _mm256_loadu_si256((const __m256i *)(b_ptr_1[b].qs));
const __m256i rhs_raw_mat_CDEF_0 = _mm256_loadu_si256((const __m256i *)(b_ptr_1[b].qs + 32));
const __m256i rhs_raw_mat_89AB_1 = _mm256_loadu_si256((const __m256i *)(b_ptr_1[b].qs + 64));
const __m256i rhs_raw_mat_CDEF_1 = _mm256_loadu_si256((const __m256i *)(b_ptr_1[b].qs + 96));
const __m256i rhs_raw_mat_0145_0 = _mm256_blend_epi32(rhs_raw_mat_0123_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_0, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_0, requiredOrder), rhs_raw_mat_4567_0, 240);
const __m256i rhs_raw_mat_0145_1 = _mm256_blend_epi32(rhs_raw_mat_0123_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_1, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_1, requiredOrder), rhs_raw_mat_4567_1, 240);
const __m256i rhs_raw_mat_89CD_0 = _mm256_blend_epi32(rhs_raw_mat_89AB_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_0, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_0, requiredOrder), rhs_raw_mat_CDEF_0, 240);
const __m256i rhs_raw_mat_89CD_1 = _mm256_blend_epi32(rhs_raw_mat_89AB_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_1, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_1, requiredOrder), rhs_raw_mat_CDEF_1, 240);
const __m512i rhs_raw_mat_014589CD_0 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_0), rhs_raw_mat_89CD_0, 1);
const __m512i rhs_raw_mat_2367ABEF_0 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_0), rhs_raw_mat_ABEF_0, 1);
const __m512i rhs_raw_mat_014589CD_1 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_1), rhs_raw_mat_89CD_1, 1);
const __m512i rhs_raw_mat_2367ABEF_1 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_1), rhs_raw_mat_ABEF_1, 1);
const __m512i rhs_mat_014589CD_0 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(rhs_raw_mat_014589CD_0, m4bexpanded));
const __m512i rhs_mat_2367ABEF_0 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(rhs_raw_mat_2367ABEF_0, m4bexpanded));
const __m512i rhs_mat_014589CD_1 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(rhs_raw_mat_014589CD_1, m4bexpanded));
const __m512i rhs_mat_2367ABEF_1 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(rhs_raw_mat_2367ABEF_1, m4bexpanded));
const __m512i rhs_mat_014589CD_2 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_0, 4), m4bexpanded));
const __m512i rhs_mat_2367ABEF_2 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_0, 4), m4bexpanded));
const __m512i rhs_mat_014589CD_3 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_1, 4), m4bexpanded));
const __m512i rhs_mat_2367ABEF_3 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_1, 4), m4bexpanded));
const __m512i rhs_mat_014589CD_0_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_0, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_0_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_0, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_1_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_1, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_1_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_1, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_2_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_2, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_2_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_2, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_3_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_3, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_3_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_3, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_0_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_0, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_0_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_0, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_1_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_1, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_1_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_1, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_2_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_2, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_2_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_2, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_3_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_3, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_3_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_3, (_MM_PERM_ENUM)221);
const __m512 col_scale_f32 = GGML_F32Cx8x2_LOAD(b_ptr_0[b].d, b_ptr_1[b].d);
for (int rp = 0; rp < 4; rp++) {
__m256i lhs_mat_ymm_0123_0 = _mm256_loadu_si256((const __m256i *)((a_ptrs[rp][b].qs)));
__m256i lhs_mat_ymm_01_0 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_0, lhs_mat_ymm_0123_0, 0);
__m256i lhs_mat_ymm_23_0 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_0, lhs_mat_ymm_0123_0, 17);
__m256i lhs_mat_ymm_0123_1 = _mm256_loadu_si256((const __m256i *)((a_ptrs[rp][b].qs + 32)));
__m256i lhs_mat_ymm_01_1 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_1, lhs_mat_ymm_0123_1, 0);
__m256i lhs_mat_ymm_23_1 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_1, lhs_mat_ymm_0123_1, 17);
__m256i lhs_mat_ymm_0123_2 = _mm256_loadu_si256((const __m256i *)((a_ptrs[rp][b].qs + 64)));
__m256i lhs_mat_ymm_01_2 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_2, lhs_mat_ymm_0123_2, 0);
__m256i lhs_mat_ymm_23_2 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_2, lhs_mat_ymm_0123_2, 17);
__m256i lhs_mat_ymm_0123_3 = _mm256_loadu_si256((const __m256i *)((a_ptrs[rp][b].qs + 96)));
__m256i lhs_mat_ymm_01_3 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_3, lhs_mat_ymm_0123_3, 0);
__m256i lhs_mat_ymm_23_3 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_3, lhs_mat_ymm_0123_3, 17);
__m512i lhs_mat_01_0 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_0), lhs_mat_ymm_01_0, 1);
__m512i lhs_mat_23_0 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_0), lhs_mat_ymm_23_0, 1);
__m512i lhs_mat_01_1 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_1), lhs_mat_ymm_01_1, 1);
__m512i lhs_mat_23_1 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_1), lhs_mat_ymm_23_1, 1);
__m512i lhs_mat_01_2 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_2), lhs_mat_ymm_01_2, 1);
__m512i lhs_mat_23_2 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_2), lhs_mat_ymm_23_2, 1);
__m512i lhs_mat_01_3 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_3), lhs_mat_ymm_01_3, 1);
__m512i lhs_mat_23_3 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_3), lhs_mat_ymm_23_3, 1);
const __m512i lhs_mat_01_0_sp1 = _mm512_shuffle_epi32(lhs_mat_01_0, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_0_sp1 = _mm512_shuffle_epi32(lhs_mat_23_0, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_1_sp1 = _mm512_shuffle_epi32(lhs_mat_01_1, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_1_sp1 = _mm512_shuffle_epi32(lhs_mat_23_1, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_2_sp1 = _mm512_shuffle_epi32(lhs_mat_01_2, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_2_sp1 = _mm512_shuffle_epi32(lhs_mat_23_2, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_3_sp1 = _mm512_shuffle_epi32(lhs_mat_01_3, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_3_sp1 = _mm512_shuffle_epi32(lhs_mat_23_3, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_0_sp2 = _mm512_shuffle_epi32(lhs_mat_01_0, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_0_sp2 = _mm512_shuffle_epi32(lhs_mat_23_0, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_1_sp2 = _mm512_shuffle_epi32(lhs_mat_01_1, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_1_sp2 = _mm512_shuffle_epi32(lhs_mat_23_1, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_2_sp2 = _mm512_shuffle_epi32(lhs_mat_01_2, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_2_sp2 = _mm512_shuffle_epi32(lhs_mat_23_2, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_3_sp2 = _mm512_shuffle_epi32(lhs_mat_01_3, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_3_sp2 = _mm512_shuffle_epi32(lhs_mat_23_3, (_MM_PERM_ENUM)245);
const __m512i zero = _mm512_setzero_epi32();
__m512i iacc_mat_00_sp1 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_01_3_sp1, rhs_mat_014589CD_3_sp1), lhs_mat_01_2_sp1, rhs_mat_014589CD_2_sp1), lhs_mat_01_1_sp1, rhs_mat_014589CD_1_sp1), lhs_mat_01_0_sp1, rhs_mat_014589CD_0_sp1);
__m512i iacc_mat_01_sp1 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_01_3_sp1, rhs_mat_2367ABEF_3_sp1), lhs_mat_01_2_sp1, rhs_mat_2367ABEF_2_sp1), lhs_mat_01_1_sp1, rhs_mat_2367ABEF_1_sp1), lhs_mat_01_0_sp1, rhs_mat_2367ABEF_0_sp1);
__m512i iacc_mat_10_sp1 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_23_3_sp1, rhs_mat_014589CD_3_sp1), lhs_mat_23_2_sp1, rhs_mat_014589CD_2_sp1), lhs_mat_23_1_sp1, rhs_mat_014589CD_1_sp1), lhs_mat_23_0_sp1, rhs_mat_014589CD_0_sp1);
__m512i iacc_mat_11_sp1 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_23_3_sp1, rhs_mat_2367ABEF_3_sp1), lhs_mat_23_2_sp1, rhs_mat_2367ABEF_2_sp1), lhs_mat_23_1_sp1, rhs_mat_2367ABEF_1_sp1), lhs_mat_23_0_sp1, rhs_mat_2367ABEF_0_sp1);
__m512i iacc_mat_00_sp2 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_01_3_sp2, rhs_mat_014589CD_3_sp2), lhs_mat_01_2_sp2, rhs_mat_014589CD_2_sp2), lhs_mat_01_1_sp2, rhs_mat_014589CD_1_sp2), lhs_mat_01_0_sp2, rhs_mat_014589CD_0_sp2);
__m512i iacc_mat_01_sp2 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_01_3_sp2, rhs_mat_2367ABEF_3_sp2), lhs_mat_01_2_sp2, rhs_mat_2367ABEF_2_sp2), lhs_mat_01_1_sp2, rhs_mat_2367ABEF_1_sp2), lhs_mat_01_0_sp2, rhs_mat_2367ABEF_0_sp2);
__m512i iacc_mat_10_sp2 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_23_3_sp2, rhs_mat_014589CD_3_sp2), lhs_mat_23_2_sp2, rhs_mat_014589CD_2_sp2), lhs_mat_23_1_sp2, rhs_mat_014589CD_1_sp2), lhs_mat_23_0_sp2, rhs_mat_014589CD_0_sp2);
__m512i iacc_mat_11_sp2 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_23_3_sp2, rhs_mat_2367ABEF_3_sp2), lhs_mat_23_2_sp2, rhs_mat_2367ABEF_2_sp2), lhs_mat_23_1_sp2, rhs_mat_2367ABEF_1_sp2), lhs_mat_23_0_sp2, rhs_mat_2367ABEF_0_sp2);
__m512i iacc_mat_00 = _mm512_add_epi32(iacc_mat_00_sp1, iacc_mat_00_sp2);
__m512i iacc_mat_01 = _mm512_add_epi32(iacc_mat_01_sp1, iacc_mat_01_sp2);
__m512i iacc_mat_10 = _mm512_add_epi32(iacc_mat_10_sp1, iacc_mat_10_sp2);
__m512i iacc_mat_11 = _mm512_add_epi32(iacc_mat_11_sp1, iacc_mat_11_sp2);
__m512i iacc_row_0 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_00, _mm512_shuffle_epi32(iacc_mat_01, (_MM_PERM_ENUM)78));
__m512i iacc_row_1 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_00, (_MM_PERM_ENUM)78), iacc_mat_01);
__m512i iacc_row_2 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_10, _mm512_shuffle_epi32(iacc_mat_11, (_MM_PERM_ENUM)78));
__m512i iacc_row_3 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_10, (_MM_PERM_ENUM)78), iacc_mat_11);
const __m128i row_scale_f16 = _mm_shuffle_epi32(_mm_maskload_epi32((int const*)(a_ptrs[rp][b].d), loadMask), 68);
const __m512 row_scale_f32 = GGML_F32Cx16_REPEAT_LOAD(row_scale_f16);
acc_rows[rp * 4]     = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_0), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 0)),   acc_rows[rp * 4]);
acc_rows[rp * 4 + 1] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_1), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 85)),  acc_rows[rp * 4 + 1]);
acc_rows[rp * 4 + 2] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_2), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_rows[rp * 4 + 2]);
acc_rows[rp * 4 + 3] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_3), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_rows[rp * 4 + 3]);
}
}
for (int i = 0; i < 16; i++) {
_mm512_storeu_ps((float *)(s + ((y * 4 + i) * bs + x * 8)), acc_rows[i]);
}
}
}
for (; y < nr / 4; y ++) {
const block_q8_0x4 * a_ptr = a_ptr_start + (y * nb);
for (int64_t x = 0; x < anc / 8; x += 2) {
const block_q4_0x8 * b_ptr_0 = b_ptr_start + ((x)     * b_nb);
const block_q4_0x8 * b_ptr_1 = b_ptr_start + ((x + 1) * b_nb);
__m512 acc_rows[4];
for (int i = 0; i < 4; i++) {
acc_rows[i] = _mm512_setzero_ps();
}
for (int64_t b = 0; b < nb; b++) {
const __m256i rhs_raw_mat_0123_0 = _mm256_loadu_si256((const __m256i *)(b_ptr_0[b].qs));
const __m256i rhs_raw_mat_4567_0 = _mm256_loadu_si256((const __m256i *)(b_ptr_0[b].qs + 32));
const __m256i rhs_raw_mat_0123_1 = _mm256_loadu_si256((const __m256i *)(b_ptr_0[b].qs + 64));
const __m256i rhs_raw_mat_4567_1 = _mm256_loadu_si256((const __m256i *)(b_ptr_0[b].qs + 96));
const __m256i rhs_raw_mat_89AB_0 = _mm256_loadu_si256((const __m256i *)(b_ptr_1[b].qs));
const __m256i rhs_raw_mat_CDEF_0 = _mm256_loadu_si256((const __m256i *)(b_ptr_1[b].qs + 32));
const __m256i rhs_raw_mat_89AB_1 = _mm256_loadu_si256((const __m256i *)(b_ptr_1[b].qs + 64));
const __m256i rhs_raw_mat_CDEF_1 = _mm256_loadu_si256((const __m256i *)(b_ptr_1[b].qs + 96));
const __m256i rhs_raw_mat_0145_0 = _mm256_blend_epi32(rhs_raw_mat_0123_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_0, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_0, requiredOrder), rhs_raw_mat_4567_0, 240);
const __m256i rhs_raw_mat_0145_1 = _mm256_blend_epi32(rhs_raw_mat_0123_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_1, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_1, requiredOrder), rhs_raw_mat_4567_1, 240);
const __m256i rhs_raw_mat_89CD_0 = _mm256_blend_epi32(rhs_raw_mat_89AB_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_0, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_0, requiredOrder), rhs_raw_mat_CDEF_0, 240);
const __m256i rhs_raw_mat_89CD_1 = _mm256_blend_epi32(rhs_raw_mat_89AB_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_1, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_1, requiredOrder), rhs_raw_mat_CDEF_1, 240);
const __m512i rhs_raw_mat_014589CD_0 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_0), rhs_raw_mat_89CD_0, 1);
const __m512i rhs_raw_mat_2367ABEF_0 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_0), rhs_raw_mat_ABEF_0, 1);
const __m512i rhs_raw_mat_014589CD_1 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_1), rhs_raw_mat_89CD_1, 1);
const __m512i rhs_raw_mat_2367ABEF_1 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_1), rhs_raw_mat_ABEF_1, 1);
const __m512i rhs_mat_014589CD_0 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(rhs_raw_mat_014589CD_0, m4bexpanded));
const __m512i rhs_mat_2367ABEF_0 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(rhs_raw_mat_2367ABEF_0, m4bexpanded));
const __m512i rhs_mat_014589CD_1 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(rhs_raw_mat_014589CD_1, m4bexpanded));
const __m512i rhs_mat_2367ABEF_1 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(rhs_raw_mat_2367ABEF_1, m4bexpanded));
const __m512i rhs_mat_014589CD_2 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_0, 4), m4bexpanded));
const __m512i rhs_mat_2367ABEF_2 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_0, 4), m4bexpanded));
const __m512i rhs_mat_014589CD_3 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_1, 4), m4bexpanded));
const __m512i rhs_mat_2367ABEF_3 = _mm512_shuffle_epi8(signextendlutexpanded, _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_1, 4), m4bexpanded));
const __m512i rhs_mat_014589CD_0_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_0, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_0_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_0, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_1_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_1, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_1_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_1, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_2_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_2, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_2_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_2, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_3_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_3, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_3_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_3, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_0_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_0, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_0_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_0, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_1_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_1, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_1_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_1, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_2_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_2, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_2_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_2, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_3_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_3, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_3_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_3, (_MM_PERM_ENUM)221);
const __m512 col_scale_f32 = GGML_F32Cx8x2_LOAD(b_ptr_0[b].d, b_ptr_1[b].d);
__m256i lhs_mat_ymm_0123_0 = _mm256_loadu_si256((const __m256i *)((a_ptr[b].qs)));
__m256i lhs_mat_ymm_01_0 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_0, lhs_mat_ymm_0123_0, 0);
__m256i lhs_mat_ymm_23_0 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_0, lhs_mat_ymm_0123_0, 17);
__m256i lhs_mat_ymm_0123_1 = _mm256_loadu_si256((const __m256i *)((a_ptr[b].qs + 32)));
__m256i lhs_mat_ymm_01_1 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_1, lhs_mat_ymm_0123_1, 0);
__m256i lhs_mat_ymm_23_1 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_1, lhs_mat_ymm_0123_1, 17);
__m256i lhs_mat_ymm_0123_2 = _mm256_loadu_si256((const __m256i *)((a_ptr[b].qs + 64)));
__m256i lhs_mat_ymm_01_2 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_2, lhs_mat_ymm_0123_2, 0);
__m256i lhs_mat_ymm_23_2 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_2, lhs_mat_ymm_0123_2, 17);
__m256i lhs_mat_ymm_0123_3 = _mm256_loadu_si256((const __m256i *)((a_ptr[b].qs + 96)));
__m256i lhs_mat_ymm_01_3 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_3, lhs_mat_ymm_0123_3, 0);
__m256i lhs_mat_ymm_23_3 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_3, lhs_mat_ymm_0123_3, 17);
__m512i lhs_mat_01_0 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_0), lhs_mat_ymm_01_0, 1);
__m512i lhs_mat_23_0 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_0), lhs_mat_ymm_23_0, 1);
__m512i lhs_mat_01_1 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_1), lhs_mat_ymm_01_1, 1);
__m512i lhs_mat_23_1 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_1), lhs_mat_ymm_23_1, 1);
__m512i lhs_mat_01_2 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_2), lhs_mat_ymm_01_2, 1);
__m512i lhs_mat_23_2 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_2), lhs_mat_ymm_23_2, 1);
__m512i lhs_mat_01_3 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_3), lhs_mat_ymm_01_3, 1);
__m512i lhs_mat_23_3 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_3), lhs_mat_ymm_23_3, 1);
const __m512i lhs_mat_01_0_sp1 = _mm512_shuffle_epi32(lhs_mat_01_0, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_0_sp1 = _mm512_shuffle_epi32(lhs_mat_23_0, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_1_sp1 = _mm512_shuffle_epi32(lhs_mat_01_1, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_1_sp1 = _mm512_shuffle_epi32(lhs_mat_23_1, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_2_sp1 = _mm512_shuffle_epi32(lhs_mat_01_2, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_2_sp1 = _mm512_shuffle_epi32(lhs_mat_23_2, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_3_sp1 = _mm512_shuffle_epi32(lhs_mat_01_3, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_3_sp1 = _mm512_shuffle_epi32(lhs_mat_23_3, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_0_sp2 = _mm512_shuffle_epi32(lhs_mat_01_0, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_0_sp2 = _mm512_shuffle_epi32(lhs_mat_23_0, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_1_sp2 = _mm512_shuffle_epi32(lhs_mat_01_1, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_1_sp2 = _mm512_shuffle_epi32(lhs_mat_23_1, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_2_sp2 = _mm512_shuffle_epi32(lhs_mat_01_2, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_2_sp2 = _mm512_shuffle_epi32(lhs_mat_23_2, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_3_sp2 = _mm512_shuffle_epi32(lhs_mat_01_3, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_3_sp2 = _mm512_shuffle_epi32(lhs_mat_23_3, (_MM_PERM_ENUM)245);
const __m512i zero = _mm512_setzero_epi32();
__m512i iacc_mat_00_sp1 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_01_3_sp1, rhs_mat_014589CD_3_sp1), lhs_mat_01_2_sp1, rhs_mat_014589CD_2_sp1), lhs_mat_01_1_sp1, rhs_mat_014589CD_1_sp1), lhs_mat_01_0_sp1, rhs_mat_014589CD_0_sp1);
__m512i iacc_mat_01_sp1 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_01_3_sp1, rhs_mat_2367ABEF_3_sp1), lhs_mat_01_2_sp1, rhs_mat_2367ABEF_2_sp1), lhs_mat_01_1_sp1, rhs_mat_2367ABEF_1_sp1), lhs_mat_01_0_sp1, rhs_mat_2367ABEF_0_sp1);
__m512i iacc_mat_10_sp1 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_23_3_sp1, rhs_mat_014589CD_3_sp1), lhs_mat_23_2_sp1, rhs_mat_014589CD_2_sp1), lhs_mat_23_1_sp1, rhs_mat_014589CD_1_sp1), lhs_mat_23_0_sp1, rhs_mat_014589CD_0_sp1);
__m512i iacc_mat_11_sp1 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_23_3_sp1, rhs_mat_2367ABEF_3_sp1), lhs_mat_23_2_sp1, rhs_mat_2367ABEF_2_sp1), lhs_mat_23_1_sp1, rhs_mat_2367ABEF_1_sp1), lhs_mat_23_0_sp1, rhs_mat_2367ABEF_0_sp1);
__m512i iacc_mat_00_sp2 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_01_3_sp2, rhs_mat_014589CD_3_sp2), lhs_mat_01_2_sp2, rhs_mat_014589CD_2_sp2), lhs_mat_01_1_sp2, rhs_mat_014589CD_1_sp2), lhs_mat_01_0_sp2, rhs_mat_014589CD_0_sp2);
__m512i iacc_mat_01_sp2 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_01_3_sp2, rhs_mat_2367ABEF_3_sp2), lhs_mat_01_2_sp2, rhs_mat_2367ABEF_2_sp2), lhs_mat_01_1_sp2, rhs_mat_2367ABEF_1_sp2), lhs_mat_01_0_sp2, rhs_mat_2367ABEF_0_sp2);
__m512i iacc_mat_10_sp2 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_23_3_sp2, rhs_mat_014589CD_3_sp2), lhs_mat_23_2_sp2, rhs_mat_014589CD_2_sp2), lhs_mat_23_1_sp2, rhs_mat_014589CD_1_sp2), lhs_mat_23_0_sp2, rhs_mat_014589CD_0_sp2);
__m512i iacc_mat_11_sp2 = mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(mul_sum_i8_pairs_acc_int32x16(zero, lhs_mat_23_3_sp2, rhs_mat_2367ABEF_3_sp2), lhs_mat_23_2_sp2, rhs_mat_2367ABEF_2_sp2), lhs_mat_23_1_sp2, rhs_mat_2367ABEF_1_sp2), lhs_mat_23_0_sp2, rhs_mat_2367ABEF_0_sp2);
__m512i iacc_mat_00 = _mm512_add_epi32(iacc_mat_00_sp1, iacc_mat_00_sp2);
__m512i iacc_mat_01 = _mm512_add_epi32(iacc_mat_01_sp1, iacc_mat_01_sp2);
__m512i iacc_mat_10 = _mm512_add_epi32(iacc_mat_10_sp1, iacc_mat_10_sp2);
__m512i iacc_mat_11 = _mm512_add_epi32(iacc_mat_11_sp1, iacc_mat_11_sp2);
__m512i iacc_row_0 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_00, _mm512_shuffle_epi32(iacc_mat_01, (_MM_PERM_ENUM)78));
__m512i iacc_row_1 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_00, (_MM_PERM_ENUM)78), iacc_mat_01);
__m512i iacc_row_2 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_10, _mm512_shuffle_epi32(iacc_mat_11, (_MM_PERM_ENUM)78));
__m512i iacc_row_3 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_10, (_MM_PERM_ENUM)78), iacc_mat_11);
const __m128i row_scale_f16 = _mm_shuffle_epi32(_mm_maskload_epi32((int const*)(a_ptr[b].d), loadMask), 68);
const __m512 row_scale_f32 = GGML_F32Cx16_REPEAT_LOAD(row_scale_f16);
acc_rows[0] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_0), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 0)),   acc_rows[0]);
acc_rows[1] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_1), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 85)),  acc_rows[1]);
acc_rows[2] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_2), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_rows[2]);
acc_rows[3] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_3), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_rows[3]);
}
for (int i = 0; i < 4; i++) {
_mm512_storeu_ps((float *)(s + ((y * 4 + i) * bs + x * 8)), acc_rows[i]);
}
}
}
if (anc != nc) {
xstart = anc/8;
y = 0;
}
#endif
for (; y < anr / 4; y += 4) {
const block_q8_0x4 * a_ptrs[4];
a_ptrs[0] = a_ptr_start + (y * nb);
for (int i = 0; i < 3; ++i) {
a_ptrs[i + 1] = a_ptrs[i] + nb;
}
for (int64_t x = xstart; x < nc / 8; x++) {
const block_q4_0x8 * b_ptr = b_ptr_start + (x * b_nb);
__m256 acc_rows[16];
for (int i = 0; i < 16; i++) {
acc_rows[i] = _mm256_setzero_ps();
}
for (int64_t b = 0; b < nb; b++) {
const __m256i rhs_raw_mat_0123_0 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs));
const __m256i rhs_raw_mat_4567_0 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs + 32));
const __m256i rhs_raw_mat_0123_1 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs + 64));
const __m256i rhs_raw_mat_4567_1 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs + 96));
const __m256i rhs_raw_mat_0145_0 = _mm256_blend_epi32(rhs_raw_mat_0123_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_0, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_0, requiredOrder), rhs_raw_mat_4567_0, 240);
const __m256i rhs_raw_mat_0145_1 = _mm256_blend_epi32(rhs_raw_mat_0123_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_1, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_1, requiredOrder), rhs_raw_mat_4567_1, 240);
const __m256i rhs_mat_0145_0 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_mat_0145_0, m4b));
const __m256i rhs_mat_2367_0 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_mat_2367_0, m4b));
const __m256i rhs_mat_0145_1 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_mat_0145_1, m4b));
const __m256i rhs_mat_2367_1 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_mat_2367_1, m4b));
const __m256i rhs_mat_0145_2 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_0, 4), m4b));
const __m256i rhs_mat_2367_2 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_0, 4), m4b));
const __m256i rhs_mat_0145_3 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_1, 4), m4b));
const __m256i rhs_mat_2367_3 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_1, 4), m4b));
const __m256i rhs_mat_0145_0_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_0, 136);
const __m256i rhs_mat_2367_0_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_0, 136);
const __m256i rhs_mat_0145_1_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_1, 136);
const __m256i rhs_mat_2367_1_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_1, 136);
const __m256i rhs_mat_0145_2_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_2, 136);
const __m256i rhs_mat_2367_2_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_2, 136);
const __m256i rhs_mat_0145_3_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_3, 136);
const __m256i rhs_mat_2367_3_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_3, 136);
const __m256i rhs_mat_0145_0_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_0, 221);
const __m256i rhs_mat_2367_0_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_0, 221);
const __m256i rhs_mat_0145_1_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_1, 221);
const __m256i rhs_mat_2367_1_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_1, 221);
const __m256i rhs_mat_0145_2_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_2, 221);
const __m256i rhs_mat_2367_2_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_2, 221);
const __m256i rhs_mat_0145_3_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_3, 221);
const __m256i rhs_mat_2367_3_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_3, 221);
const __m256 col_scale_f32 = GGML_F32Cx8_LOAD(b_ptr[b].d);
for (int rp = 0; rp < 4; rp++) {
__m256i lhs_mat_0123_0 = _mm256_loadu_si256((const __m256i *)((a_ptrs[rp][b].qs)));
__m256i lhs_mat_01_0 = _mm256_permute2f128_si256(lhs_mat_0123_0, lhs_mat_0123_0, 0);
__m256i lhs_mat_23_0 = _mm256_permute2f128_si256(lhs_mat_0123_0, lhs_mat_0123_0, 17);
__m256i lhs_mat_0123_1 = _mm256_loadu_si256((const __m256i *)((a_ptrs[rp][b].qs + 32)));
__m256i lhs_mat_01_1 = _mm256_permute2f128_si256(lhs_mat_0123_1, lhs_mat_0123_1, 0);
__m256i lhs_mat_23_1 = _mm256_permute2f128_si256(lhs_mat_0123_1, lhs_mat_0123_1, 17);
__m256i lhs_mat_0123_2 = _mm256_loadu_si256((const __m256i *)((a_ptrs[rp][b].qs + 64)));
__m256i lhs_mat_01_2 = _mm256_permute2f128_si256(lhs_mat_0123_2, lhs_mat_0123_2, 0);
__m256i lhs_mat_23_2 = _mm256_permute2f128_si256(lhs_mat_0123_2, lhs_mat_0123_2, 17);
__m256i lhs_mat_0123_3 = _mm256_loadu_si256((const __m256i *)((a_ptrs[rp][b].qs + 96)));
__m256i lhs_mat_01_3 = _mm256_permute2f128_si256(lhs_mat_0123_3, lhs_mat_0123_3, 0);
__m256i lhs_mat_23_3 = _mm256_permute2f128_si256(lhs_mat_0123_3, lhs_mat_0123_3, 17);
const __m256i lhs_mat_01_0_sp1 = _mm256_shuffle_epi32(lhs_mat_01_0, 160);
const __m256i lhs_mat_23_0_sp1 = _mm256_shuffle_epi32(lhs_mat_23_0, 160);
const __m256i lhs_mat_01_1_sp1 = _mm256_shuffle_epi32(lhs_mat_01_1, 160);
const __m256i lhs_mat_23_1_sp1 = _mm256_shuffle_epi32(lhs_mat_23_1, 160);
const __m256i lhs_mat_01_2_sp1 = _mm256_shuffle_epi32(lhs_mat_01_2, 160);
const __m256i lhs_mat_23_2_sp1 = _mm256_shuffle_epi32(lhs_mat_23_2, 160);
const __m256i lhs_mat_01_3_sp1 = _mm256_shuffle_epi32(lhs_mat_01_3, 160);
const __m256i lhs_mat_23_3_sp1 = _mm256_shuffle_epi32(lhs_mat_23_3, 160);
const __m256i lhs_mat_01_0_sp2 = _mm256_shuffle_epi32(lhs_mat_01_0, 245);
const __m256i lhs_mat_23_0_sp2 = _mm256_shuffle_epi32(lhs_mat_23_0, 245);
const __m256i lhs_mat_01_1_sp2 = _mm256_shuffle_epi32(lhs_mat_01_1, 245);
const __m256i lhs_mat_23_1_sp2 = _mm256_shuffle_epi32(lhs_mat_23_1, 245);
const __m256i lhs_mat_01_2_sp2 = _mm256_shuffle_epi32(lhs_mat_01_2, 245);
const __m256i lhs_mat_23_2_sp2 = _mm256_shuffle_epi32(lhs_mat_23_2, 245);
const __m256i lhs_mat_01_3_sp2 = _mm256_shuffle_epi32(lhs_mat_01_3, 245);
const __m256i lhs_mat_23_3_sp2 = _mm256_shuffle_epi32(lhs_mat_23_3, 245);
const __m256i zero = _mm256_setzero_si256();
__m256i iacc_mat_00_sp1 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_01_3_sp1, rhs_mat_0145_3_sp1), lhs_mat_01_2_sp1, rhs_mat_0145_2_sp1), lhs_mat_01_1_sp1, rhs_mat_0145_1_sp1), lhs_mat_01_0_sp1, rhs_mat_0145_0_sp1);
__m256i iacc_mat_01_sp1 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_01_3_sp1, rhs_mat_2367_3_sp1), lhs_mat_01_2_sp1, rhs_mat_2367_2_sp1), lhs_mat_01_1_sp1, rhs_mat_2367_1_sp1), lhs_mat_01_0_sp1, rhs_mat_2367_0_sp1);
__m256i iacc_mat_10_sp1 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_23_3_sp1, rhs_mat_0145_3_sp1), lhs_mat_23_2_sp1, rhs_mat_0145_2_sp1), lhs_mat_23_1_sp1, rhs_mat_0145_1_sp1), lhs_mat_23_0_sp1, rhs_mat_0145_0_sp1);
__m256i iacc_mat_11_sp1 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_23_3_sp1, rhs_mat_2367_3_sp1), lhs_mat_23_2_sp1, rhs_mat_2367_2_sp1), lhs_mat_23_1_sp1, rhs_mat_2367_1_sp1), lhs_mat_23_0_sp1, rhs_mat_2367_0_sp1);
__m256i iacc_mat_00_sp2 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_01_3_sp2, rhs_mat_0145_3_sp2), lhs_mat_01_2_sp2, rhs_mat_0145_2_sp2), lhs_mat_01_1_sp2, rhs_mat_0145_1_sp2), lhs_mat_01_0_sp2, rhs_mat_0145_0_sp2);
__m256i iacc_mat_01_sp2 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_01_3_sp2, rhs_mat_2367_3_sp2), lhs_mat_01_2_sp2, rhs_mat_2367_2_sp2), lhs_mat_01_1_sp2, rhs_mat_2367_1_sp2), lhs_mat_01_0_sp2, rhs_mat_2367_0_sp2);
__m256i iacc_mat_10_sp2 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_23_3_sp2, rhs_mat_0145_3_sp2), lhs_mat_23_2_sp2, rhs_mat_0145_2_sp2), lhs_mat_23_1_sp2, rhs_mat_0145_1_sp2), lhs_mat_23_0_sp2, rhs_mat_0145_0_sp2);
__m256i iacc_mat_11_sp2 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_23_3_sp2, rhs_mat_2367_3_sp2), lhs_mat_23_2_sp2, rhs_mat_2367_2_sp2), lhs_mat_23_1_sp2, rhs_mat_2367_1_sp2), lhs_mat_23_0_sp2, rhs_mat_2367_0_sp2);
__m256i iacc_mat_00 = _mm256_add_epi32(iacc_mat_00_sp1, iacc_mat_00_sp2);
__m256i iacc_mat_01 = _mm256_add_epi32(iacc_mat_01_sp1, iacc_mat_01_sp2);
__m256i iacc_mat_10 = _mm256_add_epi32(iacc_mat_10_sp1, iacc_mat_10_sp2);
__m256i iacc_mat_11 = _mm256_add_epi32(iacc_mat_11_sp1, iacc_mat_11_sp2);
__m256i iacc_row_0 = _mm256_blend_epi32(iacc_mat_00, _mm256_shuffle_epi32(iacc_mat_01, 78), 204);
__m256i iacc_row_1 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_00, 78), iacc_mat_01, 204);
__m256i iacc_row_2 = _mm256_blend_epi32(iacc_mat_10, _mm256_shuffle_epi32(iacc_mat_11, 78), 204);
__m256i iacc_row_3 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_10, 78), iacc_mat_11, 204);
const __m256 row_scale_f32 = GGML_F32Cx8_REPEAT_LOAD(a_ptrs[rp][b].d, loadMask);
acc_rows[rp * 4] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_0), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_rows[rp * 4]);
acc_rows[rp * 4 + 1] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_1), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_rows[rp * 4 + 1]);
acc_rows[rp * 4 + 2] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_2), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_rows[rp * 4 + 2]);
acc_rows[rp * 4 + 3] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_3), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32,  255)), acc_rows[rp * 4 + 3]);
}
}
for (int i = 0; i < 16; i++) {
_mm256_storeu_ps((float *)(s + ((y * 4 + i) * bs + x * 8)), acc_rows[i]);
}
}
}
for (; y < nr / 4; y ++) {
const block_q8_0x4 * a_ptr = a_ptr_start + (y * nb);
for (int64_t x = xstart; x < nc / 8; x++) {
const block_q4_0x8 * b_ptr = b_ptr_start + (x * b_nb);
__m256 acc_rows[4];
for (int i = 0; i < 4; i++) {
acc_rows[i] = _mm256_setzero_ps();
}
for (int64_t b = 0; b < nb; b++) {
const __m256i rhs_raw_mat_0123_0 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs));
const __m256i rhs_raw_mat_4567_0 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs + 32));
const __m256i rhs_raw_mat_0123_1 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs + 64));
const __m256i rhs_raw_mat_4567_1 = _mm256_loadu_si256((const __m256i *)(b_ptr[b].qs + 96));
const __m256i rhs_raw_mat_0145_0 = _mm256_blend_epi32(rhs_raw_mat_0123_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_0, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_0, requiredOrder), rhs_raw_mat_4567_0, 240);
const __m256i rhs_raw_mat_0145_1 = _mm256_blend_epi32(rhs_raw_mat_0123_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_1, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_1, requiredOrder), rhs_raw_mat_4567_1, 240);
const __m256i rhs_mat_0145_0 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_mat_0145_0, m4b));
const __m256i rhs_mat_2367_0 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_mat_2367_0, m4b));
const __m256i rhs_mat_0145_1 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_mat_0145_1, m4b));
const __m256i rhs_mat_2367_1 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(rhs_raw_mat_2367_1, m4b));
const __m256i rhs_mat_0145_2 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_0, 4), m4b));
const __m256i rhs_mat_2367_2 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_0, 4), m4b));
const __m256i rhs_mat_0145_3 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_1, 4), m4b));
const __m256i rhs_mat_2367_3 = _mm256_shuffle_epi8(signextendlut, _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_1, 4), m4b));
const __m256i rhs_mat_0145_0_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_0, 136);
const __m256i rhs_mat_2367_0_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_0, 136);
const __m256i rhs_mat_0145_1_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_1, 136);
const __m256i rhs_mat_2367_1_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_1, 136);
const __m256i rhs_mat_0145_2_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_2, 136);
const __m256i rhs_mat_2367_2_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_2, 136);
const __m256i rhs_mat_0145_3_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_3, 136);
const __m256i rhs_mat_2367_3_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_3, 136);
const __m256i rhs_mat_0145_0_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_0, 221);
const __m256i rhs_mat_2367_0_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_0, 221);
const __m256i rhs_mat_0145_1_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_1, 221);
const __m256i rhs_mat_2367_1_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_1, 221);
const __m256i rhs_mat_0145_2_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_2, 221);
const __m256i rhs_mat_2367_2_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_2, 221);
const __m256i rhs_mat_0145_3_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_3, 221);
const __m256i rhs_mat_2367_3_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_3, 221);
const __m256 col_scale_f32 = GGML_F32Cx8_LOAD(b_ptr[b].d);
__m256i lhs_mat_0123_0 = _mm256_loadu_si256((const __m256i *)((a_ptr[b].qs)));
__m256i lhs_mat_01_0 = _mm256_permute2f128_si256(lhs_mat_0123_0, lhs_mat_0123_0, 0);
__m256i lhs_mat_23_0 = _mm256_permute2f128_si256(lhs_mat_0123_0, lhs_mat_0123_0, 17);
__m256i lhs_mat_0123_1 = _mm256_loadu_si256((const __m256i *)((a_ptr[b].qs + 32)));
__m256i lhs_mat_01_1 = _mm256_permute2f128_si256(lhs_mat_0123_1, lhs_mat_0123_1, 0);
__m256i lhs_mat_23_1 = _mm256_permute2f128_si256(lhs_mat_0123_1, lhs_mat_0123_1, 17);
__m256i lhs_mat_0123_2 = _mm256_loadu_si256((const __m256i *)((a_ptr[b].qs + 64)));
__m256i lhs_mat_01_2 = _mm256_permute2f128_si256(lhs_mat_0123_2, lhs_mat_0123_2, 0);
__m256i lhs_mat_23_2 = _mm256_permute2f128_si256(lhs_mat_0123_2, lhs_mat_0123_2, 17);
__m256i lhs_mat_0123_3 = _mm256_loadu_si256((const __m256i *)((a_ptr[b].qs + 96)));
__m256i lhs_mat_01_3 = _mm256_permute2f128_si256(lhs_mat_0123_3, lhs_mat_0123_3, 0);
__m256i lhs_mat_23_3 = _mm256_permute2f128_si256(lhs_mat_0123_3, lhs_mat_0123_3, 17);
const __m256i lhs_mat_01_0_sp1 = _mm256_shuffle_epi32(lhs_mat_01_0, 160);
const __m256i lhs_mat_23_0_sp1 = _mm256_shuffle_epi32(lhs_mat_23_0, 160);
const __m256i lhs_mat_01_1_sp1 = _mm256_shuffle_epi32(lhs_mat_01_1, 160);
const __m256i lhs_mat_23_1_sp1 = _mm256_shuffle_epi32(lhs_mat_23_1, 160);
const __m256i lhs_mat_01_2_sp1 = _mm256_shuffle_epi32(lhs_mat_01_2, 160);
const __m256i lhs_mat_23_2_sp1 = _mm256_shuffle_epi32(lhs_mat_23_2, 160);
const __m256i lhs_mat_01_3_sp1 = _mm256_shuffle_epi32(lhs_mat_01_3, 160);
const __m256i lhs_mat_23_3_sp1 = _mm256_shuffle_epi32(lhs_mat_23_3, 160);
const __m256i lhs_mat_01_0_sp2 = _mm256_shuffle_epi32(lhs_mat_01_0, 245);
const __m256i lhs_mat_23_0_sp2 = _mm256_shuffle_epi32(lhs_mat_23_0, 245);
const __m256i lhs_mat_01_1_sp2 = _mm256_shuffle_epi32(lhs_mat_01_1, 245);
const __m256i lhs_mat_23_1_sp2 = _mm256_shuffle_epi32(lhs_mat_23_1, 245);
const __m256i lhs_mat_01_2_sp2 = _mm256_shuffle_epi32(lhs_mat_01_2, 245);
const __m256i lhs_mat_23_2_sp2 = _mm256_shuffle_epi32(lhs_mat_23_2, 245);
const __m256i lhs_mat_01_3_sp2 = _mm256_shuffle_epi32(lhs_mat_01_3, 245);
const __m256i lhs_mat_23_3_sp2 = _mm256_shuffle_epi32(lhs_mat_23_3, 245);
const __m256i zero = _mm256_setzero_si256();
__m256i iacc_mat_00_sp1 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_01_3_sp1, rhs_mat_0145_3_sp1), lhs_mat_01_2_sp1, rhs_mat_0145_2_sp1), lhs_mat_01_1_sp1, rhs_mat_0145_1_sp1), lhs_mat_01_0_sp1, rhs_mat_0145_0_sp1);
__m256i iacc_mat_01_sp1 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_01_3_sp1, rhs_mat_2367_3_sp1), lhs_mat_01_2_sp1, rhs_mat_2367_2_sp1), lhs_mat_01_1_sp1, rhs_mat_2367_1_sp1), lhs_mat_01_0_sp1, rhs_mat_2367_0_sp1);
__m256i iacc_mat_10_sp1 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_23_3_sp1, rhs_mat_0145_3_sp1), lhs_mat_23_2_sp1, rhs_mat_0145_2_sp1), lhs_mat_23_1_sp1, rhs_mat_0145_1_sp1), lhs_mat_23_0_sp1, rhs_mat_0145_0_sp1);
__m256i iacc_mat_11_sp1 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_23_3_sp1, rhs_mat_2367_3_sp1), lhs_mat_23_2_sp1, rhs_mat_2367_2_sp1), lhs_mat_23_1_sp1, rhs_mat_2367_1_sp1), lhs_mat_23_0_sp1, rhs_mat_2367_0_sp1);
__m256i iacc_mat_00_sp2 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_01_3_sp2, rhs_mat_0145_3_sp2), lhs_mat_01_2_sp2, rhs_mat_0145_2_sp2), lhs_mat_01_1_sp2, rhs_mat_0145_1_sp2), lhs_mat_01_0_sp2, rhs_mat_0145_0_sp2);
__m256i iacc_mat_01_sp2 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_01_3_sp2, rhs_mat_2367_3_sp2), lhs_mat_01_2_sp2, rhs_mat_2367_2_sp2), lhs_mat_01_1_sp2, rhs_mat_2367_1_sp2), lhs_mat_01_0_sp2, rhs_mat_2367_0_sp2);
__m256i iacc_mat_10_sp2 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_23_3_sp2, rhs_mat_0145_3_sp2), lhs_mat_23_2_sp2, rhs_mat_0145_2_sp2), lhs_mat_23_1_sp2, rhs_mat_0145_1_sp2), lhs_mat_23_0_sp2, rhs_mat_0145_0_sp2);
__m256i iacc_mat_11_sp2 = mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(mul_sum_i8_pairs_acc_int32x8(zero, lhs_mat_23_3_sp2, rhs_mat_2367_3_sp2), lhs_mat_23_2_sp2, rhs_mat_2367_2_sp2), lhs_mat_23_1_sp2, rhs_mat_2367_1_sp2), lhs_mat_23_0_sp2, rhs_mat_2367_0_sp2);
__m256i iacc_mat_00 = _mm256_add_epi32(iacc_mat_00_sp1, iacc_mat_00_sp2);
__m256i iacc_mat_01 = _mm256_add_epi32(iacc_mat_01_sp1, iacc_mat_01_sp2);
__m256i iacc_mat_10 = _mm256_add_epi32(iacc_mat_10_sp1, iacc_mat_10_sp2);
__m256i iacc_mat_11 = _mm256_add_epi32(iacc_mat_11_sp1, iacc_mat_11_sp2);
__m256i iacc_row_0 = _mm256_blend_epi32(iacc_mat_00, _mm256_shuffle_epi32(iacc_mat_01, 78), 204);
__m256i iacc_row_1 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_00, 78), iacc_mat_01, 204);
__m256i iacc_row_2 = _mm256_blend_epi32(iacc_mat_10, _mm256_shuffle_epi32(iacc_mat_11, 78), 204);
__m256i iacc_row_3 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_10, 78), iacc_mat_11, 204);
const __m256 row_scale_f32 = GGML_F32Cx8_REPEAT_LOAD(a_ptr[b].d, loadMask);
acc_rows[0] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_0), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_rows[0]);
acc_rows[1] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_1), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_rows[1]);
acc_rows[2] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_2), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_rows[2]);
acc_rows[3] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_3), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_rows[3]);
}
for (int i = 0; i < 4; i++) {
_mm256_storeu_ps((float *)(s + ((y * 4 + i) * bs + x * 8)), acc_rows[i]);
}
}
}
return;
}
#elif defined(__riscv_v_intrinsic)
if (__riscv_vlenb() >= QK4_0) {
const size_t vl = QK4_0;
for (int y = 0; y < nr / 4; y++) {
const block_q8_0x4 * a_ptr = (const block_q8_0x4 *) vy + (y * nb);
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_0x8 * b_ptr = (const block_q4_0x8 *) vx + (x * nb);
vfloat32m1_t sumf0 = __riscv_vfmv_v_f_f32m1(0.0, vl / 4);
vfloat32m1_t sumf1 = __riscv_vfmv_v_f_f32m1(0.0, vl / 4);
vfloat32m1_t sumf2 = __riscv_vfmv_v_f_f32m1(0.0, vl / 4);
vfloat32m1_t sumf3 = __riscv_vfmv_v_f_f32m1(0.0, vl / 4);
for (int l = 0; l < nb; l++) {
const vint8m4_t rhs_raw_vec = __riscv_vle8_v_i8m4((const int8_t *)b_ptr[l].qs, vl * 4);
const vint8m4_t rhs_vec_lo = __riscv_vsra_vx_i8m4(__riscv_vsll_vx_i8m4(rhs_raw_vec, 4, vl * 4), 4, vl * 4);
const vint8m4_t rhs_vec_hi = __riscv_vsra_vx_i8m4(rhs_raw_vec, 4, vl * 4);
const vint8m2_t rhs_vec_lo_0 = __riscv_vget_v_i8m4_i8m2(rhs_vec_lo, 0);
const vint8m2_t rhs_vec_lo_1 = __riscv_vget_v_i8m4_i8m2(rhs_vec_lo, 1);
const vint8m2_t rhs_vec_hi_0 = __riscv_vget_v_i8m4_i8m2(rhs_vec_hi, 0);
const vint8m2_t rhs_vec_hi_1 = __riscv_vget_v_i8m4_i8m2(rhs_vec_hi, 1);
const float a_scales[4] = {
GGML_FP16_TO_FP32(a_ptr[l].d[0]),
GGML_FP16_TO_FP32(a_ptr[l].d[1]),
GGML_FP16_TO_FP32(a_ptr[l].d[2]),
GGML_FP16_TO_FP32(a_ptr[l].d[3])
};
const float b_scales[8] = {
GGML_FP16_TO_FP32(b_ptr[l].d[0]),
GGML_FP16_TO_FP32(b_ptr[l].d[1]),
GGML_FP16_TO_FP32(b_ptr[l].d[2]),
GGML_FP16_TO_FP32(b_ptr[l].d[3]),
GGML_FP16_TO_FP32(b_ptr[l].d[4]),
GGML_FP16_TO_FP32(b_ptr[l].d[5]),
GGML_FP16_TO_FP32(b_ptr[l].d[6]),
GGML_FP16_TO_FP32(b_ptr[l].d[7])
};
const vfloat32m1_t b_scales_vec = __riscv_vle32_v_f32m1(b_scales, vl / 4);
const int64_t A0 = *(const int64_t *)&a_ptr[l].qs[0];
const int64_t A4 = *(const int64_t *)&a_ptr[l].qs[32];
const int64_t A8 = *(const int64_t *)&a_ptr[l].qs[64];
const int64_t Ac = *(const int64_t *)&a_ptr[l].qs[96];
__asm__ __volatile__("" ::: "memory");
vint16m4_t sumi_l0;
{
const vint8m2_t lhs_0_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A0, vl / 4));
const vint8m2_t lhs_1_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A4, vl / 4));
const vint8m2_t lhs_2_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A8, vl / 4));
const vint8m2_t lhs_3_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(Ac, vl / 4));
const vint16m4_t sumi_lo_0 = __riscv_vwmul_vv_i16m4(rhs_vec_lo_0, lhs_0_8, vl * 2);
const vint16m4_t sumi_lo_1 = __riscv_vwmacc_vv_i16m4(sumi_lo_0, rhs_vec_lo_1, lhs_1_8, vl * 2);
const vint16m4_t sumi_hi_0 = __riscv_vwmacc_vv_i16m4(sumi_lo_1, rhs_vec_hi_0, lhs_2_8, vl * 2);
const vint16m4_t sumi_hi_m = __riscv_vwmacc_vv_i16m4(sumi_hi_0, rhs_vec_hi_1, lhs_3_8, vl * 2);
sumi_l0 = sumi_hi_m;
}
{
const vuint32m4_t sumi_i32 = __riscv_vreinterpret_v_i32m4_u32m4(__riscv_vreinterpret_v_i16m4_i32m4(sumi_l0));
const vuint16m2_t sumi_h2_0 = __riscv_vnsrl_wx_u16m2(sumi_i32, 0, vl);
const vuint16m2_t sumi_h2_1 = __riscv_vnsrl_wx_u16m2(sumi_i32, 16, vl);
const vuint16m2_t sumi_h2 = __riscv_vadd_vv_u16m2(sumi_h2_0, sumi_h2_1, vl);
const vuint32m2_t sumi_h2_i32 = __riscv_vreinterpret_v_u16m2_u32m2(sumi_h2);
const vuint16m1_t sumi_h4_0 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 0, vl / 2);
const vuint16m1_t sumi_h4_1 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 16, vl / 2);
const vuint16m1_t sumi_h4 = __riscv_vadd_vv_u16m1(sumi_h4_0, sumi_h4_1, vl / 2);
const vuint32m1_t sumi_h4_i32 = __riscv_vreinterpret_v_u16m1_u32m1(sumi_h4);
const vint16mf2_t sumi_h8_0 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 0, vl / 4));
const vint16mf2_t sumi_h8_1 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 16, vl / 4));
const vint32m1_t sumi_h8 = __riscv_vwadd_vv_i32m1(sumi_h8_0, sumi_h8_1, vl / 4);
const vfloat32m1_t facc = __riscv_vfcvt_f_x_v_f32m1(sumi_h8, vl / 4);
const vfloat32m1_t tmp1 = __riscv_vfmul_vf_f32m1(facc, a_scales[0], vl / 4);
sumf0 = __riscv_vfmacc_vv_f32m1(sumf0, tmp1, b_scales_vec, vl / 4);
}
const int64_t A1 = *(const int64_t *)&a_ptr[l].qs[8];
const int64_t A5 = *(const int64_t *)&a_ptr[l].qs[40];
const int64_t A9 = *(const int64_t *)&a_ptr[l].qs[72];
const int64_t Ad = *(const int64_t *)&a_ptr[l].qs[104];
__asm__ __volatile__("" ::: "memory");
vint16m4_t sumi_l1;
{
const vint8m2_t lhs_0_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A1, vl / 4));
const vint8m2_t lhs_1_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A5, vl / 4));
const vint8m2_t lhs_2_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A9, vl / 4));
const vint8m2_t lhs_3_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(Ad, vl / 4));
const vint16m4_t sumi_lo_0 = __riscv_vwmul_vv_i16m4(rhs_vec_lo_0, lhs_0_8, vl * 2);
const vint16m4_t sumi_lo_1 = __riscv_vwmacc_vv_i16m4(sumi_lo_0, rhs_vec_lo_1, lhs_1_8, vl * 2);
const vint16m4_t sumi_hi_0 = __riscv_vwmacc_vv_i16m4(sumi_lo_1, rhs_vec_hi_0, lhs_2_8, vl * 2);
const vint16m4_t sumi_hi_m = __riscv_vwmacc_vv_i16m4(sumi_hi_0, rhs_vec_hi_1, lhs_3_8, vl * 2);
sumi_l1 = sumi_hi_m;
}
{
const vuint32m4_t sumi_i32 = __riscv_vreinterpret_v_i32m4_u32m4(__riscv_vreinterpret_v_i16m4_i32m4(sumi_l1));
const vuint16m2_t sumi_h2_0 = __riscv_vnsrl_wx_u16m2(sumi_i32, 0, vl);
const vuint16m2_t sumi_h2_1 = __riscv_vnsrl_wx_u16m2(sumi_i32, 16, vl);
const vuint16m2_t sumi_h2 = __riscv_vadd_vv_u16m2(sumi_h2_0, sumi_h2_1, vl);
const vuint32m2_t sumi_h2_i32 = __riscv_vreinterpret_v_u16m2_u32m2(sumi_h2);
const vuint16m1_t sumi_h4_0 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 0, vl / 2);
const vuint16m1_t sumi_h4_1 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 16, vl / 2);
const vuint16m1_t sumi_h4 = __riscv_vadd_vv_u16m1(sumi_h4_0, sumi_h4_1, vl / 2);
const vuint32m1_t sumi_h4_i32 = __riscv_vreinterpret_v_u16m1_u32m1(sumi_h4);
const vint16mf2_t sumi_h8_0 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 0, vl / 4));
const vint16mf2_t sumi_h8_1 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 16, vl / 4));
const vint32m1_t sumi_h8 = __riscv_vwadd_vv_i32m1(sumi_h8_0, sumi_h8_1, vl / 4);
const vfloat32m1_t facc = __riscv_vfcvt_f_x_v_f32m1(sumi_h8, vl / 4);
const vfloat32m1_t tmp1 = __riscv_vfmul_vf_f32m1(facc, a_scales[1], vl / 4);
sumf1 = __riscv_vfmacc_vv_f32m1(sumf1, tmp1, b_scales_vec, vl / 4);
}
const int64_t A2 = *(const int64_t *)&a_ptr[l].qs[16];
const int64_t A6 = *(const int64_t *)&a_ptr[l].qs[48];
const int64_t Aa = *(const int64_t *)&a_ptr[l].qs[80];
const int64_t Ae = *(const int64_t *)&a_ptr[l].qs[112];
__asm__ __volatile__("" ::: "memory");
vint16m4_t sumi_l2;
{
const vint8m2_t lhs_0_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A2, vl / 4));
const vint8m2_t lhs_1_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A6, vl / 4));
const vint8m2_t lhs_2_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(Aa, vl / 4));
const vint8m2_t lhs_3_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(Ae, vl / 4));
const vint16m4_t sumi_lo_0 = __riscv_vwmul_vv_i16m4(rhs_vec_lo_0, lhs_0_8, vl * 2);
const vint16m4_t sumi_lo_1 = __riscv_vwmacc_vv_i16m4(sumi_lo_0, rhs_vec_lo_1, lhs_1_8, vl * 2);
const vint16m4_t sumi_hi_0 = __riscv_vwmacc_vv_i16m4(sumi_lo_1, rhs_vec_hi_0, lhs_2_8, vl * 2);
const vint16m4_t sumi_hi_m = __riscv_vwmacc_vv_i16m4(sumi_hi_0, rhs_vec_hi_1, lhs_3_8, vl * 2);
sumi_l2 = sumi_hi_m;
}
{
const vuint32m4_t sumi_i32 = __riscv_vreinterpret_v_i32m4_u32m4(__riscv_vreinterpret_v_i16m4_i32m4(sumi_l2));
const vuint16m2_t sumi_h2_0 = __riscv_vnsrl_wx_u16m2(sumi_i32, 0, vl);
const vuint16m2_t sumi_h2_1 = __riscv_vnsrl_wx_u16m2(sumi_i32, 16, vl);
const vuint16m2_t sumi_h2 = __riscv_vadd_vv_u16m2(sumi_h2_0, sumi_h2_1, vl);
const vuint32m2_t sumi_h2_i32 = __riscv_vreinterpret_v_u16m2_u32m2(sumi_h2);
const vuint16m1_t sumi_h4_0 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 0, vl / 2);
const vuint16m1_t sumi_h4_1 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 16, vl / 2);
const vuint16m1_t sumi_h4 = __riscv_vadd_vv_u16m1(sumi_h4_0, sumi_h4_1, vl / 2);
const vuint32m1_t sumi_h4_i32 = __riscv_vreinterpret_v_u16m1_u32m1(sumi_h4);
const vint16mf2_t sumi_h8_0 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 0, vl / 4));
const vint16mf2_t sumi_h8_1 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 16, vl / 4));
const vint32m1_t sumi_h8 = __riscv_vwadd_vv_i32m1(sumi_h8_0, sumi_h8_1, vl / 4);
const vfloat32m1_t facc = __riscv_vfcvt_f_x_v_f32m1(sumi_h8, vl / 4);
const vfloat32m1_t tmp1 = __riscv_vfmul_vf_f32m1(facc, a_scales[2], vl / 4);
sumf2 = __riscv_vfmacc_vv_f32m1(sumf2, tmp1, b_scales_vec, vl / 4);
}
const int64_t A3 = *(const int64_t *)&a_ptr[l].qs[24];
const int64_t A7 = *(const int64_t *)&a_ptr[l].qs[56];
const int64_t Ab = *(const int64_t *)&a_ptr[l].qs[88];
const int64_t Af = *(const int64_t *)&a_ptr[l].qs[120];
__asm__ __volatile__("" ::: "memory");
vint16m4_t sumi_l3;
{
const vint8m2_t lhs_0_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A3, vl / 4));
const vint8m2_t lhs_1_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(A7, vl / 4));
const vint8m2_t lhs_2_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(Ab, vl / 4));
const vint8m2_t lhs_3_8 =__riscv_vreinterpret_v_i64m2_i8m2(__riscv_vmv_v_x_i64m2(Af, vl / 4));
const vint16m4_t sumi_lo_0 = __riscv_vwmul_vv_i16m4(rhs_vec_lo_0, lhs_0_8, vl * 2);
const vint16m4_t sumi_lo_1 = __riscv_vwmacc_vv_i16m4(sumi_lo_0, rhs_vec_lo_1, lhs_1_8, vl * 2);
const vint16m4_t sumi_hi_0 = __riscv_vwmacc_vv_i16m4(sumi_lo_1, rhs_vec_hi_0, lhs_2_8, vl * 2);
const vint16m4_t sumi_hi_m = __riscv_vwmacc_vv_i16m4(sumi_hi_0, rhs_vec_hi_1, lhs_3_8, vl * 2);
sumi_l3 = sumi_hi_m;
}
{
const vuint32m4_t sumi_i32 = __riscv_vreinterpret_v_i32m4_u32m4(__riscv_vreinterpret_v_i16m4_i32m4(sumi_l3));
const vuint16m2_t sumi_h2_0 = __riscv_vnsrl_wx_u16m2(sumi_i32, 0, vl);
const vuint16m2_t sumi_h2_1 = __riscv_vnsrl_wx_u16m2(sumi_i32, 16, vl);
const vuint16m2_t sumi_h2 = __riscv_vadd_vv_u16m2(sumi_h2_0, sumi_h2_1, vl);
const vuint32m2_t sumi_h2_i32 = __riscv_vreinterpret_v_u16m2_u32m2(sumi_h2);
const vuint16m1_t sumi_h4_0 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 0, vl / 2);
const vuint16m1_t sumi_h4_1 = __riscv_vnsrl_wx_u16m1(sumi_h2_i32, 16, vl / 2);
const vuint16m1_t sumi_h4 = __riscv_vadd_vv_u16m1(sumi_h4_0, sumi_h4_1, vl / 2);
const vuint32m1_t sumi_h4_i32 = __riscv_vreinterpret_v_u16m1_u32m1(sumi_h4);
const vint16mf2_t sumi_h8_0 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 0, vl / 4));
const vint16mf2_t sumi_h8_1 = __riscv_vreinterpret_v_u16mf2_i16mf2(__riscv_vnsrl_wx_u16mf2(sumi_h4_i32, 16, vl / 4));
const vint32m1_t sumi_h8 = __riscv_vwadd_vv_i32m1(sumi_h8_0, sumi_h8_1, vl / 4);
const vfloat32m1_t facc = __riscv_vfcvt_f_x_v_f32m1(sumi_h8, vl / 4);
const vfloat32m1_t tmp1 = __riscv_vfmul_vf_f32m1(facc, a_scales[3], vl / 4);
sumf3 = __riscv_vfmacc_vv_f32m1(sumf3, tmp1, b_scales_vec, vl / 4);
}
}
__riscv_vse32_v_f32m1(&s[(y * 4 + 0) * bs + x * ncols_interleaved], sumf0, vl / 4);
__riscv_vse32_v_f32m1(&s[(y * 4 + 1) * bs + x * ncols_interleaved], sumf1, vl / 4);
__riscv_vse32_v_f32m1(&s[(y * 4 + 2) * bs + x * ncols_interleaved], sumf2, vl / 4);
__riscv_vse32_v_f32m1(&s[(y * 4 + 3) * bs + x * ncols_interleaved], sumf3, vl / 4);
}
}
return;
}
#endif
float sumf[4][8];
int sumi;
for (int y = 0; y < nr / 4; y++) {
const block_q8_0x4 * a_ptr = (const block_q8_0x4 *) vy + (y * nb);
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_0x8 * b_ptr = (const block_q4_0x8 *) vx + (x * nb);
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) sumf[m][j] = 0.0;
}
for (int l = 0; l < nb; l++) {
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] << 4);
const int v1 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0xF0);
sumi += ((v0 * a_ptr[l].qs[k * 4 * blocklen + m * blocklen + i]) +
(v1 * a_ptr[l].qs[k * 4 * blocklen + m * blocklen + i + qk / 2 * 4])) >> 4;
}
sumf[m][j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * GGML_FP16_TO_FP32(a_ptr[l].d[m]);
}
}
}
}
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++)
s[(y * 4 + m) * bs + x * ncols_interleaved + j] = sumf[m][j];
}
}
}
}
static void ggml_gemm_q4_K_8x8_q8_K(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK_K;
const int nb = n / qk;
const int ncols_interleaved = 8;
const int blocklen = 8;
static const uint32_t kmask1 = 0x3f3f3f3f;
static const uint32_t kmask2 = 0x0f0f0f0f;
static const uint32_t kmask3 = 0x03030303;
assert (n % qk == 0);
assert (nr % 4 == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if defined(__AVX2__) || defined(__AVX512F__)
const block_q4_Kx8 * b_ptr_start = (const block_q4_Kx8 * ) vx;
const block_q8_Kx4 * a_ptr_start = (const block_q8_Kx4 * ) vy;
int64_t b_nb = n / QK_K;
int64_t y = 0;
const __m256i m4b = _mm256_set1_epi8(0x0F);
__m256i requiredOrder = _mm256_set_epi32(3, 2, 1, 0, 7, 6, 5, 4);
int64_t xstart = 0;
int anr = nr - nr % 16;;
#ifdef __AVX512F__
int anc = nc - nc % 16;
const __m512i m4bexpanded = _mm512_set1_epi8(0x0F);
for (; y < anr / 4; y += 4) {
const block_q8_Kx4 * a_ptrs[4];
a_ptrs[0] = a_ptr_start + (y * nb);
for (int i = 0; i < 3; ++i) {
a_ptrs[i + 1] = a_ptrs[i] + nb;
}
for (int64_t x = 0; x < anc / 8; x += 2) {
const block_q4_Kx8 * b_ptr_0 = b_ptr_start + ((x) * b_nb);
const block_q4_Kx8 * b_ptr_1 = b_ptr_start + ((x + 1) * b_nb);
__m512 acc_rows[16];
for (int i = 0; i < 16; i++) {
acc_rows[i] = _mm512_setzero_ps();
}
__m512 acc_min_rows[16];
for (int i = 0; i < 16; i++) {
acc_min_rows[i] = _mm512_setzero_ps();
}
for (int64_t b = 0; b < nb; b++) {
const __m512 col_scale_f32 = GGML_F32Cx8x2_LOAD(b_ptr_0[b].d, b_ptr_1[b].d);
const __m512 col_dmin_f32 = GGML_F32Cx8x2_LOAD(b_ptr_0[b].dmin, b_ptr_1[b].dmin);
for (int sb = 0; sb < QK_K / 64; sb++) {
const __m256i rhs_raw_mat_0123_0 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + sb * 256));
const __m256i rhs_raw_mat_4567_0 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 32 + sb * 256));
const __m256i rhs_raw_mat_0123_1 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 64 + sb * 256));
const __m256i rhs_raw_mat_4567_1 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 96 + sb * 256));
const __m256i rhs_raw_mat_0123_2 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 128 + sb * 256));
const __m256i rhs_raw_mat_4567_2 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 160 + sb * 256));
const __m256i rhs_raw_mat_0123_3 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 192 + sb * 256));
const __m256i rhs_raw_mat_4567_3 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 224 + sb * 256));
const __m256i rhs_raw_mat_89AB_0 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + sb * 256));
const __m256i rhs_raw_mat_CDEF_0 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 32 + sb * 256));
const __m256i rhs_raw_mat_89AB_1 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 64 + sb * 256));
const __m256i rhs_raw_mat_CDEF_1 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 96 + sb * 256));
const __m256i rhs_raw_mat_89AB_2 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 128 + sb * 256));
const __m256i rhs_raw_mat_CDEF_2 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 160 + sb * 256));
const __m256i rhs_raw_mat_89AB_3 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 192 + sb * 256));
const __m256i rhs_raw_mat_CDEF_3 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 224 + sb * 256));
const __m256i rhs_raw_mat_0145_0 = _mm256_blend_epi32(rhs_raw_mat_0123_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_0, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_0, requiredOrder), rhs_raw_mat_4567_0, 240);
const __m256i rhs_raw_mat_0145_1 = _mm256_blend_epi32(rhs_raw_mat_0123_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_1, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_1, requiredOrder), rhs_raw_mat_4567_1, 240);
const __m256i rhs_raw_mat_0145_2 = _mm256_blend_epi32(rhs_raw_mat_0123_2, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_2, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_2 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_2, requiredOrder), rhs_raw_mat_4567_2, 240);
const __m256i rhs_raw_mat_0145_3 = _mm256_blend_epi32(rhs_raw_mat_0123_3, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_3, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_3 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_3, requiredOrder), rhs_raw_mat_4567_3, 240);
const __m256i rhs_raw_mat_89CD_0 = _mm256_blend_epi32(rhs_raw_mat_89AB_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_0, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_0, requiredOrder), rhs_raw_mat_CDEF_0, 240);
const __m256i rhs_raw_mat_89CD_1 = _mm256_blend_epi32(rhs_raw_mat_89AB_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_1, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_1, requiredOrder), rhs_raw_mat_CDEF_1, 240);
const __m256i rhs_raw_mat_89CD_2 = _mm256_blend_epi32(rhs_raw_mat_89AB_2, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_2, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_2 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_2, requiredOrder), rhs_raw_mat_CDEF_2, 240);
const __m256i rhs_raw_mat_89CD_3 = _mm256_blend_epi32(rhs_raw_mat_89AB_3, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_3, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_3 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_3, requiredOrder), rhs_raw_mat_CDEF_3, 240);
const __m512i rhs_raw_mat_014589CD_0 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_0), rhs_raw_mat_89CD_0, 1);
const __m512i rhs_raw_mat_2367ABEF_0 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_0), rhs_raw_mat_ABEF_0, 1);
const __m512i rhs_raw_mat_014589CD_1 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_1), rhs_raw_mat_89CD_1, 1);
const __m512i rhs_raw_mat_2367ABEF_1 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_1), rhs_raw_mat_ABEF_1, 1);
const __m512i rhs_raw_mat_014589CD_2 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_2), rhs_raw_mat_89CD_2, 1);
const __m512i rhs_raw_mat_2367ABEF_2 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_2), rhs_raw_mat_ABEF_2, 1);
const __m512i rhs_raw_mat_014589CD_3 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_3), rhs_raw_mat_89CD_3, 1);
const __m512i rhs_raw_mat_2367ABEF_3 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_3), rhs_raw_mat_ABEF_3, 1);
const __m512i rhs_mat_014589CD_00 = _mm512_and_si512(rhs_raw_mat_014589CD_0, m4bexpanded);
const __m512i rhs_mat_2367ABEF_00 = _mm512_and_si512(rhs_raw_mat_2367ABEF_0, m4bexpanded);
const __m512i rhs_mat_014589CD_01 = _mm512_and_si512(rhs_raw_mat_014589CD_1, m4bexpanded);
const __m512i rhs_mat_2367ABEF_01 = _mm512_and_si512(rhs_raw_mat_2367ABEF_1, m4bexpanded);
const __m512i rhs_mat_014589CD_02 = _mm512_and_si512(rhs_raw_mat_014589CD_2, m4bexpanded);
const __m512i rhs_mat_2367ABEF_02 = _mm512_and_si512(rhs_raw_mat_2367ABEF_2, m4bexpanded);
const __m512i rhs_mat_014589CD_03 = _mm512_and_si512(rhs_raw_mat_014589CD_3, m4bexpanded);
const __m512i rhs_mat_2367ABEF_03 = _mm512_and_si512(rhs_raw_mat_2367ABEF_3, m4bexpanded);
const __m512i rhs_mat_014589CD_10 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_0, 4), m4bexpanded);
const __m512i rhs_mat_2367ABEF_10 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_0, 4), m4bexpanded);
const __m512i rhs_mat_014589CD_11 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_1, 4), m4bexpanded);
const __m512i rhs_mat_2367ABEF_11 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_1, 4), m4bexpanded);
const __m512i rhs_mat_014589CD_12 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_2, 4), m4bexpanded);
const __m512i rhs_mat_2367ABEF_12 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_2, 4), m4bexpanded);
const __m512i rhs_mat_014589CD_13 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_3, 4), m4bexpanded);
const __m512i rhs_mat_2367ABEF_13 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_3, 4), m4bexpanded);
const __m512i rhs_mat_014589CD_00_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_00, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_00_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_00, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_01_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_01, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_01_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_01, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_02_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_02, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_02_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_02, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_03_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_03, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_03_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_03, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_10_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_10, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_10_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_10, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_11_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_11, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_11_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_11, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_12_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_12, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_12_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_12, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_13_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_13, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_13_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_13, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_00_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_00, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_00_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_00, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_01_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_01, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_01_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_01, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_02_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_02, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_02_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_02, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_03_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_03, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_03_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_03, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_10_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_10, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_10_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_10, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_11_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_11, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_11_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_11, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_12_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_12, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_12_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_12, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_13_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_13, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_13_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_13, (_MM_PERM_ENUM)221);
uint32_t utmp_00[4], utmp_01[4], utmp_10[4], utmp_11[4];
memcpy(utmp_00, b_ptr_0[b].scales + 24 * sb, 12);
utmp_00[3] = ((utmp_00[2] >> 4) & kmask2) | (((utmp_00[1] >> 6) & kmask3) << 4);
const uint32_t uaux_00 = utmp_00[1] & kmask1;
utmp_00[1] = (utmp_00[2] & kmask2) | (((utmp_00[0] >> 6) & kmask3) << 4);
utmp_00[2] = uaux_00;
utmp_00[0] &= kmask1;
memcpy(utmp_01, b_ptr_0[b].scales + 12 + sb * 24, 12);
utmp_01[3] = ((utmp_01[2] >> 4) & kmask2) | (((utmp_01[1] >> 6) & kmask3) << 4);
const uint32_t uaux_01 = utmp_01[1] & kmask1;
utmp_01[1] = (utmp_01[2] & kmask2) | (((utmp_01[0] >> 6) & kmask3) << 4);
utmp_01[2] = uaux_01;
utmp_01[0] &= kmask1;
memcpy(utmp_10, b_ptr_1[b].scales + sb * 24, 12);
utmp_10[3] = ((utmp_10[2] >> 4) & kmask2) | (((utmp_10[1] >> 6) & kmask3) << 4);
const uint32_t uaux_10 = utmp_10[1] & kmask1;
utmp_10[1] = (utmp_10[2] & kmask2) | (((utmp_10[0] >> 6) & kmask3) << 4);
utmp_10[2] = uaux_10;
utmp_10[0] &= kmask1;
memcpy(utmp_11, b_ptr_1[b].scales + 12 + sb * 24, 12);
utmp_11[3] = ((utmp_11[2] >> 4) & kmask2) | (((utmp_11[1] >> 6) & kmask3) << 4);
const uint32_t uaux_11 = utmp_11[1] & kmask1;
utmp_11[1] = (utmp_11[2] & kmask2) | (((utmp_11[0] >> 6) & kmask3) << 4);
utmp_11[2] = uaux_11;
utmp_11[0] &= kmask1;
const __m256i mins_and_scales_0 = _mm256_set_epi32(utmp_10[3], utmp_10[2], utmp_10[1], utmp_10[0], utmp_00[3], utmp_00[2], utmp_00[1], utmp_00[0]);
const __m512i scales_0 = _mm512_cvtepu8_epi16(_mm256_unpacklo_epi8(mins_and_scales_0, mins_and_scales_0));
const __m256i mins_and_scales_1 = _mm256_set_epi32(utmp_11[3], utmp_11[2], utmp_11[1], utmp_11[0], utmp_01[3], utmp_01[2], utmp_01[1], utmp_01[0]);
const __m512i scales_1 = _mm512_cvtepu8_epi16(_mm256_unpacklo_epi8(mins_and_scales_1, mins_and_scales_1));
const __m512i mins_01 = _mm512_cvtepu8_epi16(_mm256_unpacklo_epi8(_mm256_shuffle_epi32(mins_and_scales_0, 78), _mm256_shuffle_epi32(mins_and_scales_1, 78)));
const __m512i scale_014589CD_0 = _mm512_shuffle_epi32(scales_0, (_MM_PERM_ENUM)68);
const __m512i scale_2367ABEF_0 = _mm512_shuffle_epi32(scales_0, (_MM_PERM_ENUM)238);
const __m512i scale_014589CD_1 = _mm512_shuffle_epi32(scales_1, (_MM_PERM_ENUM)68);
const __m512i scale_2367ABEF_1 = _mm512_shuffle_epi32(scales_1, (_MM_PERM_ENUM)238);
for (int rp = 0; rp < 4; rp++) {
__m256i lhs_mat_ymm_0123_00 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 256 * sb)));
__m256i lhs_mat_ymm_01_00 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_00, lhs_mat_ymm_0123_00, 0);
__m256i lhs_mat_ymm_23_00 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_00, lhs_mat_ymm_0123_00, 17);
__m256i lhs_mat_ymm_0123_01 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 32 + 256 * sb)));
__m256i lhs_mat_ymm_01_01 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_01, lhs_mat_ymm_0123_01, 0);
__m256i lhs_mat_ymm_23_01 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_01, lhs_mat_ymm_0123_01, 17);
__m256i lhs_mat_ymm_0123_02 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 64 + 256 * sb)));
__m256i lhs_mat_ymm_01_02 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_02, lhs_mat_ymm_0123_02, 0);
__m256i lhs_mat_ymm_23_02 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_02, lhs_mat_ymm_0123_02, 17);
__m256i lhs_mat_ymm_0123_03 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 96 + 256 * sb)));
__m256i lhs_mat_ymm_01_03 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_03, lhs_mat_ymm_0123_03, 0);
__m256i lhs_mat_ymm_23_03 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_03, lhs_mat_ymm_0123_03, 17);
__m256i lhs_mat_ymm_0123_10 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 128 + 256 * sb)));
__m256i lhs_mat_ymm_01_10 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_10, lhs_mat_ymm_0123_10, 0);
__m256i lhs_mat_ymm_23_10 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_10, lhs_mat_ymm_0123_10, 17);
__m256i lhs_mat_ymm_0123_11 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 160 + 256 * sb)));
__m256i lhs_mat_ymm_01_11 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_11, lhs_mat_ymm_0123_11, 0);
__m256i lhs_mat_ymm_23_11 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_11, lhs_mat_ymm_0123_11, 17);
__m256i lhs_mat_ymm_0123_12 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 192 + 256 * sb)));
__m256i lhs_mat_ymm_01_12 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_12, lhs_mat_ymm_0123_12, 0);
__m256i lhs_mat_ymm_23_12 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_12, lhs_mat_ymm_0123_12, 17);
__m256i lhs_mat_ymm_0123_13 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 224 + 256 * sb)));
__m256i lhs_mat_ymm_01_13 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_13, lhs_mat_ymm_0123_13, 0);
__m256i lhs_mat_ymm_23_13 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_13, lhs_mat_ymm_0123_13, 17);
__m512i lhs_mat_01_00 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_00), lhs_mat_ymm_01_00, 1);
__m512i lhs_mat_23_00 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_00), lhs_mat_ymm_23_00, 1);
__m512i lhs_mat_01_01 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_01), lhs_mat_ymm_01_01, 1);
__m512i lhs_mat_23_01 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_01), lhs_mat_ymm_23_01, 1);
__m512i lhs_mat_01_02 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_02), lhs_mat_ymm_01_02, 1);
__m512i lhs_mat_23_02 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_02), lhs_mat_ymm_23_02, 1);
__m512i lhs_mat_01_03 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_03), lhs_mat_ymm_01_03, 1);
__m512i lhs_mat_23_03 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_03), lhs_mat_ymm_23_03, 1);
__m512i lhs_mat_01_10 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_10), lhs_mat_ymm_01_10, 1);
__m512i lhs_mat_23_10 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_10), lhs_mat_ymm_23_10, 1);
__m512i lhs_mat_01_11 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_11), lhs_mat_ymm_01_11, 1);
__m512i lhs_mat_23_11 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_11), lhs_mat_ymm_23_11, 1);
__m512i lhs_mat_01_12 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_12), lhs_mat_ymm_01_12, 1);
__m512i lhs_mat_23_12 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_12), lhs_mat_ymm_23_12, 1);
__m512i lhs_mat_01_13 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_13), lhs_mat_ymm_01_13, 1);
__m512i lhs_mat_23_13 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_13), lhs_mat_ymm_23_13, 1);
__m256i lhs_bsums_0123_01 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].bsums + 16 * sb)));
__m256i lhs_bsums_hsum_ymm_0123_01 = _mm256_castsi128_si256(_mm_hadd_epi16(_mm256_castsi256_si128(lhs_bsums_0123_01), _mm256_extractf128_si256(lhs_bsums_0123_01, 1)));
lhs_bsums_hsum_ymm_0123_01 = _mm256_permute2x128_si256(lhs_bsums_hsum_ymm_0123_01, lhs_bsums_hsum_ymm_0123_01, 0);
__m512i lhs_bsums_hsum_0123_01 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_bsums_hsum_ymm_0123_01), lhs_bsums_hsum_ymm_0123_01, 1);
const __m512i lhs_mat_01_00_sp1 = _mm512_shuffle_epi32(lhs_mat_01_00, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_00_sp1 = _mm512_shuffle_epi32(lhs_mat_23_00, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_01_sp1 = _mm512_shuffle_epi32(lhs_mat_01_01, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_01_sp1 = _mm512_shuffle_epi32(lhs_mat_23_01, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_02_sp1 = _mm512_shuffle_epi32(lhs_mat_01_02, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_02_sp1 = _mm512_shuffle_epi32(lhs_mat_23_02, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_03_sp1 = _mm512_shuffle_epi32(lhs_mat_01_03, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_03_sp1 = _mm512_shuffle_epi32(lhs_mat_23_03, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_10_sp1 = _mm512_shuffle_epi32(lhs_mat_01_10, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_10_sp1 = _mm512_shuffle_epi32(lhs_mat_23_10, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_11_sp1 = _mm512_shuffle_epi32(lhs_mat_01_11, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_11_sp1 = _mm512_shuffle_epi32(lhs_mat_23_11, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_12_sp1 = _mm512_shuffle_epi32(lhs_mat_01_12, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_12_sp1 = _mm512_shuffle_epi32(lhs_mat_23_12, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_13_sp1 = _mm512_shuffle_epi32(lhs_mat_01_13, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_13_sp1 = _mm512_shuffle_epi32(lhs_mat_23_13, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_00_sp2 = _mm512_shuffle_epi32(lhs_mat_01_00, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_00_sp2 = _mm512_shuffle_epi32(lhs_mat_23_00, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_01_sp2 = _mm512_shuffle_epi32(lhs_mat_01_01, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_01_sp2 = _mm512_shuffle_epi32(lhs_mat_23_01, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_02_sp2 = _mm512_shuffle_epi32(lhs_mat_01_02, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_02_sp2 = _mm512_shuffle_epi32(lhs_mat_23_02, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_03_sp2 = _mm512_shuffle_epi32(lhs_mat_01_03, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_03_sp2 = _mm512_shuffle_epi32(lhs_mat_23_03, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_10_sp2 = _mm512_shuffle_epi32(lhs_mat_01_10, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_10_sp2 = _mm512_shuffle_epi32(lhs_mat_23_10, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_11_sp2 = _mm512_shuffle_epi32(lhs_mat_01_11, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_11_sp2 = _mm512_shuffle_epi32(lhs_mat_23_11, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_12_sp2 = _mm512_shuffle_epi32(lhs_mat_01_12, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_12_sp2 = _mm512_shuffle_epi32(lhs_mat_23_12, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_13_sp2 = _mm512_shuffle_epi32(lhs_mat_01_13, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_13_sp2 = _mm512_shuffle_epi32(lhs_mat_23_13, (_MM_PERM_ENUM)245);
__m512i iacc_mat_00_0_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_03_sp1, lhs_mat_01_03_sp1), _mm512_maddubs_epi16(rhs_mat_014589CD_02_sp1, lhs_mat_01_02_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_01_sp1, lhs_mat_01_01_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_00_sp1, lhs_mat_01_00_sp1));
__m512i iacc_mat_01_0_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_03_sp1, lhs_mat_01_03_sp1), _mm512_maddubs_epi16(rhs_mat_2367ABEF_02_sp1, lhs_mat_01_02_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_01_sp1, lhs_mat_01_01_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_00_sp1, lhs_mat_01_00_sp1));
__m512i iacc_mat_10_0_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_03_sp1, lhs_mat_23_03_sp1), _mm512_maddubs_epi16(rhs_mat_014589CD_02_sp1, lhs_mat_23_02_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_01_sp1, lhs_mat_23_01_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_00_sp1, lhs_mat_23_00_sp1));
__m512i iacc_mat_11_0_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_03_sp1, lhs_mat_23_03_sp1), _mm512_maddubs_epi16(rhs_mat_2367ABEF_02_sp1, lhs_mat_23_02_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_01_sp1, lhs_mat_23_01_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_00_sp1, lhs_mat_23_00_sp1));
__m512i iacc_mat_00_1_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_13_sp1, lhs_mat_01_13_sp1), _mm512_maddubs_epi16(rhs_mat_014589CD_12_sp1, lhs_mat_01_12_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_11_sp1, lhs_mat_01_11_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_10_sp1, lhs_mat_01_10_sp1));
__m512i iacc_mat_01_1_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_13_sp1, lhs_mat_01_13_sp1), _mm512_maddubs_epi16(rhs_mat_2367ABEF_12_sp1, lhs_mat_01_12_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_11_sp1, lhs_mat_01_11_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_10_sp1, lhs_mat_01_10_sp1));
__m512i iacc_mat_10_1_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_13_sp1, lhs_mat_23_13_sp1), _mm512_maddubs_epi16(rhs_mat_014589CD_12_sp1, lhs_mat_23_12_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_11_sp1, lhs_mat_23_11_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_10_sp1, lhs_mat_23_10_sp1));
__m512i iacc_mat_11_1_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_13_sp1, lhs_mat_23_13_sp1), _mm512_maddubs_epi16(rhs_mat_2367ABEF_12_sp1, lhs_mat_23_12_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_11_sp1, lhs_mat_23_11_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_10_sp1, lhs_mat_23_10_sp1));
__m512i iacc_mat_00_0_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_03_sp2, lhs_mat_01_03_sp2), _mm512_maddubs_epi16(rhs_mat_014589CD_02_sp2, lhs_mat_01_02_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_01_sp2, lhs_mat_01_01_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_00_sp2, lhs_mat_01_00_sp2));
__m512i iacc_mat_01_0_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_03_sp2, lhs_mat_01_03_sp2), _mm512_maddubs_epi16(rhs_mat_2367ABEF_02_sp2, lhs_mat_01_02_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_01_sp2, lhs_mat_01_01_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_00_sp2, lhs_mat_01_00_sp2));
__m512i iacc_mat_10_0_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_03_sp2, lhs_mat_23_03_sp2), _mm512_maddubs_epi16(rhs_mat_014589CD_02_sp2, lhs_mat_23_02_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_01_sp2, lhs_mat_23_01_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_00_sp2, lhs_mat_23_00_sp2));
__m512i iacc_mat_11_0_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_03_sp2, lhs_mat_23_03_sp2), _mm512_maddubs_epi16(rhs_mat_2367ABEF_02_sp2, lhs_mat_23_02_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_01_sp2, lhs_mat_23_01_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_00_sp2, lhs_mat_23_00_sp2));
__m512i iacc_mat_00_1_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_13_sp2, lhs_mat_01_13_sp2), _mm512_maddubs_epi16(rhs_mat_014589CD_12_sp2, lhs_mat_01_12_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_11_sp2, lhs_mat_01_11_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_10_sp2, lhs_mat_01_10_sp2));
__m512i iacc_mat_01_1_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_13_sp2, lhs_mat_01_13_sp2), _mm512_maddubs_epi16(rhs_mat_2367ABEF_12_sp2, lhs_mat_01_12_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_11_sp2, lhs_mat_01_11_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_10_sp2, lhs_mat_01_10_sp2));
__m512i iacc_mat_10_1_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_13_sp2, lhs_mat_23_13_sp2), _mm512_maddubs_epi16(rhs_mat_014589CD_12_sp2, lhs_mat_23_12_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_11_sp2, lhs_mat_23_11_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_10_sp2, lhs_mat_23_10_sp2));
__m512i iacc_mat_11_1_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_13_sp2, lhs_mat_23_13_sp2), _mm512_maddubs_epi16(rhs_mat_2367ABEF_12_sp2, lhs_mat_23_12_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_11_sp2, lhs_mat_23_11_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_10_sp2, lhs_mat_23_10_sp2));
__m512i iacc_mat_00_0 = _mm512_add_epi16(iacc_mat_00_0_sp1, iacc_mat_00_0_sp2);
__m512i iacc_mat_01_0 = _mm512_add_epi16(iacc_mat_01_0_sp1, iacc_mat_01_0_sp2);
__m512i iacc_mat_10_0 = _mm512_add_epi16(iacc_mat_10_0_sp1, iacc_mat_10_0_sp2);
__m512i iacc_mat_11_0 = _mm512_add_epi16(iacc_mat_11_0_sp1, iacc_mat_11_0_sp2);
__m512i iacc_mat_00_1 = _mm512_add_epi16(iacc_mat_00_1_sp1, iacc_mat_00_1_sp2);
__m512i iacc_mat_01_1 = _mm512_add_epi16(iacc_mat_01_1_sp1, iacc_mat_01_1_sp2);
__m512i iacc_mat_10_1 = _mm512_add_epi16(iacc_mat_10_1_sp1, iacc_mat_10_1_sp2);
__m512i iacc_mat_11_1 = _mm512_add_epi16(iacc_mat_11_1_sp1, iacc_mat_11_1_sp2);
iacc_mat_00_0 = _mm512_madd_epi16(iacc_mat_00_0, scale_014589CD_0);
iacc_mat_01_0 = _mm512_madd_epi16(iacc_mat_01_0, scale_2367ABEF_0);
iacc_mat_10_0 = _mm512_madd_epi16(iacc_mat_10_0, scale_014589CD_0);
iacc_mat_11_0 = _mm512_madd_epi16(iacc_mat_11_0, scale_2367ABEF_0);
iacc_mat_00_1 = _mm512_madd_epi16(iacc_mat_00_1, scale_014589CD_1);
iacc_mat_01_1 = _mm512_madd_epi16(iacc_mat_01_1, scale_2367ABEF_1);
iacc_mat_10_1 = _mm512_madd_epi16(iacc_mat_10_1, scale_014589CD_1);
iacc_mat_11_1 = _mm512_madd_epi16(iacc_mat_11_1, scale_2367ABEF_1);
__m512i iacc_row_0_0 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_00_0, _mm512_shuffle_epi32(iacc_mat_01_0, (_MM_PERM_ENUM)78));
__m512i iacc_row_1_0 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_00_0, (_MM_PERM_ENUM)78), iacc_mat_01_0);
__m512i iacc_row_2_0 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_10_0, _mm512_shuffle_epi32(iacc_mat_11_0, (_MM_PERM_ENUM)78));
__m512i iacc_row_3_0 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_10_0, (_MM_PERM_ENUM)78), iacc_mat_11_0);
__m512i iacc_row_0_1 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_00_1, _mm512_shuffle_epi32(iacc_mat_01_1, (_MM_PERM_ENUM)78));
__m512i iacc_row_1_1 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_00_1, (_MM_PERM_ENUM)78), iacc_mat_01_1);
__m512i iacc_row_2_1 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_10_1, _mm512_shuffle_epi32(iacc_mat_11_1, (_MM_PERM_ENUM)78));
__m512i iacc_row_3_1 = _mm512_mask_blend_epi32(0xCCCC,_mm512_shuffle_epi32(iacc_mat_10_1, (_MM_PERM_ENUM)78), iacc_mat_11_1);
__m512i iacc_row_0 = _mm512_add_epi32(iacc_row_0_0, iacc_row_0_1);
__m512i iacc_row_1 = _mm512_add_epi32(iacc_row_1_0, iacc_row_1_1);
__m512i iacc_row_2 = _mm512_add_epi32(iacc_row_2_0, iacc_row_2_1);
__m512i iacc_row_3 = _mm512_add_epi32(iacc_row_3_0, iacc_row_3_1);
const __m128 row_scale_f32_sse = _mm_load_ps(a_ptrs[rp][b].d);
const __m256 row_scale_f32_ymm = _mm256_set_m128(row_scale_f32_sse, row_scale_f32_sse);
const __m512 row_scale_f32 = _mm512_insertf32x8(_mm512_castps256_ps512(row_scale_f32_ymm), row_scale_f32_ymm, 1);
acc_rows[rp * 4] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_0), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_rows[rp * 4]);
acc_rows[rp * 4  + 1] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_1), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_rows[rp * 4 + 1]);
acc_rows[rp * 4 + 2] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_2), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_rows[rp * 4 + 2]);
acc_rows[rp * 4 + 3] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_3), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_rows[rp * 4 + 3]);
__m512i iacc_row_min_0 = _mm512_madd_epi16(_mm512_shuffle_epi32(lhs_bsums_hsum_0123_01, (_MM_PERM_ENUM)0), mins_01);
__m512i iacc_row_min_1 = _mm512_madd_epi16(_mm512_shuffle_epi32(lhs_bsums_hsum_0123_01, (_MM_PERM_ENUM)85), mins_01);
__m512i iacc_row_min_2 = _mm512_madd_epi16(_mm512_shuffle_epi32(lhs_bsums_hsum_0123_01, (_MM_PERM_ENUM)170), mins_01);
__m512i iacc_row_min_3 = _mm512_madd_epi16(_mm512_shuffle_epi32(lhs_bsums_hsum_0123_01, (_MM_PERM_ENUM)255), mins_01);
acc_min_rows[rp * 4] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_min_0), _mm512_mul_ps(col_dmin_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_min_rows[rp * 4]);
acc_min_rows[rp * 4 + 1] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_min_1), _mm512_mul_ps(col_dmin_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_min_rows[rp * 4 + 1]);
acc_min_rows[rp * 4 + 2] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_min_2), _mm512_mul_ps(col_dmin_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_min_rows[rp * 4 + 2]);
acc_min_rows[rp * 4 + 3] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_min_3), _mm512_mul_ps(col_dmin_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_min_rows[rp * 4 + 3]);
}
}
}
for (int i = 0; i < 16; i++) {
_mm512_storeu_ps((float * )(s + ((y * 4 + i) * bs + x * 8)), _mm512_sub_ps(acc_rows[i], acc_min_rows[i]));
}
}
}
for (; y < nr / 4; y++) {
const block_q8_Kx4 * a_ptr = a_ptr_start + (y * nb);
for (int64_t x = 0; x < anc / 8; x += 2) {
const block_q4_Kx8 * b_ptr_0 = b_ptr_start + ((x) * b_nb);
const block_q4_Kx8 * b_ptr_1 = b_ptr_start + ((x + 1) * b_nb);
__m512 acc_rows[4];
for (int i = 0; i < 4; i++) {
acc_rows[i] = _mm512_setzero_ps();
}
__m512 acc_min_rows[4];
for (int i = 0; i < 4; i++) {
acc_min_rows[i] = _mm512_setzero_ps();
}
for (int64_t b = 0; b < nb; b++) {
const __m512 col_scale_f32 = GGML_F32Cx8x2_LOAD(b_ptr_0[b].d, b_ptr_1[b].d);
const __m512 col_dmin_f32 = GGML_F32Cx8x2_LOAD(b_ptr_0[b].dmin, b_ptr_1[b].dmin);
for (int sb = 0; sb < QK_K / 64; sb++) {
const __m256i rhs_raw_mat_0123_0 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + sb * 256));
const __m256i rhs_raw_mat_4567_0 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 32 + sb * 256));
const __m256i rhs_raw_mat_0123_1 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 64 + sb * 256));
const __m256i rhs_raw_mat_4567_1 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 96 + sb * 256));
const __m256i rhs_raw_mat_0123_2 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 128 + sb * 256));
const __m256i rhs_raw_mat_4567_2 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 160 + sb * 256));
const __m256i rhs_raw_mat_0123_3 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 192 + sb * 256));
const __m256i rhs_raw_mat_4567_3 = _mm256_loadu_si256((const __m256i * )(b_ptr_0[b].qs + 224 + sb * 256));
const __m256i rhs_raw_mat_89AB_0 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + sb * 256));
const __m256i rhs_raw_mat_CDEF_0 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 32 + sb * 256));
const __m256i rhs_raw_mat_89AB_1 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 64 + sb * 256));
const __m256i rhs_raw_mat_CDEF_1 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 96 + sb * 256));
const __m256i rhs_raw_mat_89AB_2 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 128 + sb * 256));
const __m256i rhs_raw_mat_CDEF_2 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 160 + sb * 256));
const __m256i rhs_raw_mat_89AB_3 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 192 + sb * 256));
const __m256i rhs_raw_mat_CDEF_3 = _mm256_loadu_si256((const __m256i * )(b_ptr_1[b].qs + 224 + sb * 256));
const __m256i rhs_raw_mat_0145_0 = _mm256_blend_epi32(rhs_raw_mat_0123_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_0, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_0, requiredOrder), rhs_raw_mat_4567_0, 240);
const __m256i rhs_raw_mat_0145_1 = _mm256_blend_epi32(rhs_raw_mat_0123_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_1, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_1, requiredOrder), rhs_raw_mat_4567_1, 240);
const __m256i rhs_raw_mat_0145_2 = _mm256_blend_epi32(rhs_raw_mat_0123_2, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_2, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_2 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_2, requiredOrder), rhs_raw_mat_4567_2, 240);
const __m256i rhs_raw_mat_0145_3 = _mm256_blend_epi32(rhs_raw_mat_0123_3, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_3, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_3 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_3, requiredOrder), rhs_raw_mat_4567_3, 240);
const __m256i rhs_raw_mat_89CD_0 = _mm256_blend_epi32(rhs_raw_mat_89AB_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_0, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_0, requiredOrder), rhs_raw_mat_CDEF_0, 240);
const __m256i rhs_raw_mat_89CD_1 = _mm256_blend_epi32(rhs_raw_mat_89AB_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_1, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_1, requiredOrder), rhs_raw_mat_CDEF_1, 240);
const __m256i rhs_raw_mat_89CD_2 = _mm256_blend_epi32(rhs_raw_mat_89AB_2, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_2, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_2 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_2, requiredOrder), rhs_raw_mat_CDEF_2, 240);
const __m256i rhs_raw_mat_89CD_3 = _mm256_blend_epi32(rhs_raw_mat_89AB_3, _mm256_permutevar8x32_epi32(rhs_raw_mat_CDEF_3, requiredOrder), 240);
const __m256i rhs_raw_mat_ABEF_3 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_89AB_3, requiredOrder), rhs_raw_mat_CDEF_3, 240);
const __m512i rhs_raw_mat_014589CD_0 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_0), rhs_raw_mat_89CD_0, 1);
const __m512i rhs_raw_mat_2367ABEF_0 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_0), rhs_raw_mat_ABEF_0, 1);
const __m512i rhs_raw_mat_014589CD_1 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_1), rhs_raw_mat_89CD_1, 1);
const __m512i rhs_raw_mat_2367ABEF_1 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_1), rhs_raw_mat_ABEF_1, 1);
const __m512i rhs_raw_mat_014589CD_2 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_2), rhs_raw_mat_89CD_2, 1);
const __m512i rhs_raw_mat_2367ABEF_2 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_2), rhs_raw_mat_ABEF_2, 1);
const __m512i rhs_raw_mat_014589CD_3 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_0145_3), rhs_raw_mat_89CD_3, 1);
const __m512i rhs_raw_mat_2367ABEF_3 = _mm512_inserti32x8(_mm512_castsi256_si512(rhs_raw_mat_2367_3), rhs_raw_mat_ABEF_3, 1);
const __m512i rhs_mat_014589CD_00 = _mm512_and_si512(rhs_raw_mat_014589CD_0, m4bexpanded);
const __m512i rhs_mat_2367ABEF_00 = _mm512_and_si512(rhs_raw_mat_2367ABEF_0, m4bexpanded);
const __m512i rhs_mat_014589CD_01 = _mm512_and_si512(rhs_raw_mat_014589CD_1, m4bexpanded);
const __m512i rhs_mat_2367ABEF_01 = _mm512_and_si512(rhs_raw_mat_2367ABEF_1, m4bexpanded);
const __m512i rhs_mat_014589CD_02 = _mm512_and_si512(rhs_raw_mat_014589CD_2, m4bexpanded);
const __m512i rhs_mat_2367ABEF_02 = _mm512_and_si512(rhs_raw_mat_2367ABEF_2, m4bexpanded);
const __m512i rhs_mat_014589CD_03 = _mm512_and_si512(rhs_raw_mat_014589CD_3, m4bexpanded);
const __m512i rhs_mat_2367ABEF_03 = _mm512_and_si512(rhs_raw_mat_2367ABEF_3, m4bexpanded);
const __m512i rhs_mat_014589CD_10 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_0, 4), m4bexpanded);
const __m512i rhs_mat_2367ABEF_10 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_0, 4), m4bexpanded);
const __m512i rhs_mat_014589CD_11 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_1, 4), m4bexpanded);
const __m512i rhs_mat_2367ABEF_11 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_1, 4), m4bexpanded);
const __m512i rhs_mat_014589CD_12 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_2, 4), m4bexpanded);
const __m512i rhs_mat_2367ABEF_12 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_2, 4), m4bexpanded);
const __m512i rhs_mat_014589CD_13 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_014589CD_3, 4), m4bexpanded);
const __m512i rhs_mat_2367ABEF_13 = _mm512_and_si512(_mm512_srli_epi16(rhs_raw_mat_2367ABEF_3, 4), m4bexpanded);
const __m512i rhs_mat_014589CD_00_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_00, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_00_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_00, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_01_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_01, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_01_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_01, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_02_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_02, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_02_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_02, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_03_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_03, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_03_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_03, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_10_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_10, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_10_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_10, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_11_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_11, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_11_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_11, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_12_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_12, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_12_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_12, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_13_sp1 = _mm512_shuffle_epi32(rhs_mat_014589CD_13, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_2367ABEF_13_sp1 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_13, (_MM_PERM_ENUM)136);
const __m512i rhs_mat_014589CD_00_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_00, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_00_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_00, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_01_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_01, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_01_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_01, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_02_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_02, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_02_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_02, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_03_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_03, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_03_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_03, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_10_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_10, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_10_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_10, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_11_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_11, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_11_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_11, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_12_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_12, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_12_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_12, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_014589CD_13_sp2 = _mm512_shuffle_epi32(rhs_mat_014589CD_13, (_MM_PERM_ENUM)221);
const __m512i rhs_mat_2367ABEF_13_sp2 = _mm512_shuffle_epi32(rhs_mat_2367ABEF_13, (_MM_PERM_ENUM)221);
uint32_t utmp_00[4], utmp_01[4], utmp_10[4], utmp_11[4];
memcpy(utmp_00, b_ptr_0[b].scales + 24 * sb, 12);
utmp_00[3] = ((utmp_00[2] >> 4) & kmask2) | (((utmp_00[1] >> 6) & kmask3) << 4);
const uint32_t uaux_00 = utmp_00[1] & kmask1;
utmp_00[1] = (utmp_00[2] & kmask2) | (((utmp_00[0] >> 6) & kmask3) << 4);
utmp_00[2] = uaux_00;
utmp_00[0] &= kmask1;
memcpy(utmp_01, b_ptr_0[b].scales + 12 + sb * 24, 12);
utmp_01[3] = ((utmp_01[2] >> 4) & kmask2) | (((utmp_01[1] >> 6) & kmask3) << 4);
const uint32_t uaux_01 = utmp_01[1] & kmask1;
utmp_01[1] = (utmp_01[2] & kmask2) | (((utmp_01[0] >> 6) & kmask3) << 4);
utmp_01[2] = uaux_01;
utmp_01[0] &= kmask1;
memcpy(utmp_10, b_ptr_1[b].scales + sb * 24, 12);
utmp_10[3] = ((utmp_10[2] >> 4) & kmask2) | (((utmp_10[1] >> 6) & kmask3) << 4);
const uint32_t uaux_10 = utmp_10[1] & kmask1;
utmp_10[1] = (utmp_10[2] & kmask2) | (((utmp_10[0] >> 6) & kmask3) << 4);
utmp_10[2] = uaux_10;
utmp_10[0] &= kmask1;
memcpy(utmp_11, b_ptr_1[b].scales + 12 + sb * 24, 12);
utmp_11[3] = ((utmp_11[2] >> 4) & kmask2) | (((utmp_11[1] >> 6) & kmask3) << 4);
const uint32_t uaux_11 = utmp_11[1] & kmask1;
utmp_11[1] = (utmp_11[2] & kmask2) | (((utmp_11[0] >> 6) & kmask3) << 4);
utmp_11[2] = uaux_11;
utmp_11[0] &= kmask1;
const __m256i mins_and_scales_0 = _mm256_set_epi32(utmp_10[3], utmp_10[2], utmp_10[1], utmp_10[0], utmp_00[3], utmp_00[2], utmp_00[1], utmp_00[0]);
const __m512i scales_0 = _mm512_cvtepu8_epi16(_mm256_unpacklo_epi8(mins_and_scales_0, mins_and_scales_0));
const __m256i mins_and_scales_1 = _mm256_set_epi32(utmp_11[3], utmp_11[2], utmp_11[1], utmp_11[0], utmp_01[3], utmp_01[2], utmp_01[1], utmp_01[0]);
const __m512i scales_1 = _mm512_cvtepu8_epi16(_mm256_unpacklo_epi8(mins_and_scales_1, mins_and_scales_1));
const __m512i mins_01 = _mm512_cvtepu8_epi16(_mm256_unpacklo_epi8(_mm256_shuffle_epi32(mins_and_scales_0, 78), _mm256_shuffle_epi32(mins_and_scales_1, 78)));
const __m512i scale_014589CD_0 = _mm512_shuffle_epi32(scales_0, (_MM_PERM_ENUM)68);
const __m512i scale_2367ABEF_0 = _mm512_shuffle_epi32(scales_0, (_MM_PERM_ENUM)238);
const __m512i scale_014589CD_1 = _mm512_shuffle_epi32(scales_1, (_MM_PERM_ENUM)68);
const __m512i scale_2367ABEF_1 = _mm512_shuffle_epi32(scales_1, (_MM_PERM_ENUM)238);
__m256i lhs_mat_ymm_0123_00 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 256 * sb)));
__m256i lhs_mat_ymm_01_00 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_00, lhs_mat_ymm_0123_00, 0);
__m256i lhs_mat_ymm_23_00 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_00, lhs_mat_ymm_0123_00, 17);
__m256i lhs_mat_ymm_0123_01 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 32 + 256 * sb)));
__m256i lhs_mat_ymm_01_01 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_01, lhs_mat_ymm_0123_01, 0);
__m256i lhs_mat_ymm_23_01 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_01, lhs_mat_ymm_0123_01, 17);
__m256i lhs_mat_ymm_0123_02 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 64 + 256 * sb)));
__m256i lhs_mat_ymm_01_02 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_02, lhs_mat_ymm_0123_02, 0);
__m256i lhs_mat_ymm_23_02 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_02, lhs_mat_ymm_0123_02, 17);
__m256i lhs_mat_ymm_0123_03 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 96 + 256 * sb)));
__m256i lhs_mat_ymm_01_03 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_03, lhs_mat_ymm_0123_03, 0);
__m256i lhs_mat_ymm_23_03 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_03, lhs_mat_ymm_0123_03, 17);
__m256i lhs_mat_ymm_0123_10 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 128 + 256 * sb)));
__m256i lhs_mat_ymm_01_10 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_10, lhs_mat_ymm_0123_10, 0);
__m256i lhs_mat_ymm_23_10 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_10, lhs_mat_ymm_0123_10, 17);
__m256i lhs_mat_ymm_0123_11 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 160 + 256 * sb)));
__m256i lhs_mat_ymm_01_11 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_11, lhs_mat_ymm_0123_11, 0);
__m256i lhs_mat_ymm_23_11 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_11, lhs_mat_ymm_0123_11, 17);
__m256i lhs_mat_ymm_0123_12 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 192 + 256 * sb)));
__m256i lhs_mat_ymm_01_12 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_12, lhs_mat_ymm_0123_12, 0);
__m256i lhs_mat_ymm_23_12 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_12, lhs_mat_ymm_0123_12, 17);
__m256i lhs_mat_ymm_0123_13 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 224 + 256 * sb)));
__m256i lhs_mat_ymm_01_13 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_13, lhs_mat_ymm_0123_13, 0);
__m256i lhs_mat_ymm_23_13 = _mm256_permute2f128_si256(lhs_mat_ymm_0123_13, lhs_mat_ymm_0123_13, 17);
__m512i lhs_mat_01_00 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_00), lhs_mat_ymm_01_00, 1);
__m512i lhs_mat_23_00 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_00), lhs_mat_ymm_23_00, 1);
__m512i lhs_mat_01_01 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_01), lhs_mat_ymm_01_01, 1);
__m512i lhs_mat_23_01 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_01), lhs_mat_ymm_23_01, 1);
__m512i lhs_mat_01_02 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_02), lhs_mat_ymm_01_02, 1);
__m512i lhs_mat_23_02 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_02), lhs_mat_ymm_23_02, 1);
__m512i lhs_mat_01_03 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_03), lhs_mat_ymm_01_03, 1);
__m512i lhs_mat_23_03 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_03), lhs_mat_ymm_23_03, 1);
__m512i lhs_mat_01_10 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_10), lhs_mat_ymm_01_10, 1);
__m512i lhs_mat_23_10 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_10), lhs_mat_ymm_23_10, 1);
__m512i lhs_mat_01_11 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_11), lhs_mat_ymm_01_11, 1);
__m512i lhs_mat_23_11 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_11), lhs_mat_ymm_23_11, 1);
__m512i lhs_mat_01_12 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_12), lhs_mat_ymm_01_12, 1);
__m512i lhs_mat_23_12 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_12), lhs_mat_ymm_23_12, 1);
__m512i lhs_mat_01_13 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_01_13), lhs_mat_ymm_01_13, 1);
__m512i lhs_mat_23_13 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_mat_ymm_23_13), lhs_mat_ymm_23_13, 1);
__m256i lhs_bsums_0123_01 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].bsums + 16 * sb)));
__m256i lhs_bsums_hsum_ymm_0123_01 = _mm256_castsi128_si256(_mm_hadd_epi16(_mm256_castsi256_si128(lhs_bsums_0123_01), _mm256_extractf128_si256(lhs_bsums_0123_01, 1)));
lhs_bsums_hsum_ymm_0123_01 = _mm256_permute2x128_si256(lhs_bsums_hsum_ymm_0123_01, lhs_bsums_hsum_ymm_0123_01, 0);
__m512i lhs_bsums_hsum_0123_01 = _mm512_inserti32x8(_mm512_castsi256_si512(lhs_bsums_hsum_ymm_0123_01), lhs_bsums_hsum_ymm_0123_01, 1);
const __m512i lhs_mat_01_00_sp1 = _mm512_shuffle_epi32(lhs_mat_01_00, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_00_sp1 = _mm512_shuffle_epi32(lhs_mat_23_00, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_01_sp1 = _mm512_shuffle_epi32(lhs_mat_01_01, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_01_sp1 = _mm512_shuffle_epi32(lhs_mat_23_01, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_02_sp1 = _mm512_shuffle_epi32(lhs_mat_01_02, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_02_sp1 = _mm512_shuffle_epi32(lhs_mat_23_02, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_03_sp1 = _mm512_shuffle_epi32(lhs_mat_01_03, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_03_sp1 = _mm512_shuffle_epi32(lhs_mat_23_03, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_10_sp1 = _mm512_shuffle_epi32(lhs_mat_01_10, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_10_sp1 = _mm512_shuffle_epi32(lhs_mat_23_10, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_11_sp1 = _mm512_shuffle_epi32(lhs_mat_01_11, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_11_sp1 = _mm512_shuffle_epi32(lhs_mat_23_11, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_12_sp1 = _mm512_shuffle_epi32(lhs_mat_01_12, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_12_sp1 = _mm512_shuffle_epi32(lhs_mat_23_12, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_13_sp1 = _mm512_shuffle_epi32(lhs_mat_01_13, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_23_13_sp1 = _mm512_shuffle_epi32(lhs_mat_23_13, (_MM_PERM_ENUM)160);
const __m512i lhs_mat_01_00_sp2 = _mm512_shuffle_epi32(lhs_mat_01_00, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_00_sp2 = _mm512_shuffle_epi32(lhs_mat_23_00, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_01_sp2 = _mm512_shuffle_epi32(lhs_mat_01_01, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_01_sp2 = _mm512_shuffle_epi32(lhs_mat_23_01, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_02_sp2 = _mm512_shuffle_epi32(lhs_mat_01_02, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_02_sp2 = _mm512_shuffle_epi32(lhs_mat_23_02, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_03_sp2 = _mm512_shuffle_epi32(lhs_mat_01_03, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_03_sp2 = _mm512_shuffle_epi32(lhs_mat_23_03, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_10_sp2 = _mm512_shuffle_epi32(lhs_mat_01_10, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_10_sp2 = _mm512_shuffle_epi32(lhs_mat_23_10, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_11_sp2 = _mm512_shuffle_epi32(lhs_mat_01_11, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_11_sp2 = _mm512_shuffle_epi32(lhs_mat_23_11, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_12_sp2 = _mm512_shuffle_epi32(lhs_mat_01_12, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_12_sp2 = _mm512_shuffle_epi32(lhs_mat_23_12, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_01_13_sp2 = _mm512_shuffle_epi32(lhs_mat_01_13, (_MM_PERM_ENUM)245);
const __m512i lhs_mat_23_13_sp2 = _mm512_shuffle_epi32(lhs_mat_23_13, (_MM_PERM_ENUM)245);
__m512i iacc_mat_00_0_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_03_sp1, lhs_mat_01_03_sp1), _mm512_maddubs_epi16(rhs_mat_014589CD_02_sp1, lhs_mat_01_02_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_01_sp1, lhs_mat_01_01_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_00_sp1, lhs_mat_01_00_sp1));
__m512i iacc_mat_01_0_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_03_sp1, lhs_mat_01_03_sp1), _mm512_maddubs_epi16(rhs_mat_2367ABEF_02_sp1, lhs_mat_01_02_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_01_sp1, lhs_mat_01_01_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_00_sp1, lhs_mat_01_00_sp1));
__m512i iacc_mat_10_0_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_03_sp1, lhs_mat_23_03_sp1), _mm512_maddubs_epi16(rhs_mat_014589CD_02_sp1, lhs_mat_23_02_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_01_sp1, lhs_mat_23_01_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_00_sp1, lhs_mat_23_00_sp1));
__m512i iacc_mat_11_0_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_03_sp1, lhs_mat_23_03_sp1), _mm512_maddubs_epi16(rhs_mat_2367ABEF_02_sp1, lhs_mat_23_02_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_01_sp1, lhs_mat_23_01_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_00_sp1, lhs_mat_23_00_sp1));
__m512i iacc_mat_00_1_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_13_sp1, lhs_mat_01_13_sp1), _mm512_maddubs_epi16(rhs_mat_014589CD_12_sp1, lhs_mat_01_12_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_11_sp1, lhs_mat_01_11_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_10_sp1, lhs_mat_01_10_sp1));
__m512i iacc_mat_01_1_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_13_sp1, lhs_mat_01_13_sp1), _mm512_maddubs_epi16(rhs_mat_2367ABEF_12_sp1, lhs_mat_01_12_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_11_sp1, lhs_mat_01_11_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_10_sp1, lhs_mat_01_10_sp1));
__m512i iacc_mat_10_1_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_13_sp1, lhs_mat_23_13_sp1), _mm512_maddubs_epi16(rhs_mat_014589CD_12_sp1, lhs_mat_23_12_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_11_sp1, lhs_mat_23_11_sp1)), _mm512_maddubs_epi16(rhs_mat_014589CD_10_sp1, lhs_mat_23_10_sp1));
__m512i iacc_mat_11_1_sp1 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_13_sp1, lhs_mat_23_13_sp1), _mm512_maddubs_epi16(rhs_mat_2367ABEF_12_sp1, lhs_mat_23_12_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_11_sp1, lhs_mat_23_11_sp1)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_10_sp1, lhs_mat_23_10_sp1));
__m512i iacc_mat_00_0_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_03_sp2, lhs_mat_01_03_sp2), _mm512_maddubs_epi16(rhs_mat_014589CD_02_sp2, lhs_mat_01_02_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_01_sp2, lhs_mat_01_01_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_00_sp2, lhs_mat_01_00_sp2));
__m512i iacc_mat_01_0_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_03_sp2, lhs_mat_01_03_sp2), _mm512_maddubs_epi16(rhs_mat_2367ABEF_02_sp2, lhs_mat_01_02_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_01_sp2, lhs_mat_01_01_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_00_sp2, lhs_mat_01_00_sp2));
__m512i iacc_mat_10_0_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_03_sp2, lhs_mat_23_03_sp2), _mm512_maddubs_epi16(rhs_mat_014589CD_02_sp2, lhs_mat_23_02_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_01_sp2, lhs_mat_23_01_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_00_sp2, lhs_mat_23_00_sp2));
__m512i iacc_mat_11_0_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_03_sp2, lhs_mat_23_03_sp2), _mm512_maddubs_epi16(rhs_mat_2367ABEF_02_sp2, lhs_mat_23_02_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_01_sp2, lhs_mat_23_01_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_00_sp2, lhs_mat_23_00_sp2));
__m512i iacc_mat_00_1_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_13_sp2, lhs_mat_01_13_sp2), _mm512_maddubs_epi16(rhs_mat_014589CD_12_sp2, lhs_mat_01_12_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_11_sp2, lhs_mat_01_11_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_10_sp2, lhs_mat_01_10_sp2));
__m512i iacc_mat_01_1_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_13_sp2, lhs_mat_01_13_sp2), _mm512_maddubs_epi16(rhs_mat_2367ABEF_12_sp2, lhs_mat_01_12_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_11_sp2, lhs_mat_01_11_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_10_sp2, lhs_mat_01_10_sp2));
__m512i iacc_mat_10_1_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_014589CD_13_sp2, lhs_mat_23_13_sp2), _mm512_maddubs_epi16(rhs_mat_014589CD_12_sp2, lhs_mat_23_12_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_11_sp2, lhs_mat_23_11_sp2)), _mm512_maddubs_epi16(rhs_mat_014589CD_10_sp2, lhs_mat_23_10_sp2));
__m512i iacc_mat_11_1_sp2 = _mm512_add_epi16(_mm512_add_epi16(_mm512_add_epi16(_mm512_maddubs_epi16(rhs_mat_2367ABEF_13_sp2, lhs_mat_23_13_sp2), _mm512_maddubs_epi16(rhs_mat_2367ABEF_12_sp2, lhs_mat_23_12_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_11_sp2, lhs_mat_23_11_sp2)), _mm512_maddubs_epi16(rhs_mat_2367ABEF_10_sp2, lhs_mat_23_10_sp2));
__m512i iacc_mat_00_0 = _mm512_add_epi16(iacc_mat_00_0_sp1, iacc_mat_00_0_sp2);
__m512i iacc_mat_01_0 = _mm512_add_epi16(iacc_mat_01_0_sp1, iacc_mat_01_0_sp2);
__m512i iacc_mat_10_0 = _mm512_add_epi16(iacc_mat_10_0_sp1, iacc_mat_10_0_sp2);
__m512i iacc_mat_11_0 = _mm512_add_epi16(iacc_mat_11_0_sp1, iacc_mat_11_0_sp2);
__m512i iacc_mat_00_1 = _mm512_add_epi16(iacc_mat_00_1_sp1, iacc_mat_00_1_sp2);
__m512i iacc_mat_01_1 = _mm512_add_epi16(iacc_mat_01_1_sp1, iacc_mat_01_1_sp2);
__m512i iacc_mat_10_1 = _mm512_add_epi16(iacc_mat_10_1_sp1, iacc_mat_10_1_sp2);
__m512i iacc_mat_11_1 = _mm512_add_epi16(iacc_mat_11_1_sp1, iacc_mat_11_1_sp2);
iacc_mat_00_0 = _mm512_madd_epi16(iacc_mat_00_0, scale_014589CD_0);
iacc_mat_01_0 = _mm512_madd_epi16(iacc_mat_01_0, scale_2367ABEF_0);
iacc_mat_10_0 = _mm512_madd_epi16(iacc_mat_10_0, scale_014589CD_0);
iacc_mat_11_0 = _mm512_madd_epi16(iacc_mat_11_0, scale_2367ABEF_0);
iacc_mat_00_1 = _mm512_madd_epi16(iacc_mat_00_1, scale_014589CD_1);
iacc_mat_01_1 = _mm512_madd_epi16(iacc_mat_01_1, scale_2367ABEF_1);
iacc_mat_10_1 = _mm512_madd_epi16(iacc_mat_10_1, scale_014589CD_1);
iacc_mat_11_1 = _mm512_madd_epi16(iacc_mat_11_1, scale_2367ABEF_1);
__m512i iacc_row_0_0 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_00_0, _mm512_shuffle_epi32(iacc_mat_01_0, (_MM_PERM_ENUM)78));
__m512i iacc_row_1_0 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_00_0, (_MM_PERM_ENUM)78), iacc_mat_01_0);
__m512i iacc_row_2_0 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_10_0, _mm512_shuffle_epi32(iacc_mat_11_0, (_MM_PERM_ENUM)78));
__m512i iacc_row_3_0 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_10_0, (_MM_PERM_ENUM)78), iacc_mat_11_0);
__m512i iacc_row_0_1 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_00_1, _mm512_shuffle_epi32(iacc_mat_01_1, (_MM_PERM_ENUM)78));
__m512i iacc_row_1_1 = _mm512_mask_blend_epi32(0xCCCC, _mm512_shuffle_epi32(iacc_mat_00_1, (_MM_PERM_ENUM)78), iacc_mat_01_1);
__m512i iacc_row_2_1 = _mm512_mask_blend_epi32(0xCCCC, iacc_mat_10_1, _mm512_shuffle_epi32(iacc_mat_11_1, (_MM_PERM_ENUM)78));
__m512i iacc_row_3_1 = _mm512_mask_blend_epi32(0xCCCC,_mm512_shuffle_epi32(iacc_mat_10_1, (_MM_PERM_ENUM)78), iacc_mat_11_1);
__m512i iacc_row_0 = _mm512_add_epi32(iacc_row_0_0, iacc_row_0_1);
__m512i iacc_row_1 = _mm512_add_epi32(iacc_row_1_0, iacc_row_1_1);
__m512i iacc_row_2 = _mm512_add_epi32(iacc_row_2_0, iacc_row_2_1);
__m512i iacc_row_3 = _mm512_add_epi32(iacc_row_3_0, iacc_row_3_1);
const __m128 row_scale_f32_sse = _mm_load_ps(a_ptr[b].d);
const __m256 row_scale_f32_ymm = _mm256_set_m128(row_scale_f32_sse, row_scale_f32_sse);
const __m512 row_scale_f32 = _mm512_insertf32x8(_mm512_castps256_ps512(row_scale_f32_ymm), row_scale_f32_ymm, 1);
acc_rows[0] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_0), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_rows[0]);
acc_rows[1] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_1), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_rows[1]);
acc_rows[2] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_2), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_rows[2]);
acc_rows[3] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_3), _mm512_mul_ps(col_scale_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_rows[3]);
__m512i iacc_row_min_0 = _mm512_madd_epi16(_mm512_shuffle_epi32(lhs_bsums_hsum_0123_01, (_MM_PERM_ENUM)0), mins_01);
__m512i iacc_row_min_1 = _mm512_madd_epi16(_mm512_shuffle_epi32(lhs_bsums_hsum_0123_01, (_MM_PERM_ENUM)85), mins_01);
__m512i iacc_row_min_2 = _mm512_madd_epi16(_mm512_shuffle_epi32(lhs_bsums_hsum_0123_01, (_MM_PERM_ENUM)170), mins_01);
__m512i iacc_row_min_3 = _mm512_madd_epi16(_mm512_shuffle_epi32(lhs_bsums_hsum_0123_01, (_MM_PERM_ENUM)255), mins_01);
acc_min_rows[0] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_min_0), _mm512_mul_ps(col_dmin_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_min_rows[0]);
acc_min_rows[1] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_min_1), _mm512_mul_ps(col_dmin_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_min_rows[1]);
acc_min_rows[2] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_min_2), _mm512_mul_ps(col_dmin_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_min_rows[2]);
acc_min_rows[3] = _mm512_fmadd_ps(_mm512_cvtepi32_ps(iacc_row_min_3), _mm512_mul_ps(col_dmin_f32, _mm512_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_min_rows[3]);
}
}
for (int i = 0; i < 4; i++) {
_mm512_storeu_ps((float * )(s + ((y * 4 + i) * bs + x * 8)), _mm512_sub_ps(acc_rows[i], acc_min_rows[i]));
}
}
}
if (anc != nc) {
xstart = anc/8;
y = 0;
}
#endif
for (; y < anr / 4; y += 4) {
const block_q8_Kx4 * a_ptrs[4];
a_ptrs[0] = a_ptr_start + (y * nb);
for (int i = 0; i < 3; ++i) {
a_ptrs[i + 1] = a_ptrs[i] + nb;
}
for (int64_t x = xstart; x < nc / 8; x++) {
const block_q4_Kx8 * b_ptr = b_ptr_start + (x * b_nb);
__m256 acc_rows[16];
for (int i = 0; i < 16; i++) {
acc_rows[i] = _mm256_setzero_ps();
}
__m256 acc_min_rows[16];
for (int i = 0; i < 16; i++) {
acc_min_rows[i] = _mm256_setzero_ps();
}
for (int64_t b = 0; b < nb; b++) {
const __m256 col_scale_f32 = GGML_F32Cx8_LOAD(b_ptr[b].d);
const __m256 col_dmin_f32 = GGML_F32Cx8_LOAD(b_ptr[b].dmin);
for (int sb = 0; sb < QK_K / 64; sb++) {
const __m256i rhs_raw_mat_0123_0 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + sb * 256));
const __m256i rhs_raw_mat_4567_0 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 32 + sb * 256));
const __m256i rhs_raw_mat_0123_1 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 64 + sb * 256));
const __m256i rhs_raw_mat_4567_1 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 96 + sb * 256));
const __m256i rhs_raw_mat_0123_2 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 128 + sb * 256));
const __m256i rhs_raw_mat_4567_2 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 160 + sb * 256));
const __m256i rhs_raw_mat_0123_3 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 192 + sb * 256));
const __m256i rhs_raw_mat_4567_3 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 224 + sb * 256));
const __m256i rhs_raw_mat_0145_0 = _mm256_blend_epi32(rhs_raw_mat_0123_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_0, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_0, requiredOrder), rhs_raw_mat_4567_0, 240);
const __m256i rhs_raw_mat_0145_1 = _mm256_blend_epi32(rhs_raw_mat_0123_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_1, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_1, requiredOrder), rhs_raw_mat_4567_1, 240);
const __m256i rhs_raw_mat_0145_2 = _mm256_blend_epi32(rhs_raw_mat_0123_2, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_2, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_2 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_2, requiredOrder), rhs_raw_mat_4567_2, 240);
const __m256i rhs_raw_mat_0145_3 = _mm256_blend_epi32(rhs_raw_mat_0123_3, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_3, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_3 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_3, requiredOrder), rhs_raw_mat_4567_3, 240);
const __m256i rhs_mat_0145_00 = _mm256_and_si256(rhs_raw_mat_0145_0, m4b);
const __m256i rhs_mat_2367_00 = _mm256_and_si256(rhs_raw_mat_2367_0, m4b);
const __m256i rhs_mat_0145_01 = _mm256_and_si256(rhs_raw_mat_0145_1, m4b);
const __m256i rhs_mat_2367_01 = _mm256_and_si256(rhs_raw_mat_2367_1, m4b);
const __m256i rhs_mat_0145_02 = _mm256_and_si256(rhs_raw_mat_0145_2, m4b);
const __m256i rhs_mat_2367_02 = _mm256_and_si256(rhs_raw_mat_2367_2, m4b);
const __m256i rhs_mat_0145_03 = _mm256_and_si256(rhs_raw_mat_0145_3, m4b);
const __m256i rhs_mat_2367_03 = _mm256_and_si256(rhs_raw_mat_2367_3, m4b);
const __m256i rhs_mat_0145_10 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_0, 4), m4b);
const __m256i rhs_mat_2367_10 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_0, 4), m4b);
const __m256i rhs_mat_0145_11 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_1, 4), m4b);
const __m256i rhs_mat_2367_11 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_1, 4), m4b);
const __m256i rhs_mat_0145_12 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_2, 4), m4b);
const __m256i rhs_mat_2367_12 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_2, 4), m4b);
const __m256i rhs_mat_0145_13 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_3, 4), m4b);
const __m256i rhs_mat_2367_13 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_3, 4), m4b);
const __m256i rhs_mat_0145_00_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_00, 136);
const __m256i rhs_mat_2367_00_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_00, 136);
const __m256i rhs_mat_0145_01_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_01, 136);
const __m256i rhs_mat_2367_01_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_01, 136);
const __m256i rhs_mat_0145_02_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_02, 136);
const __m256i rhs_mat_2367_02_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_02, 136);
const __m256i rhs_mat_0145_03_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_03, 136);
const __m256i rhs_mat_2367_03_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_03, 136);
const __m256i rhs_mat_0145_10_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_10, 136);
const __m256i rhs_mat_2367_10_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_10, 136);
const __m256i rhs_mat_0145_11_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_11, 136);
const __m256i rhs_mat_2367_11_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_11, 136);
const __m256i rhs_mat_0145_12_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_12, 136);
const __m256i rhs_mat_2367_12_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_12, 136);
const __m256i rhs_mat_0145_13_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_13, 136);
const __m256i rhs_mat_2367_13_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_13, 136);
const __m256i rhs_mat_0145_00_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_00, 221);
const __m256i rhs_mat_2367_00_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_00, 221);
const __m256i rhs_mat_0145_01_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_01, 221);
const __m256i rhs_mat_2367_01_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_01, 221);
const __m256i rhs_mat_0145_02_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_02, 221);
const __m256i rhs_mat_2367_02_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_02, 221);
const __m256i rhs_mat_0145_03_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_03, 221);
const __m256i rhs_mat_2367_03_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_03, 221);
const __m256i rhs_mat_0145_10_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_10, 221);
const __m256i rhs_mat_2367_10_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_10, 221);
const __m256i rhs_mat_0145_11_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_11, 221);
const __m256i rhs_mat_2367_11_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_11, 221);
const __m256i rhs_mat_0145_12_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_12, 221);
const __m256i rhs_mat_2367_12_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_12, 221);
const __m256i rhs_mat_0145_13_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_13, 221);
const __m256i rhs_mat_2367_13_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_13, 221);
uint32_t utmp_0[4], utmp_1[4];
memcpy(utmp_0, b_ptr[b].scales + 24 * sb, 12);
utmp_0[3] = ((utmp_0[2] >> 4) & kmask2) | (((utmp_0[1] >> 6) & kmask3) << 4);
const uint32_t uaux_0 = utmp_0[1] & kmask1;
utmp_0[1] = (utmp_0[2] & kmask2) | (((utmp_0[0] >> 6) & kmask3) << 4);
utmp_0[2] = uaux_0;
utmp_0[0] &= kmask1;
memcpy(utmp_1, b_ptr[b].scales + 12 + sb * 24, 12);
utmp_1[3] = ((utmp_1[2] >> 4) & kmask2) | (((utmp_1[1] >> 6) & kmask3) << 4);
const uint32_t uaux_1 = utmp_1[1] & kmask1;
utmp_1[1] = (utmp_1[2] & kmask2) | (((utmp_1[0] >> 6) & kmask3) << 4);
utmp_1[2] = uaux_1;
utmp_1[0] &= kmask1;
const __m128i mins_and_scales_0 = _mm_set_epi32(utmp_0[3], utmp_0[2], utmp_0[1], utmp_0[0]);
const __m256i scales_0 = _mm256_cvtepu8_epi16(_mm_unpacklo_epi8(mins_and_scales_0, mins_and_scales_0));
const __m128i mins_and_scales_1 = _mm_set_epi32(utmp_1[3], utmp_1[2], utmp_1[1], utmp_1[0]);
const __m256i scales_1 = _mm256_cvtepu8_epi16(_mm_unpacklo_epi8(mins_and_scales_1, mins_and_scales_1));
const __m256i mins_01 = _mm256_cvtepu8_epi16(_mm_unpacklo_epi8(_mm_shuffle_epi32(mins_and_scales_0, 78), _mm_shuffle_epi32(mins_and_scales_1, 78)));
const __m256i scale_0145_0 = _mm256_shuffle_epi32(scales_0, 68);
const __m256i scale_2367_0 = _mm256_shuffle_epi32(scales_0, 238);
const __m256i scale_0145_1 = _mm256_shuffle_epi32(scales_1, 68);
const __m256i scale_2367_1 = _mm256_shuffle_epi32(scales_1, 238);
for (int rp = 0; rp < 4; rp++) {
__m256i lhs_mat_0123_00 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 256 * sb)));
__m256i lhs_mat_01_00 = _mm256_permute2f128_si256(lhs_mat_0123_00, lhs_mat_0123_00, 0);
__m256i lhs_mat_23_00 = _mm256_permute2f128_si256(lhs_mat_0123_00, lhs_mat_0123_00, 17);
__m256i lhs_mat_0123_01 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 32 + 256 * sb)));
__m256i lhs_mat_01_01 = _mm256_permute2f128_si256(lhs_mat_0123_01, lhs_mat_0123_01, 0);
__m256i lhs_mat_23_01 = _mm256_permute2f128_si256(lhs_mat_0123_01, lhs_mat_0123_01, 17);
__m256i lhs_mat_0123_02 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 64 + 256 * sb)));
__m256i lhs_mat_01_02 = _mm256_permute2f128_si256(lhs_mat_0123_02, lhs_mat_0123_02, 0);
__m256i lhs_mat_23_02 = _mm256_permute2f128_si256(lhs_mat_0123_02, lhs_mat_0123_02, 17);
__m256i lhs_mat_0123_03 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 96 + 256 * sb)));
__m256i lhs_mat_01_03 = _mm256_permute2f128_si256(lhs_mat_0123_03, lhs_mat_0123_03, 0);
__m256i lhs_mat_23_03 = _mm256_permute2f128_si256(lhs_mat_0123_03, lhs_mat_0123_03, 17);
__m256i lhs_mat_0123_10 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 128 + 256 * sb)));
__m256i lhs_mat_01_10 = _mm256_permute2f128_si256(lhs_mat_0123_10, lhs_mat_0123_10, 0);
__m256i lhs_mat_23_10 = _mm256_permute2f128_si256(lhs_mat_0123_10, lhs_mat_0123_10, 17);
__m256i lhs_mat_0123_11 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 160 + 256 * sb)));
__m256i lhs_mat_01_11 = _mm256_permute2f128_si256(lhs_mat_0123_11, lhs_mat_0123_11, 0);
__m256i lhs_mat_23_11 = _mm256_permute2f128_si256(lhs_mat_0123_11, lhs_mat_0123_11, 17);
__m256i lhs_mat_0123_12 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 192 + 256 * sb)));
__m256i lhs_mat_01_12 = _mm256_permute2f128_si256(lhs_mat_0123_12, lhs_mat_0123_12, 0);
__m256i lhs_mat_23_12 = _mm256_permute2f128_si256(lhs_mat_0123_12, lhs_mat_0123_12, 17);
__m256i lhs_mat_0123_13 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].qs + 224 + 256 * sb)));
__m256i lhs_mat_01_13 = _mm256_permute2f128_si256(lhs_mat_0123_13, lhs_mat_0123_13, 0);
__m256i lhs_mat_23_13 = _mm256_permute2f128_si256(lhs_mat_0123_13, lhs_mat_0123_13, 17);
__m256i lhs_bsums_0123_01 = _mm256_loadu_si256((const __m256i * )((a_ptrs[rp][b].bsums + 16 * sb)));
__m256i lhs_bsums_hsum_0123_01 = _mm256_castsi128_si256(_mm_hadd_epi16(_mm256_castsi256_si128(lhs_bsums_0123_01), _mm256_extractf128_si256(lhs_bsums_0123_01, 1)));
lhs_bsums_hsum_0123_01 = _mm256_permute2x128_si256(lhs_bsums_hsum_0123_01, lhs_bsums_hsum_0123_01, 0);
const __m256i lhs_mat_01_00_sp1 = _mm256_shuffle_epi32(lhs_mat_01_00, 160);
const __m256i lhs_mat_23_00_sp1 = _mm256_shuffle_epi32(lhs_mat_23_00, 160);
const __m256i lhs_mat_01_01_sp1 = _mm256_shuffle_epi32(lhs_mat_01_01, 160);
const __m256i lhs_mat_23_01_sp1 = _mm256_shuffle_epi32(lhs_mat_23_01, 160);
const __m256i lhs_mat_01_02_sp1 = _mm256_shuffle_epi32(lhs_mat_01_02, 160);
const __m256i lhs_mat_23_02_sp1 = _mm256_shuffle_epi32(lhs_mat_23_02, 160);
const __m256i lhs_mat_01_03_sp1 = _mm256_shuffle_epi32(lhs_mat_01_03, 160);
const __m256i lhs_mat_23_03_sp1 = _mm256_shuffle_epi32(lhs_mat_23_03, 160);
const __m256i lhs_mat_01_10_sp1 = _mm256_shuffle_epi32(lhs_mat_01_10, 160);
const __m256i lhs_mat_23_10_sp1 = _mm256_shuffle_epi32(lhs_mat_23_10, 160);
const __m256i lhs_mat_01_11_sp1 = _mm256_shuffle_epi32(lhs_mat_01_11, 160);
const __m256i lhs_mat_23_11_sp1 = _mm256_shuffle_epi32(lhs_mat_23_11, 160);
const __m256i lhs_mat_01_12_sp1 = _mm256_shuffle_epi32(lhs_mat_01_12, 160);
const __m256i lhs_mat_23_12_sp1 = _mm256_shuffle_epi32(lhs_mat_23_12, 160);
const __m256i lhs_mat_01_13_sp1 = _mm256_shuffle_epi32(lhs_mat_01_13, 160);
const __m256i lhs_mat_23_13_sp1 = _mm256_shuffle_epi32(lhs_mat_23_13, 160);
const __m256i lhs_mat_01_00_sp2 = _mm256_shuffle_epi32(lhs_mat_01_00, 245);
const __m256i lhs_mat_23_00_sp2 = _mm256_shuffle_epi32(lhs_mat_23_00, 245);
const __m256i lhs_mat_01_01_sp2 = _mm256_shuffle_epi32(lhs_mat_01_01, 245);
const __m256i lhs_mat_23_01_sp2 = _mm256_shuffle_epi32(lhs_mat_23_01, 245);
const __m256i lhs_mat_01_02_sp2 = _mm256_shuffle_epi32(lhs_mat_01_02, 245);
const __m256i lhs_mat_23_02_sp2 = _mm256_shuffle_epi32(lhs_mat_23_02, 245);
const __m256i lhs_mat_01_03_sp2 = _mm256_shuffle_epi32(lhs_mat_01_03, 245);
const __m256i lhs_mat_23_03_sp2 = _mm256_shuffle_epi32(lhs_mat_23_03, 245);
const __m256i lhs_mat_01_10_sp2 = _mm256_shuffle_epi32(lhs_mat_01_10, 245);
const __m256i lhs_mat_23_10_sp2 = _mm256_shuffle_epi32(lhs_mat_23_10, 245);
const __m256i lhs_mat_01_11_sp2 = _mm256_shuffle_epi32(lhs_mat_01_11, 245);
const __m256i lhs_mat_23_11_sp2 = _mm256_shuffle_epi32(lhs_mat_23_11, 245);
const __m256i lhs_mat_01_12_sp2 = _mm256_shuffle_epi32(lhs_mat_01_12, 245);
const __m256i lhs_mat_23_12_sp2 = _mm256_shuffle_epi32(lhs_mat_23_12, 245);
const __m256i lhs_mat_01_13_sp2 = _mm256_shuffle_epi32(lhs_mat_01_13, 245);
const __m256i lhs_mat_23_13_sp2 = _mm256_shuffle_epi32(lhs_mat_23_13, 245);
__m256i iacc_mat_00_0_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_03_sp1, lhs_mat_01_03_sp1), _mm256_maddubs_epi16(rhs_mat_0145_02_sp1, lhs_mat_01_02_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_01_sp1, lhs_mat_01_01_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_00_sp1, lhs_mat_01_00_sp1));
__m256i iacc_mat_01_0_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_03_sp1, lhs_mat_01_03_sp1), _mm256_maddubs_epi16(rhs_mat_2367_02_sp1, lhs_mat_01_02_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_01_sp1, lhs_mat_01_01_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_00_sp1, lhs_mat_01_00_sp1));
__m256i iacc_mat_10_0_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_03_sp1, lhs_mat_23_03_sp1), _mm256_maddubs_epi16(rhs_mat_0145_02_sp1, lhs_mat_23_02_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_01_sp1, lhs_mat_23_01_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_00_sp1, lhs_mat_23_00_sp1));
__m256i iacc_mat_11_0_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_03_sp1, lhs_mat_23_03_sp1), _mm256_maddubs_epi16(rhs_mat_2367_02_sp1, lhs_mat_23_02_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_01_sp1, lhs_mat_23_01_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_00_sp1, lhs_mat_23_00_sp1));
__m256i iacc_mat_00_1_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_13_sp1, lhs_mat_01_13_sp1), _mm256_maddubs_epi16(rhs_mat_0145_12_sp1, lhs_mat_01_12_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_11_sp1, lhs_mat_01_11_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_10_sp1, lhs_mat_01_10_sp1));
__m256i iacc_mat_01_1_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_13_sp1, lhs_mat_01_13_sp1), _mm256_maddubs_epi16(rhs_mat_2367_12_sp1, lhs_mat_01_12_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_11_sp1, lhs_mat_01_11_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_10_sp1, lhs_mat_01_10_sp1));
__m256i iacc_mat_10_1_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_13_sp1, lhs_mat_23_13_sp1), _mm256_maddubs_epi16(rhs_mat_0145_12_sp1, lhs_mat_23_12_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_11_sp1, lhs_mat_23_11_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_10_sp1, lhs_mat_23_10_sp1));
__m256i iacc_mat_11_1_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_13_sp1, lhs_mat_23_13_sp1), _mm256_maddubs_epi16(rhs_mat_2367_12_sp1, lhs_mat_23_12_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_11_sp1, lhs_mat_23_11_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_10_sp1, lhs_mat_23_10_sp1));
__m256i iacc_mat_00_0_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_03_sp2, lhs_mat_01_03_sp2), _mm256_maddubs_epi16(rhs_mat_0145_02_sp2, lhs_mat_01_02_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_01_sp2, lhs_mat_01_01_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_00_sp2, lhs_mat_01_00_sp2));
__m256i iacc_mat_01_0_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_03_sp2, lhs_mat_01_03_sp2), _mm256_maddubs_epi16(rhs_mat_2367_02_sp2, lhs_mat_01_02_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_01_sp2, lhs_mat_01_01_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_00_sp2, lhs_mat_01_00_sp2));
__m256i iacc_mat_10_0_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_03_sp2, lhs_mat_23_03_sp2), _mm256_maddubs_epi16(rhs_mat_0145_02_sp2, lhs_mat_23_02_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_01_sp2, lhs_mat_23_01_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_00_sp2, lhs_mat_23_00_sp2));
__m256i iacc_mat_11_0_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_03_sp2, lhs_mat_23_03_sp2), _mm256_maddubs_epi16(rhs_mat_2367_02_sp2, lhs_mat_23_02_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_01_sp2, lhs_mat_23_01_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_00_sp2, lhs_mat_23_00_sp2));
__m256i iacc_mat_00_1_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_13_sp2, lhs_mat_01_13_sp2), _mm256_maddubs_epi16(rhs_mat_0145_12_sp2, lhs_mat_01_12_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_11_sp2, lhs_mat_01_11_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_10_sp2, lhs_mat_01_10_sp2));
__m256i iacc_mat_01_1_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_13_sp2, lhs_mat_01_13_sp2), _mm256_maddubs_epi16(rhs_mat_2367_12_sp2, lhs_mat_01_12_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_11_sp2, lhs_mat_01_11_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_10_sp2, lhs_mat_01_10_sp2));
__m256i iacc_mat_10_1_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_13_sp2, lhs_mat_23_13_sp2), _mm256_maddubs_epi16(rhs_mat_0145_12_sp2, lhs_mat_23_12_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_11_sp2, lhs_mat_23_11_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_10_sp2, lhs_mat_23_10_sp2));
__m256i iacc_mat_11_1_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_13_sp2, lhs_mat_23_13_sp2), _mm256_maddubs_epi16(rhs_mat_2367_12_sp2, lhs_mat_23_12_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_11_sp2, lhs_mat_23_11_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_10_sp2, lhs_mat_23_10_sp2));
__m256i iacc_mat_00_0 = _mm256_add_epi16(iacc_mat_00_0_sp1, iacc_mat_00_0_sp2);
__m256i iacc_mat_01_0 = _mm256_add_epi16(iacc_mat_01_0_sp1, iacc_mat_01_0_sp2);
__m256i iacc_mat_10_0 = _mm256_add_epi16(iacc_mat_10_0_sp1, iacc_mat_10_0_sp2);
__m256i iacc_mat_11_0 = _mm256_add_epi16(iacc_mat_11_0_sp1, iacc_mat_11_0_sp2);
__m256i iacc_mat_00_1 = _mm256_add_epi16(iacc_mat_00_1_sp1, iacc_mat_00_1_sp2);
__m256i iacc_mat_01_1 = _mm256_add_epi16(iacc_mat_01_1_sp1, iacc_mat_01_1_sp2);
__m256i iacc_mat_10_1 = _mm256_add_epi16(iacc_mat_10_1_sp1, iacc_mat_10_1_sp2);
__m256i iacc_mat_11_1 = _mm256_add_epi16(iacc_mat_11_1_sp1, iacc_mat_11_1_sp2);
iacc_mat_00_0 = _mm256_madd_epi16(iacc_mat_00_0, scale_0145_0);
iacc_mat_01_0 = _mm256_madd_epi16(iacc_mat_01_0, scale_2367_0);
iacc_mat_10_0 = _mm256_madd_epi16(iacc_mat_10_0, scale_0145_0);
iacc_mat_11_0 = _mm256_madd_epi16(iacc_mat_11_0, scale_2367_0);
iacc_mat_00_1 = _mm256_madd_epi16(iacc_mat_00_1, scale_0145_1);
iacc_mat_01_1 = _mm256_madd_epi16(iacc_mat_01_1, scale_2367_1);
iacc_mat_10_1 = _mm256_madd_epi16(iacc_mat_10_1, scale_0145_1);
iacc_mat_11_1 = _mm256_madd_epi16(iacc_mat_11_1, scale_2367_1);
__m256i iacc_row_0_0 = _mm256_blend_epi32(iacc_mat_00_0, _mm256_shuffle_epi32(iacc_mat_01_0, 78), 204);
__m256i iacc_row_1_0 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_00_0, 78), iacc_mat_01_0, 204);
__m256i iacc_row_2_0 = _mm256_blend_epi32(iacc_mat_10_0, _mm256_shuffle_epi32(iacc_mat_11_0, 78), 204);
__m256i iacc_row_3_0 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_10_0, 78), iacc_mat_11_0, 204);
__m256i iacc_row_0_1 = _mm256_blend_epi32(iacc_mat_00_1, _mm256_shuffle_epi32(iacc_mat_01_1, 78), 204);
__m256i iacc_row_1_1 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_00_1, 78), iacc_mat_01_1, 204);
__m256i iacc_row_2_1 = _mm256_blend_epi32(iacc_mat_10_1, _mm256_shuffle_epi32(iacc_mat_11_1, 78), 204);
__m256i iacc_row_3_1 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_10_1, 78), iacc_mat_11_1, 204);
__m256i iacc_row_0 = _mm256_add_epi32(iacc_row_0_0, iacc_row_0_1);
__m256i iacc_row_1 = _mm256_add_epi32(iacc_row_1_0, iacc_row_1_1);
__m256i iacc_row_2 = _mm256_add_epi32(iacc_row_2_0, iacc_row_2_1);
__m256i iacc_row_3 = _mm256_add_epi32(iacc_row_3_0, iacc_row_3_1);
const __m128 row_scale_f32_sse = _mm_load_ps(a_ptrs[rp][b].d);
const __m256 row_scale_f32 = _mm256_set_m128(row_scale_f32_sse, row_scale_f32_sse);
acc_rows[rp * 4] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_0), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_rows[rp * 4]);
acc_rows[rp * 4 + 1] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_1), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_rows[rp * 4 + 1]);
acc_rows[rp * 4 + 2] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_2), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_rows[rp * 4 + 2]);
acc_rows[rp * 4 + 3] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_3), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_rows[rp * 4 + 3]);
__m256i iacc_row_min_0 = _mm256_madd_epi16(_mm256_shuffle_epi32(lhs_bsums_hsum_0123_01, 0), mins_01);
__m256i iacc_row_min_1 = _mm256_madd_epi16(_mm256_shuffle_epi32(lhs_bsums_hsum_0123_01, 85), mins_01);
__m256i iacc_row_min_2 = _mm256_madd_epi16(_mm256_shuffle_epi32(lhs_bsums_hsum_0123_01, 170), mins_01);
__m256i iacc_row_min_3 = _mm256_madd_epi16(_mm256_shuffle_epi32(lhs_bsums_hsum_0123_01, 255), mins_01);
acc_min_rows[rp * 4] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_min_0), _mm256_mul_ps(col_dmin_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_min_rows[rp * 4]);
acc_min_rows[rp * 4 + 1] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_min_1), _mm256_mul_ps(col_dmin_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_min_rows[rp * 4 + 1]);
acc_min_rows[rp * 4 + 2] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_min_2), _mm256_mul_ps(col_dmin_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_min_rows[rp * 4 + 2]);
acc_min_rows[rp * 4 + 3] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_min_3), _mm256_mul_ps(col_dmin_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_min_rows[rp * 4 + 3]);
}
}
}
for (int i = 0; i < 16; i++) {
_mm256_storeu_ps((float * )(s + ((y * 4 + i) * bs + x * 8)), _mm256_sub_ps(acc_rows[i], acc_min_rows[i]));
}
}
}
for (; y < nr / 4; y++) {
const block_q8_Kx4 * a_ptr = a_ptr_start + (y * nb);
for (int64_t x = xstart; x < nc / 8; x++) {
const block_q4_Kx8 * b_ptr = b_ptr_start + (x * b_nb);
__m256 acc_rows[4];
for (int i = 0; i < 4; i++) {
acc_rows[i] = _mm256_setzero_ps();
}
__m256 acc_min_rows[4];
for (int i = 0; i < 4; i++) {
acc_min_rows[i] = _mm256_setzero_ps();
}
for (int64_t b = 0; b < nb; b++) {
const __m256 col_scale_f32 = GGML_F32Cx8_LOAD(b_ptr[b].d);
const __m256 col_dmin_f32 = GGML_F32Cx8_LOAD(b_ptr[b].dmin);
for (int sb = 0; sb < QK_K / 64; sb++) {
const __m256i rhs_raw_mat_0123_0 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + sb * 256));
const __m256i rhs_raw_mat_4567_0 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 32 + sb * 256));
const __m256i rhs_raw_mat_0123_1 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 64 + sb * 256));
const __m256i rhs_raw_mat_4567_1 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 96 + sb * 256));
const __m256i rhs_raw_mat_0123_2 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 128 + sb * 256));
const __m256i rhs_raw_mat_4567_2 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 160 + sb * 256));
const __m256i rhs_raw_mat_0123_3 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 192 + sb * 256));
const __m256i rhs_raw_mat_4567_3 = _mm256_loadu_si256((const __m256i * )(b_ptr[b].qs + 224 + sb * 256));
const __m256i rhs_raw_mat_0145_0 = _mm256_blend_epi32(rhs_raw_mat_0123_0, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_0, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_0 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_0, requiredOrder), rhs_raw_mat_4567_0, 240);
const __m256i rhs_raw_mat_0145_1 = _mm256_blend_epi32(rhs_raw_mat_0123_1, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_1, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_1 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_1, requiredOrder), rhs_raw_mat_4567_1, 240);
const __m256i rhs_raw_mat_0145_2 = _mm256_blend_epi32(rhs_raw_mat_0123_2, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_2, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_2 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_2, requiredOrder), rhs_raw_mat_4567_2, 240);
const __m256i rhs_raw_mat_0145_3 = _mm256_blend_epi32(rhs_raw_mat_0123_3, _mm256_permutevar8x32_epi32(rhs_raw_mat_4567_3, requiredOrder), 240);
const __m256i rhs_raw_mat_2367_3 = _mm256_blend_epi32(_mm256_permutevar8x32_epi32(rhs_raw_mat_0123_3, requiredOrder), rhs_raw_mat_4567_3, 240);
const __m256i rhs_mat_0145_00 = _mm256_and_si256(rhs_raw_mat_0145_0, m4b);
const __m256i rhs_mat_2367_00 = _mm256_and_si256(rhs_raw_mat_2367_0, m4b);
const __m256i rhs_mat_0145_01 = _mm256_and_si256(rhs_raw_mat_0145_1, m4b);
const __m256i rhs_mat_2367_01 = _mm256_and_si256(rhs_raw_mat_2367_1, m4b);
const __m256i rhs_mat_0145_02 = _mm256_and_si256(rhs_raw_mat_0145_2, m4b);
const __m256i rhs_mat_2367_02 = _mm256_and_si256(rhs_raw_mat_2367_2, m4b);
const __m256i rhs_mat_0145_03 = _mm256_and_si256(rhs_raw_mat_0145_3, m4b);
const __m256i rhs_mat_2367_03 = _mm256_and_si256(rhs_raw_mat_2367_3, m4b);
const __m256i rhs_mat_0145_10 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_0, 4), m4b);
const __m256i rhs_mat_2367_10 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_0, 4), m4b);
const __m256i rhs_mat_0145_11 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_1, 4), m4b);
const __m256i rhs_mat_2367_11 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_1, 4), m4b);
const __m256i rhs_mat_0145_12 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_2, 4), m4b);
const __m256i rhs_mat_2367_12 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_2, 4), m4b);
const __m256i rhs_mat_0145_13 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_0145_3, 4), m4b);
const __m256i rhs_mat_2367_13 = _mm256_and_si256(_mm256_srli_epi16(rhs_raw_mat_2367_3, 4), m4b);
const __m256i rhs_mat_0145_00_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_00, 136);
const __m256i rhs_mat_2367_00_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_00, 136);
const __m256i rhs_mat_0145_01_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_01, 136);
const __m256i rhs_mat_2367_01_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_01, 136);
const __m256i rhs_mat_0145_02_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_02, 136);
const __m256i rhs_mat_2367_02_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_02, 136);
const __m256i rhs_mat_0145_03_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_03, 136);
const __m256i rhs_mat_2367_03_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_03, 136);
const __m256i rhs_mat_0145_10_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_10, 136);
const __m256i rhs_mat_2367_10_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_10, 136);
const __m256i rhs_mat_0145_11_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_11, 136);
const __m256i rhs_mat_2367_11_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_11, 136);
const __m256i rhs_mat_0145_12_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_12, 136);
const __m256i rhs_mat_2367_12_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_12, 136);
const __m256i rhs_mat_0145_13_sp1 = _mm256_shuffle_epi32(rhs_mat_0145_13, 136);
const __m256i rhs_mat_2367_13_sp1 = _mm256_shuffle_epi32(rhs_mat_2367_13, 136);
const __m256i rhs_mat_0145_00_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_00, 221);
const __m256i rhs_mat_2367_00_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_00, 221);
const __m256i rhs_mat_0145_01_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_01, 221);
const __m256i rhs_mat_2367_01_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_01, 221);
const __m256i rhs_mat_0145_02_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_02, 221);
const __m256i rhs_mat_2367_02_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_02, 221);
const __m256i rhs_mat_0145_03_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_03, 221);
const __m256i rhs_mat_2367_03_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_03, 221);
const __m256i rhs_mat_0145_10_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_10, 221);
const __m256i rhs_mat_2367_10_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_10, 221);
const __m256i rhs_mat_0145_11_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_11, 221);
const __m256i rhs_mat_2367_11_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_11, 221);
const __m256i rhs_mat_0145_12_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_12, 221);
const __m256i rhs_mat_2367_12_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_12, 221);
const __m256i rhs_mat_0145_13_sp2 = _mm256_shuffle_epi32(rhs_mat_0145_13, 221);
const __m256i rhs_mat_2367_13_sp2 = _mm256_shuffle_epi32(rhs_mat_2367_13, 221);
uint32_t utmp_0[4], utmp_1[4];
memcpy(utmp_0, b_ptr[b].scales + 24 * sb, 12);
utmp_0[3] = ((utmp_0[2] >> 4) & kmask2) | (((utmp_0[1] >> 6) & kmask3) << 4);
const uint32_t uaux_0 = utmp_0[1] & kmask1;
utmp_0[1] = (utmp_0[2] & kmask2) | (((utmp_0[0] >> 6) & kmask3) << 4);
utmp_0[2] = uaux_0;
utmp_0[0] &= kmask1;
memcpy(utmp_1, b_ptr[b].scales + 12 + sb * 24, 12);
utmp_1[3] = ((utmp_1[2] >> 4) & kmask2) | (((utmp_1[1] >> 6) & kmask3) << 4);
const uint32_t uaux_1 = utmp_1[1] & kmask1;
utmp_1[1] = (utmp_1[2] & kmask2) | (((utmp_1[0] >> 6) & kmask3) << 4);
utmp_1[2] = uaux_1;
utmp_1[0] &= kmask1;
const __m128i mins_and_scales_0 = _mm_set_epi32(utmp_0[3], utmp_0[2], utmp_0[1], utmp_0[0]);
const __m256i scales_0 = _mm256_cvtepu8_epi16(_mm_unpacklo_epi8(mins_and_scales_0, mins_and_scales_0));
const __m128i mins_and_scales_1 = _mm_set_epi32(utmp_1[3], utmp_1[2], utmp_1[1], utmp_1[0]);
const __m256i scales_1 = _mm256_cvtepu8_epi16(_mm_unpacklo_epi8(mins_and_scales_1, mins_and_scales_1));
const __m256i mins_01 = _mm256_cvtepu8_epi16(_mm_unpacklo_epi8(_mm_shuffle_epi32(mins_and_scales_0, 78), _mm_shuffle_epi32(mins_and_scales_1, 78)));
const __m256i scale_0145_0 = _mm256_shuffle_epi32(scales_0, 68);
const __m256i scale_2367_0 = _mm256_shuffle_epi32(scales_0, 238);
const __m256i scale_0145_1 = _mm256_shuffle_epi32(scales_1, 68);
const __m256i scale_2367_1 = _mm256_shuffle_epi32(scales_1, 238);
__m256i lhs_mat_0123_00 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 256 * sb)));
__m256i lhs_mat_01_00 = _mm256_permute2f128_si256(lhs_mat_0123_00, lhs_mat_0123_00, 0);
__m256i lhs_mat_23_00 = _mm256_permute2f128_si256(lhs_mat_0123_00, lhs_mat_0123_00, 17);
__m256i lhs_mat_0123_01 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 32 + 256 * sb)));
__m256i lhs_mat_01_01 = _mm256_permute2f128_si256(lhs_mat_0123_01, lhs_mat_0123_01, 0);
__m256i lhs_mat_23_01 = _mm256_permute2f128_si256(lhs_mat_0123_01, lhs_mat_0123_01, 17);
__m256i lhs_mat_0123_02 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 64 + 256 * sb)));
__m256i lhs_mat_01_02 = _mm256_permute2f128_si256(lhs_mat_0123_02, lhs_mat_0123_02, 0);
__m256i lhs_mat_23_02 = _mm256_permute2f128_si256(lhs_mat_0123_02, lhs_mat_0123_02, 17);
__m256i lhs_mat_0123_03 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 96 + 256 * sb)));
__m256i lhs_mat_01_03 = _mm256_permute2f128_si256(lhs_mat_0123_03, lhs_mat_0123_03, 0);
__m256i lhs_mat_23_03 = _mm256_permute2f128_si256(lhs_mat_0123_03, lhs_mat_0123_03, 17);
__m256i lhs_mat_0123_10 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 128 + 256 * sb)));
__m256i lhs_mat_01_10 = _mm256_permute2f128_si256(lhs_mat_0123_10, lhs_mat_0123_10, 0);
__m256i lhs_mat_23_10 = _mm256_permute2f128_si256(lhs_mat_0123_10, lhs_mat_0123_10, 17);
__m256i lhs_mat_0123_11 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 160 + 256 * sb)));
__m256i lhs_mat_01_11 = _mm256_permute2f128_si256(lhs_mat_0123_11, lhs_mat_0123_11, 0);
__m256i lhs_mat_23_11 = _mm256_permute2f128_si256(lhs_mat_0123_11, lhs_mat_0123_11, 17);
__m256i lhs_mat_0123_12 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 192 + 256 * sb)));
__m256i lhs_mat_01_12 = _mm256_permute2f128_si256(lhs_mat_0123_12, lhs_mat_0123_12, 0);
__m256i lhs_mat_23_12 = _mm256_permute2f128_si256(lhs_mat_0123_12, lhs_mat_0123_12, 17);
__m256i lhs_mat_0123_13 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].qs + 224 + 256 * sb)));
__m256i lhs_mat_01_13 = _mm256_permute2f128_si256(lhs_mat_0123_13, lhs_mat_0123_13, 0);
__m256i lhs_mat_23_13 = _mm256_permute2f128_si256(lhs_mat_0123_13, lhs_mat_0123_13, 17);
__m256i lhs_bsums_0123_01 = _mm256_loadu_si256((const __m256i * )((a_ptr[b].bsums + 16 * sb)));
__m256i lhs_bsums_hsum_0123_01 = _mm256_castsi128_si256(_mm_hadd_epi16(_mm256_castsi256_si128(lhs_bsums_0123_01), _mm256_extractf128_si256(lhs_bsums_0123_01, 1)));
lhs_bsums_hsum_0123_01 = _mm256_permute2x128_si256(lhs_bsums_hsum_0123_01, lhs_bsums_hsum_0123_01, 0);
const __m256i lhs_mat_01_00_sp1 = _mm256_shuffle_epi32(lhs_mat_01_00, 160);
const __m256i lhs_mat_23_00_sp1 = _mm256_shuffle_epi32(lhs_mat_23_00, 160);
const __m256i lhs_mat_01_01_sp1 = _mm256_shuffle_epi32(lhs_mat_01_01, 160);
const __m256i lhs_mat_23_01_sp1 = _mm256_shuffle_epi32(lhs_mat_23_01, 160);
const __m256i lhs_mat_01_02_sp1 = _mm256_shuffle_epi32(lhs_mat_01_02, 160);
const __m256i lhs_mat_23_02_sp1 = _mm256_shuffle_epi32(lhs_mat_23_02, 160);
const __m256i lhs_mat_01_03_sp1 = _mm256_shuffle_epi32(lhs_mat_01_03, 160);
const __m256i lhs_mat_23_03_sp1 = _mm256_shuffle_epi32(lhs_mat_23_03, 160);
const __m256i lhs_mat_01_10_sp1 = _mm256_shuffle_epi32(lhs_mat_01_10, 160);
const __m256i lhs_mat_23_10_sp1 = _mm256_shuffle_epi32(lhs_mat_23_10, 160);
const __m256i lhs_mat_01_11_sp1 = _mm256_shuffle_epi32(lhs_mat_01_11, 160);
const __m256i lhs_mat_23_11_sp1 = _mm256_shuffle_epi32(lhs_mat_23_11, 160);
const __m256i lhs_mat_01_12_sp1 = _mm256_shuffle_epi32(lhs_mat_01_12, 160);
const __m256i lhs_mat_23_12_sp1 = _mm256_shuffle_epi32(lhs_mat_23_12, 160);
const __m256i lhs_mat_01_13_sp1 = _mm256_shuffle_epi32(lhs_mat_01_13, 160);
const __m256i lhs_mat_23_13_sp1 = _mm256_shuffle_epi32(lhs_mat_23_13, 160);
const __m256i lhs_mat_01_00_sp2 = _mm256_shuffle_epi32(lhs_mat_01_00, 245);
const __m256i lhs_mat_23_00_sp2 = _mm256_shuffle_epi32(lhs_mat_23_00, 245);
const __m256i lhs_mat_01_01_sp2 = _mm256_shuffle_epi32(lhs_mat_01_01, 245);
const __m256i lhs_mat_23_01_sp2 = _mm256_shuffle_epi32(lhs_mat_23_01, 245);
const __m256i lhs_mat_01_02_sp2 = _mm256_shuffle_epi32(lhs_mat_01_02, 245);
const __m256i lhs_mat_23_02_sp2 = _mm256_shuffle_epi32(lhs_mat_23_02, 245);
const __m256i lhs_mat_01_03_sp2 = _mm256_shuffle_epi32(lhs_mat_01_03, 245);
const __m256i lhs_mat_23_03_sp2 = _mm256_shuffle_epi32(lhs_mat_23_03, 245);
const __m256i lhs_mat_01_10_sp2 = _mm256_shuffle_epi32(lhs_mat_01_10, 245);
const __m256i lhs_mat_23_10_sp2 = _mm256_shuffle_epi32(lhs_mat_23_10, 245);
const __m256i lhs_mat_01_11_sp2 = _mm256_shuffle_epi32(lhs_mat_01_11, 245);
const __m256i lhs_mat_23_11_sp2 = _mm256_shuffle_epi32(lhs_mat_23_11, 245);
const __m256i lhs_mat_01_12_sp2 = _mm256_shuffle_epi32(lhs_mat_01_12, 245);
const __m256i lhs_mat_23_12_sp2 = _mm256_shuffle_epi32(lhs_mat_23_12, 245);
const __m256i lhs_mat_01_13_sp2 = _mm256_shuffle_epi32(lhs_mat_01_13, 245);
const __m256i lhs_mat_23_13_sp2 = _mm256_shuffle_epi32(lhs_mat_23_13, 245);
__m256i iacc_mat_00_0_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_03_sp1, lhs_mat_01_03_sp1), _mm256_maddubs_epi16(rhs_mat_0145_02_sp1, lhs_mat_01_02_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_01_sp1, lhs_mat_01_01_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_00_sp1, lhs_mat_01_00_sp1));
__m256i iacc_mat_01_0_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_03_sp1, lhs_mat_01_03_sp1), _mm256_maddubs_epi16(rhs_mat_2367_02_sp1, lhs_mat_01_02_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_01_sp1, lhs_mat_01_01_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_00_sp1, lhs_mat_01_00_sp1));
__m256i iacc_mat_10_0_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_03_sp1, lhs_mat_23_03_sp1), _mm256_maddubs_epi16(rhs_mat_0145_02_sp1, lhs_mat_23_02_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_01_sp1, lhs_mat_23_01_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_00_sp1, lhs_mat_23_00_sp1));
__m256i iacc_mat_11_0_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_03_sp1, lhs_mat_23_03_sp1), _mm256_maddubs_epi16(rhs_mat_2367_02_sp1, lhs_mat_23_02_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_01_sp1, lhs_mat_23_01_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_00_sp1, lhs_mat_23_00_sp1));
__m256i iacc_mat_00_1_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_13_sp1, lhs_mat_01_13_sp1), _mm256_maddubs_epi16(rhs_mat_0145_12_sp1, lhs_mat_01_12_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_11_sp1, lhs_mat_01_11_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_10_sp1, lhs_mat_01_10_sp1));
__m256i iacc_mat_01_1_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_13_sp1, lhs_mat_01_13_sp1), _mm256_maddubs_epi16(rhs_mat_2367_12_sp1, lhs_mat_01_12_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_11_sp1, lhs_mat_01_11_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_10_sp1, lhs_mat_01_10_sp1));
__m256i iacc_mat_10_1_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_13_sp1, lhs_mat_23_13_sp1), _mm256_maddubs_epi16(rhs_mat_0145_12_sp1, lhs_mat_23_12_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_11_sp1, lhs_mat_23_11_sp1)), _mm256_maddubs_epi16(rhs_mat_0145_10_sp1, lhs_mat_23_10_sp1));
__m256i iacc_mat_11_1_sp1 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_13_sp1, lhs_mat_23_13_sp1), _mm256_maddubs_epi16(rhs_mat_2367_12_sp1, lhs_mat_23_12_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_11_sp1, lhs_mat_23_11_sp1)), _mm256_maddubs_epi16(rhs_mat_2367_10_sp1, lhs_mat_23_10_sp1));
__m256i iacc_mat_00_0_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_03_sp2, lhs_mat_01_03_sp2), _mm256_maddubs_epi16(rhs_mat_0145_02_sp2, lhs_mat_01_02_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_01_sp2, lhs_mat_01_01_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_00_sp2, lhs_mat_01_00_sp2));
__m256i iacc_mat_01_0_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_03_sp2, lhs_mat_01_03_sp2), _mm256_maddubs_epi16(rhs_mat_2367_02_sp2, lhs_mat_01_02_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_01_sp2, lhs_mat_01_01_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_00_sp2, lhs_mat_01_00_sp2));
__m256i iacc_mat_10_0_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_03_sp2, lhs_mat_23_03_sp2), _mm256_maddubs_epi16(rhs_mat_0145_02_sp2, lhs_mat_23_02_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_01_sp2, lhs_mat_23_01_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_00_sp2, lhs_mat_23_00_sp2));
__m256i iacc_mat_11_0_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_03_sp2, lhs_mat_23_03_sp2), _mm256_maddubs_epi16(rhs_mat_2367_02_sp2, lhs_mat_23_02_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_01_sp2, lhs_mat_23_01_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_00_sp2, lhs_mat_23_00_sp2));
__m256i iacc_mat_00_1_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_13_sp2, lhs_mat_01_13_sp2), _mm256_maddubs_epi16(rhs_mat_0145_12_sp2, lhs_mat_01_12_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_11_sp2, lhs_mat_01_11_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_10_sp2, lhs_mat_01_10_sp2));
__m256i iacc_mat_01_1_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_13_sp2, lhs_mat_01_13_sp2), _mm256_maddubs_epi16(rhs_mat_2367_12_sp2, lhs_mat_01_12_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_11_sp2, lhs_mat_01_11_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_10_sp2, lhs_mat_01_10_sp2));
__m256i iacc_mat_10_1_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_0145_13_sp2, lhs_mat_23_13_sp2), _mm256_maddubs_epi16(rhs_mat_0145_12_sp2, lhs_mat_23_12_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_11_sp2, lhs_mat_23_11_sp2)), _mm256_maddubs_epi16(rhs_mat_0145_10_sp2, lhs_mat_23_10_sp2));
__m256i iacc_mat_11_1_sp2 = _mm256_add_epi16(_mm256_add_epi16(_mm256_add_epi16(_mm256_maddubs_epi16(rhs_mat_2367_13_sp2, lhs_mat_23_13_sp2), _mm256_maddubs_epi16(rhs_mat_2367_12_sp2, lhs_mat_23_12_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_11_sp2, lhs_mat_23_11_sp2)), _mm256_maddubs_epi16(rhs_mat_2367_10_sp2, lhs_mat_23_10_sp2));
__m256i iacc_mat_00_0 = _mm256_add_epi16(iacc_mat_00_0_sp1, iacc_mat_00_0_sp2);
__m256i iacc_mat_01_0 = _mm256_add_epi16(iacc_mat_01_0_sp1, iacc_mat_01_0_sp2);
__m256i iacc_mat_10_0 = _mm256_add_epi16(iacc_mat_10_0_sp1, iacc_mat_10_0_sp2);
__m256i iacc_mat_11_0 = _mm256_add_epi16(iacc_mat_11_0_sp1, iacc_mat_11_0_sp2);
__m256i iacc_mat_00_1 = _mm256_add_epi16(iacc_mat_00_1_sp1, iacc_mat_00_1_sp2);
__m256i iacc_mat_01_1 = _mm256_add_epi16(iacc_mat_01_1_sp1, iacc_mat_01_1_sp2);
__m256i iacc_mat_10_1 = _mm256_add_epi16(iacc_mat_10_1_sp1, iacc_mat_10_1_sp2);
__m256i iacc_mat_11_1 = _mm256_add_epi16(iacc_mat_11_1_sp1, iacc_mat_11_1_sp2);
iacc_mat_00_0 = _mm256_madd_epi16(iacc_mat_00_0, scale_0145_0);
iacc_mat_01_0 = _mm256_madd_epi16(iacc_mat_01_0, scale_2367_0);
iacc_mat_10_0 = _mm256_madd_epi16(iacc_mat_10_0, scale_0145_0);
iacc_mat_11_0 = _mm256_madd_epi16(iacc_mat_11_0, scale_2367_0);
iacc_mat_00_1 = _mm256_madd_epi16(iacc_mat_00_1, scale_0145_1);
iacc_mat_01_1 = _mm256_madd_epi16(iacc_mat_01_1, scale_2367_1);
iacc_mat_10_1 = _mm256_madd_epi16(iacc_mat_10_1, scale_0145_1);
iacc_mat_11_1 = _mm256_madd_epi16(iacc_mat_11_1, scale_2367_1);
__m256i iacc_row_0_0 = _mm256_blend_epi32(iacc_mat_00_0, _mm256_shuffle_epi32(iacc_mat_01_0, 78), 204);
__m256i iacc_row_1_0 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_00_0, 78), iacc_mat_01_0, 204);
__m256i iacc_row_2_0 = _mm256_blend_epi32(iacc_mat_10_0, _mm256_shuffle_epi32(iacc_mat_11_0, 78), 204);
__m256i iacc_row_3_0 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_10_0, 78), iacc_mat_11_0, 204);
__m256i iacc_row_0_1 = _mm256_blend_epi32(iacc_mat_00_1, _mm256_shuffle_epi32(iacc_mat_01_1, 78), 204);
__m256i iacc_row_1_1 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_00_1, 78), iacc_mat_01_1, 204);
__m256i iacc_row_2_1 = _mm256_blend_epi32(iacc_mat_10_1, _mm256_shuffle_epi32(iacc_mat_11_1, 78), 204);
__m256i iacc_row_3_1 = _mm256_blend_epi32(_mm256_shuffle_epi32(iacc_mat_10_1, 78), iacc_mat_11_1, 204);
__m256i iacc_row_0 = _mm256_add_epi32(iacc_row_0_0, iacc_row_0_1);
__m256i iacc_row_1 = _mm256_add_epi32(iacc_row_1_0, iacc_row_1_1);
__m256i iacc_row_2 = _mm256_add_epi32(iacc_row_2_0, iacc_row_2_1);
__m256i iacc_row_3 = _mm256_add_epi32(iacc_row_3_0, iacc_row_3_1);
const __m128 row_scale_f32_sse = _mm_load_ps(a_ptr[b].d);
const __m256 row_scale_f32 = _mm256_set_m128(row_scale_f32_sse, row_scale_f32_sse);
acc_rows[0] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_0), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_rows[0]);
acc_rows[1] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_1), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_rows[1]);
acc_rows[2] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_2), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_rows[2]);
acc_rows[3] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_3), _mm256_mul_ps(col_scale_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_rows[3]);
__m256i iacc_row_min_0 = _mm256_madd_epi16(_mm256_shuffle_epi32(lhs_bsums_hsum_0123_01, 0), mins_01);
__m256i iacc_row_min_1 = _mm256_madd_epi16(_mm256_shuffle_epi32(lhs_bsums_hsum_0123_01, 85), mins_01);
__m256i iacc_row_min_2 = _mm256_madd_epi16(_mm256_shuffle_epi32(lhs_bsums_hsum_0123_01, 170), mins_01);
__m256i iacc_row_min_3 = _mm256_madd_epi16(_mm256_shuffle_epi32(lhs_bsums_hsum_0123_01, 255), mins_01);
acc_min_rows[0] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_min_0), _mm256_mul_ps(col_dmin_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 0)), acc_min_rows[0]);
acc_min_rows[1] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_min_1), _mm256_mul_ps(col_dmin_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 85)), acc_min_rows[1]);
acc_min_rows[2] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_min_2), _mm256_mul_ps(col_dmin_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 170)), acc_min_rows[2]);
acc_min_rows[3] = _mm256_fmadd_ps(_mm256_cvtepi32_ps(iacc_row_min_3), _mm256_mul_ps(col_dmin_f32, _mm256_shuffle_ps(row_scale_f32, row_scale_f32, 255)), acc_min_rows[3]);
}
}
for (int i = 0; i < 4; i++) {
_mm256_storeu_ps((float * )(s + ((y * 4 + i) * bs + x * 8)), _mm256_sub_ps(acc_rows[i], acc_min_rows[i]));
}
}
}
#else
float sumf[4][8];
float sum_minf[4][8];
uint32_t utmp[32];
int sumi1;
int sumi2;
int sumi;
for (int y = 0; y < nr / 4; y++) {
const block_q8_Kx4 * a_ptr = (const block_q8_Kx4 *) vy + (y * nb);
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_q4_Kx8 * b_ptr = (const block_q4_Kx8 *) vx + (x * nb);
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumf[m][j] = 0.0;
sum_minf[m][j] = 0.0;
}
}
for (int l = 0; l < nb; l++) {
for (int sb = 0; sb < 8; sb++) {
memcpy(utmp + sb * 4, b_ptr[l].scales + sb * 12, 12);
utmp[sb * 4 + 3] = ((utmp[sb * 4 + 2] >> 4) & kmask2) | (((utmp[sb * 4 + 1] >> 6) & kmask3) << 4);
const uint32_t uaux_0 = utmp[sb * 4 + 1] & kmask1;
utmp[sb * 4 + 1] = (utmp[sb * 4 + 2] & kmask2) | (((utmp[sb * 4 + 0] >> 6) & kmask3) << 4);
utmp[sb * 4 + 2] = uaux_0;
utmp[sb * 4 + 0] &= kmask1;
}
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
uint8_t *scales_0 = (uint8_t*) utmp + (k / 4) * 32;
uint8_t *scales_1 = (uint8_t*) utmp + (k / 4) * 32 + 16;
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi1 = 0;
sumi2 = 0;
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0xF);
const int v1 = (int8_t) (b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] >> 4);
sumi1 = (v0 * a_ptr[l].qs[(k >> 2) * 256 + (k % 4) * 4 * blocklen + m * blocklen + i]);
sumi2 = (v1 * a_ptr[l].qs[(k >> 2) * 256 + (k % 4) * 4 * blocklen + m * blocklen + i + 128]);
sumi1 = sumi1 * scales_0[j];
sumi2 = sumi2 * scales_1[j];
sumi += sumi1 + sumi2;
}
sumf[m][j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * a_ptr[l].d[m];
}
}
}
for (int sb = 0; sb < 8; sb++) {
uint8_t *mins = (uint8_t*) utmp + 8 + sb * 16;
for(int m = 0; m < 4; m++) {
const int16_t *bsums = a_ptr[l].bsums + (sb * 8) + (m * 4) - ((sb % 2) * 6);
for(int j = 0; j < ncols_interleaved; j++) {
sum_minf[m][j] += mins[j] * (bsums[0] + bsums[1]) * GGML_FP16_TO_FP32(b_ptr[l].dmin[j]) * a_ptr[l].d[m];
}
}
}
}
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) {
s[(y * 4 + m) * bs + x * ncols_interleaved + j] = sumf[m][j] - sum_minf[m][j];
}
}
}
}
#endif
}
static void ggml_gemm_iq4_nl_4x4_q8_0(int n, float * GGML_RESTRICT s, size_t bs, const void * GGML_RESTRICT vx, const void * GGML_RESTRICT vy, int nr, int nc) {
const int qk = QK8_0;
const int nb = n / qk;
const int ncols_interleaved = 4;
const int blocklen = 4;
assert (n % qk == 0);
assert (nr % 4 == 0);
assert (nc % ncols_interleaved == 0);
UNUSED(s);
UNUSED(bs);
UNUSED(vx);
UNUSED(vy);
UNUSED(nr);
UNUSED(nc);
UNUSED(nb);
UNUSED(ncols_interleaved);
UNUSED(blocklen);
#if ! ((defined(_MSC_VER)) && ! defined(__clang__)) && defined(__aarch64__) && defined(__ARM_NEON) && defined(__ARM_FEATURE_DOTPROD)
if (ggml_cpu_has_neon() && ggml_cpu_has_dotprod()) {
const int8x16_t kvalues = vld1q_s8(kvalues_iq4nl);
for (int y = 0; y < nr / 4; y++) {
const block_q8_0x4 * a_ptr = (const block_q8_0x4 *) vy + (y * nb);
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_iq4_nlx4 * b_ptr = (const block_iq4_nlx4 *) vx + (x * nb);
float32x4_t sumf[4];
for (int m = 0; m < 4; m++) {
sumf[m] = vdupq_n_f32(0);
}
for (int l = 0; l < nb; l++) {
float32x4_t a_d = vcvt_f32_f16(vld1_f16((const float16_t *)a_ptr[l].d));
float32x4_t b_d = vcvt_f32_f16(vld1_f16((const float16_t *)b_ptr[l].d));
int32x4_t sumi_0 = vdupq_n_s32(0);
int32x4_t sumi_1 = vdupq_n_s32(0);
int32x4_t sumi_2 = vdupq_n_s32(0);
int32x4_t sumi_3 = vdupq_n_s32(0);
for (int k = 0; k < 4; k++) {
int8x16_t a_0 = vld1q_s8(a_ptr[l].qs + 16 * k + 0);
int8x16_t a_1 = vld1q_s8(a_ptr[l].qs + 16 * k + 64);
uint8x16_t b = vld1q_u8(b_ptr[l].qs + 16 * k);
int8x16_t b_hi = vqtbl1q_s8(kvalues, b >> 4);
int8x16_t b_lo = vqtbl1q_s8(kvalues, b & 0xF);
sumi_0 = vdotq_laneq_s32(sumi_0, b_lo, a_0, 0);
sumi_1 = vdotq_laneq_s32(sumi_1, b_lo, a_0, 1);
sumi_2 = vdotq_laneq_s32(sumi_2, b_lo, a_0, 2);
sumi_3 = vdotq_laneq_s32(sumi_3, b_lo, a_0, 3);
sumi_0 = vdotq_laneq_s32(sumi_0, b_hi, a_1, 0);
sumi_1 = vdotq_laneq_s32(sumi_1, b_hi, a_1, 1);
sumi_2 = vdotq_laneq_s32(sumi_2, b_hi, a_1, 2);
sumi_3 = vdotq_laneq_s32(sumi_3, b_hi, a_1, 3);
}
sumf[0] = vmlaq_f32(sumf[0], vmulq_laneq_f32(b_d, a_d, 0), vcvtq_f32_s32(sumi_0));
sumf[1] = vmlaq_f32(sumf[1], vmulq_laneq_f32(b_d, a_d, 1), vcvtq_f32_s32(sumi_1));
sumf[2] = vmlaq_f32(sumf[2], vmulq_laneq_f32(b_d, a_d, 2), vcvtq_f32_s32(sumi_2));
sumf[3] = vmlaq_f32(sumf[3], vmulq_laneq_f32(b_d, a_d, 3), vcvtq_f32_s32(sumi_3));
}
for (int m = 0; m < 4; m++) {
vst1q_f32(s + (y * 4 + m) * bs + x * 4, sumf[m]);
}
}
}
return;
}
#endif
{
float sumf[4][4];
int sumi;
for (int y = 0; y < nr / 4; y++) {
const block_q8_0x4 * a_ptr = (const block_q8_0x4 *) vy + (y * nb);
for (int x = 0; x < nc / ncols_interleaved; x++) {
const block_iq4_nlx4 * b_ptr = (const block_iq4_nlx4 *) vx + (x * nb);
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) sumf[m][j] = 0.0;
}
for (int l = 0; l < nb; l++) {
for (int k = 0; k < (qk / (2 * blocklen)); k++) {
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++) {
sumi = 0;
for (int i = 0; i < blocklen; ++i) {
const int v0 = kvalues_iq4nl[b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] & 0x0F];
const int v1 = kvalues_iq4nl[b_ptr[l].qs[k * ncols_interleaved * blocklen + j * blocklen + i] >> 4];
sumi += ((v0 * a_ptr[l].qs[k * 4 * blocklen + m * blocklen + i]) +
(v1 * a_ptr[l].qs[k * 4 * blocklen + m * blocklen + i + qk / 2 * 4]));
}
sumf[m][j] += sumi * GGML_FP16_TO_FP32(b_ptr[l].d[j]) * GGML_FP16_TO_FP32(a_ptr[l].d[m]);
}
}
}
}
for (int m = 0; m < 4; m++) {
for (int j = 0; j < ncols_interleaved; j++)
s[(y * 4 + m) * bs + x * ncols_interleaved + j] = sumf[m][j];
}
}
}
}
}
static block_q4_0x4 make_block_q4_0x4(block_q4_0 * in, unsigned int blck_size_interleave) {
block_q4_0x4 out;
for (int i = 0; i < 4; i++) {
out.d[i] = in[i].d;
}
const int end = QK4_0 * 2 / blck_size_interleave;
if (blck_size_interleave == 8) {
const uint64_t xor_mask = 0x8888888888888888ULL;
for (int i = 0; i < end; ++i) {
int src_id = i % 4;
int src_offset = (i / 4) * blck_size_interleave;
int dst_offset = i * blck_size_interleave;
uint64_t elems;
memcpy(&elems, &in[src_id].qs[src_offset], sizeof(uint64_t));
elems ^= xor_mask;
memcpy(&out.qs[dst_offset], &elems, sizeof(uint64_t));
}
} else if (blck_size_interleave == 4) {
const uint32_t xor_mask = 0x88888888;
for (int i = 0; i < end; ++i) {
int src_id = i % 4;
int src_offset = (i / 4) * blck_size_interleave;
int dst_offset = i * blck_size_interleave;
uint32_t elems;
memcpy(&elems, &in[src_id].qs[src_offset], sizeof(uint32_t));
elems ^= xor_mask;
memcpy(&out.qs[dst_offset], &elems, sizeof(uint32_t));
}
} else {
GGML_ASSERT(false);
}
return out;
}
static block_q4_0x8 make_block_q4_0x8(block_q4_0 * in, unsigned int blck_size_interleave) {
block_q4_0x8 out;
for (int i = 0; i < 8; i++) {
out.d[i] = in[i].d;
}
const int end = QK4_0 * 4 / blck_size_interleave;
const uint64_t xor_mask = 0x8888888888888888ULL;
for (int i = 0; i < end; ++i) {
int src_id = i % 8;
int src_offset = (i / 8) * blck_size_interleave;
int dst_offset = i * blck_size_interleave;
uint64_t elems;
memcpy(&elems, &in[src_id].qs[src_offset], sizeof(uint64_t));
elems ^= xor_mask;
memcpy(&out.qs[dst_offset], &elems, sizeof(uint64_t));
}
return out;
}
static block_q4_Kx8 make_block_q4_Kx8(block_q4_K * in, unsigned int blck_size_interleave) {
block_q4_Kx8 out;
for (int i = 0; i < 8; i++) {
out.d[i] = in[i].GGML_COMMON_AGGR_U.GGML_COMMON_AGGR_S.d;
}
for (int i = 0; i < 8; i++) {
out.dmin[i] = in[i].GGML_COMMON_AGGR_U.GGML_COMMON_AGGR_S.dmin;
}
const int end = QK_K * 4 / blck_size_interleave;
for (int i = 0; i < end; ++i) {
int src_id = i % 8;
int src_offset = (i / 8) * blck_size_interleave;
int dst_offset = i * blck_size_interleave;
uint64_t elems;
memcpy(&elems, &in[src_id].qs[src_offset], sizeof(uint64_t));
memcpy(&out.qs[dst_offset], &elems, sizeof(uint64_t));
}
uint8_t s[8], m[8];
for (int i = 0; i < 4; i++) {
for (int j = 0; j < 8; j++) {
s[j] = in[j].scales[i] & 63;
m[j] = in[j].scales[i + 4] & 63;
}
out.scales[i * 12]      = (s[0] & 63) + ((s[4] & 48) << 2);
out.scales[i * 12 + 1]  = (s[1] & 63) + ((s[5] & 48) << 2);
out.scales[i * 12 + 2]  = (s[2] & 63) + ((s[6] & 48) << 2);
out.scales[i * 12 + 3]  = (s[3] & 63) + ((s[7] & 48) << 2);
out.scales[i * 12 + 4]  = (m[0] & 63) + ((m[4] & 48) << 2);
out.scales[i * 12 + 5]  = (m[1] & 63) + ((m[5] & 48) << 2);
out.scales[i * 12 + 6]  = (m[2] & 63) + ((m[6] & 48) << 2);
out.scales[i * 12 + 7]  = (m[3] & 63) + ((m[7] & 48) << 2);
out.scales[i * 12 + 8]  = (s[4] & 15) + ((m[4] & 15) << 4);
out.scales[i * 12 + 9]  = (s[5] & 15) + ((m[5] & 15) << 4);
out.scales[i * 12 + 10] = (s[6] & 15) + ((m[6] & 15) << 4);
out.scales[i * 12 + 11] = (s[7] & 15) + ((m[7] & 15) << 4);
}
for (int i = 0; i < 4; i++) {
for (int j = 0; j < 8; j++) {
s[j] = ((in[j].scales[i] & 192) >> 2) | (in[j].scales[i+8] & 15);
m[j] = ((in[j].scales[i + 4] & 192) >> 2) | ((in[j].scales[i+8] & 240) >> 4);
}
out.scales[i * 12 + 48] = (s[0] & 63) + ((s[4] & 48) << 2);
out.scales[i * 12 + 49] = (s[1] & 63) + ((s[5] & 48) << 2);
out.scales[i * 12 + 50] = (s[2] & 63) + ((s[6] & 48) << 2);
out.scales[i * 12 + 51] = (s[3] & 63) + ((s[7] & 48) << 2);
out.scales[i * 12 + 52] = (m[0] & 63) + ((m[4] & 48) << 2);
out.scales[i * 12 + 53] = (m[1] & 63) + ((m[5] & 48) << 2);
out.scales[i * 12 + 54] = (m[2] & 63) + ((m[6] & 48) << 2);
out.scales[i * 12 + 55] = (m[3] & 63) + ((m[7] & 48) << 2);
out.scales[i * 12 + 56] = (s[4] & 15) + ((m[4] & 15) << 4);
out.scales[i * 12 + 57] = (s[5] & 15) + ((m[5] & 15) << 4);
out.scales[i * 12 + 58] = (s[6] & 15) + ((m[6] & 15) << 4);
out.scales[i * 12 + 59] = (s[7] & 15) + ((m[7] & 15) << 4);
}
return out;
}
static int repack_q4_0_to_q4_0_4_bl(struct ggml_tensor * t, int interleave_block, const void * GGML_RESTRICT data, size_t data_size) {
GGML_ASSERT(t->type == GGML_TYPE_Q4_0);
GGML_ASSERT(interleave_block == 4 || interleave_block == 8);
constexpr int nrows_interleaved = 4;
block_q4_0x4 * dst = (block_q4_0x4 *)t->data;
const block_q4_0 * src = (const block_q4_0 *)data;
block_q4_0 dst_tmp[4];
int nrow = ggml_nrows(t);
int nblocks = t->ne[0] / QK4_0;
GGML_ASSERT(data_size == nrow * nblocks * sizeof(block_q4_0));
if (t->ne[1] % nrows_interleaved != 0 || t->ne[0] % 8 != 0) {
return -1;
}
for (int b = 0; b < nrow; b += nrows_interleaved) {
for (int64_t x = 0; x < nblocks; x++) {
for (int i = 0; i < nrows_interleaved; i++) {
dst_tmp[i] = src[x + i * nblocks];
}
*dst++ = make_block_q4_0x4(dst_tmp, interleave_block);
}
src += nrows_interleaved * nblocks;
}
return 0;
GGML_UNUSED(data_size);
}
static int repack_q4_K_to_q4_K_8_bl(struct ggml_tensor * t, int interleave_block, const void * GGML_RESTRICT data, size_t data_size) {
GGML_ASSERT(t->type == GGML_TYPE_Q4_K);
GGML_ASSERT(interleave_block == 8);
constexpr int nrows_interleaved = 8;
block_q4_Kx8 * dst = (block_q4_Kx8*)t->data;
const block_q4_K * src = (const block_q4_K*) data;
block_q4_K dst_tmp[8];
int nrow = ggml_nrows(t);
int nblocks = t->ne[0] / QK_K;
GGML_ASSERT(data_size == nrow * nblocks * sizeof(block_q4_K));
if (t->ne[1] % nrows_interleaved != 0 || t->ne[0] % 8 != 0) {
return -1;
}
for (int b = 0; b < nrow; b += nrows_interleaved) {
for (int64_t x = 0; x < nblocks; x++) {
for (int i  = 0; i < nrows_interleaved; i++ ) {
dst_tmp[i] = src[x + i * nblocks];
}
*dst++ = make_block_q4_Kx8(dst_tmp, interleave_block);
}
src += nrows_interleaved * nblocks;
}
return 0;
GGML_UNUSED(data_size);
}
static int repack_q4_0_to_q4_0_8_bl(struct ggml_tensor * t, int interleave_block, const void * GGML_RESTRICT data, size_t data_size) {
GGML_ASSERT(t->type == GGML_TYPE_Q4_0);
GGML_ASSERT(interleave_block == 8);
constexpr int nrows_interleaved = 8;
block_q4_0x8 * dst = (block_q4_0x8*)t->data;
const block_q4_0 * src = (const block_q4_0*) data;
block_q4_0 dst_tmp[8];
int nrow = ggml_nrows(t);
int nblocks = t->ne[0] / QK4_0;
GGML_ASSERT(data_size == nrow * nblocks * sizeof(block_q4_0));
if (t->ne[1] % nrows_interleaved != 0 || t->ne[0] % 8 != 0) {
return -1;
}
for (int b = 0; b < nrow; b += nrows_interleaved) {
for (int64_t x = 0; x < nblocks; x++) {
for (int i  = 0; i < nrows_interleaved; i++ ) {
dst_tmp[i] = src[x + i * nblocks];
}
*dst++ = make_block_q4_0x8(dst_tmp, interleave_block);
}
src += nrows_interleaved * nblocks;
}
return 0;
GGML_UNUSED(data_size);
}
static block_iq4_nlx4 make_block_iq4_nlx4(block_iq4_nl * in, unsigned int blck_size_interleave) {
block_iq4_nlx4 out;
for (int i = 0; i < 4; i++) {
out.d[i] = in[i].d;
}
const int end = QK4_NL * 2 / blck_size_interleave;
if (blck_size_interleave == 4) {
for (int i = 0; i < end; ++i) {
int src_id = i % 4;
int src_offset = (i / 4) * blck_size_interleave;
int dst_offset = i * blck_size_interleave;
memcpy(&out.qs[dst_offset], &in[src_id].qs[src_offset], sizeof(uint32_t));
}
} else {
GGML_ASSERT(false);
}
return out;
}
static int repack_iq4_nl_to_iq4_nl_4_bl(struct ggml_tensor * t, int interleave_block, const void * GGML_RESTRICT data, size_t data_size) {
GGML_ASSERT(t->type == GGML_TYPE_IQ4_NL);
GGML_ASSERT(interleave_block == 4);
block_iq4_nlx4 * dst = (block_iq4_nlx4 *)t->data;
const block_iq4_nl * src = (const block_iq4_nl *)data;
block_iq4_nl dst_tmp[4];
int nrow = ggml_nrows(t);
int nrows_interleaved = 4;
int nblocks = t->ne[0] / QK4_0;
GGML_ASSERT(data_size == nrow * nblocks * sizeof(block_iq4_nl));
if (t->ne[1] % nrows_interleaved != 0 || t->ne[0] % 8 != 0) {
return -1;
}
for (int b = 0; b < nrow; b += nrows_interleaved) {
for (int64_t x = 0; x < nblocks; x++) {
for (int i = 0; i < nrows_interleaved; i++) {
dst_tmp[i] = src[x + i * nblocks];
}
*dst++ = make_block_iq4_nlx4(dst_tmp, interleave_block);
}
src += nrows_interleaved * nblocks;
}
return 0;
GGML_UNUSED(data_size);
}
namespace ggml::cpu::aarch64 {
template <typename BLOC_TYPE, int64_t INTER_SIZE, int64_t NB_COLS>
int repack(struct ggml_tensor *, const void *, size_t);
template <> int repack<block_q4_0, 4, 4>(struct ggml_tensor * t, const void * data, size_t data_size) {
return repack_q4_0_to_q4_0_4_bl(t, 4, data, data_size);
}
template <> int repack<block_q4_0, 8, 4>(struct ggml_tensor * t, const void * data, size_t data_size) {
return repack_q4_0_to_q4_0_4_bl(t, 8, data, data_size);
}
template <> int repack<block_q4_0, 8, 8>(struct ggml_tensor * t, const void * data, size_t data_size) {
return repack_q4_0_to_q4_0_8_bl(t, 8, data, data_size);
}
template <> int repack<block_q4_K, 8, 8>(struct ggml_tensor * t, const void * data, size_t data_size) {
return repack_q4_K_to_q4_K_8_bl(t, 8, data, data_size);
}
template <> int repack<block_iq4_nl, 4, 4>(struct ggml_tensor * t, const void * data, size_t data_size) {
return repack_iq4_nl_to_iq4_nl_4_bl(t, 4, data, data_size);
}
template <typename BLOC_TYPE, int64_t INTER_SIZE, int64_t NB_COLS, ggml_type PARAM_TYPE>
void gemv(int, float *, size_t, const void *, const void *, int, int);
template <> void gemv<block_q4_0, 4, 4, GGML_TYPE_Q8_0>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemv_q4_0_4x4_q8_0(n, s, bs, vx, vy, nr, nc);
}
template <> void gemv<block_q4_0, 8, 4, GGML_TYPE_Q8_0>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemv_q4_0_4x8_q8_0(n, s, bs, vx, vy, nr, nc);
}
template <> void gemv<block_q4_0, 8, 8, GGML_TYPE_Q8_0>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemv_q4_0_8x8_q8_0(n, s, bs, vx, vy, nr, nc);
}
template <> void gemv<block_q4_K, 8, 8, GGML_TYPE_Q8_K>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemv_q4_K_8x8_q8_K(n, s, bs, vx, vy, nr, nc);
}
template <> void gemv<block_iq4_nl, 4, 4, GGML_TYPE_Q8_0>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemv_iq4_nl_4x4_q8_0(n, s, bs, vx, vy, nr, nc);
}
template <typename BLOC_TYPE, int64_t INTER_SIZE, int64_t NB_COLS, ggml_type PARAM_TYPE>
void gemm(int, float *, size_t, const void *, const void *, int, int);
template <> void gemm<block_q4_0, 4, 4, GGML_TYPE_Q8_0>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemm_q4_0_4x4_q8_0(n, s, bs, vx, vy, nr, nc);
}
template <> void gemm<block_q4_0, 8, 4, GGML_TYPE_Q8_0>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemm_q4_0_4x8_q8_0(n, s, bs, vx, vy, nr, nc);
}
template <> void gemm<block_q4_0, 8, 8, GGML_TYPE_Q8_0>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemm_q4_0_8x8_q8_0(n, s, bs, vx, vy, nr, nc);
}
template <> void gemm<block_q4_K, 8, 8, GGML_TYPE_Q8_K>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemm_q4_K_8x8_q8_K(n, s, bs, vx, vy, nr, nc);
}
template <> void gemm<block_iq4_nl, 4, 4, GGML_TYPE_Q8_0>(int n, float * s, size_t bs, const void * vx, const void * vy, int nr, int nc) {
ggml_gemm_iq4_nl_4x4_q8_0(n, s, bs, vx, vy, nr, nc);
}
class tensor_traits_base : public ggml::cpu::tensor_traits {
public:
virtual int repack(struct ggml_tensor * t, const void * data, size_t data_size) = 0;
};
template <typename BLOC_TYPE, int64_t INTER_SIZE, int64_t NB_COLS, ggml_type PARAM_TYPE> class tensor_traits : public tensor_traits_base {
bool work_size(int , const struct ggml_tensor * op, size_t & size) override {
switch (op->op) {
case GGML_OP_MUL_MAT:
size = ggml_row_size(PARAM_TYPE, ggml_nelements(op->src[1]));
return true;
case GGML_OP_MUL_MAT_ID:
size = ggml_row_size(PARAM_TYPE, ggml_nelements(op->src[1]));
size = GGML_PAD(size, sizeof(int64_t));
size += sizeof(int64_t) * (1+op->src[0]->ne[2]) * op->src[1]->ne[2];
return true;
default:
break;
}
return false;
}
bool compute_forward(struct ggml_compute_params * params, struct ggml_tensor * op) override {
switch (op->op) {
case GGML_OP_MUL_MAT:
forward_mul_mat(params, op);
return true;
case GGML_OP_MUL_MAT_ID:
forward_mul_mat_id(params, op);
return true;
default:
break;
}
return false;
}
void forward_mul_mat(ggml_compute_params * params, ggml_tensor * op) {
const ggml_tensor * src0 = op->src[0];
const ggml_tensor * src1 = op->src[1];
ggml_tensor *       dst  = op;
GGML_TENSOR_BINARY_OP_LOCALS
const int ith = params->ith;
const int nth = params->nth;
GGML_ASSERT(ne0 == ne01);
GGML_ASSERT(ne1 == ne11);
GGML_ASSERT(ne2 == ne12);
GGML_ASSERT(ne3 == ne13);
GGML_ASSERT(nb0 == sizeof(float));
GGML_ASSERT(nb0 <= nb1);
GGML_ASSERT(nb1 <= nb2);
GGML_ASSERT(nb2 <= nb3);
GGML_ASSERT(src1->type == GGML_TYPE_F32);
GGML_ASSERT(ggml_n_dims(op->src[0]) == 2);
char *       wdata = static_cast<char *>(params->wdata);
const size_t nbw1  = ggml_row_size(PARAM_TYPE, ne10);
assert(params->wsize >= nbw1 * ne11);
const ggml_from_float_t from_float = ggml_get_type_traits_cpu(PARAM_TYPE)->from_float;
int64_t i11_processed = 0;
for (int64_t i11 = ith * 4; i11 < ne11 - ne11 % 4; i11 += nth * 4) {
ggml_quantize_mat_t<INTER_SIZE, PARAM_TYPE>((float *) ((char *) src1->data + i11 * nb11), (void *) (wdata + i11 * nbw1), 4, ne10);
}
i11_processed = ne11 - ne11 % 4;
for (int64_t i11 = i11_processed + ith; i11 < ne11; i11 += nth) {
from_float((float *) ((char *) src1->data + i11 * nb11), (void *) (wdata + i11 * nbw1), ne10);
}
ggml_barrier(params->threadpool);
const void * src1_wdata      = params->wdata;
const size_t src1_col_stride = ggml_row_size(PARAM_TYPE, ne10);
int64_t      src0_start      = (ith * ne01) / nth;
int64_t      src0_end        = ((ith + 1) * ne01) / nth;
src0_start = (src0_start % NB_COLS) ? src0_start + NB_COLS - (src0_start % NB_COLS) : src0_start;
src0_end   = (src0_end   % NB_COLS) ? src0_end   + NB_COLS - (src0_end   % NB_COLS) : src0_end;
if (src0_start >= src0_end) {
return;
}
if (ne11 > 3) {
gemm<BLOC_TYPE, INTER_SIZE, NB_COLS, PARAM_TYPE>(ne00,
(float *) ((char *) dst->data) + src0_start, ne01,
(const char *) src0->data + src0_start * nb01,
(const char *) src1_wdata, ne11 - ne11 % 4, src0_end - src0_start);
}
for (int iter = ne11 - ne11 % 4; iter < ne11; iter++) {
gemv<BLOC_TYPE, INTER_SIZE, NB_COLS, PARAM_TYPE>(ne00,
(float *) ((char *) dst->data + (iter * nb1)) + src0_start, ne01,
(const char *) src0->data + src0_start * nb01,
(const char *) src1_wdata + (src1_col_stride * iter), 1,
src0_end - src0_start);
}
}
void forward_mul_mat_id(ggml_compute_params * params, ggml_tensor * op) {
const ggml_tensor * src0 = op->src[0];
const ggml_tensor * src1 = op->src[1];
const ggml_tensor * ids  = op->src[2];
ggml_tensor *       dst  = op;
GGML_TENSOR_BINARY_OP_LOCALS
const int ith = params->ith;
const int nth = params->nth;
const ggml_from_float_t from_float = ggml_get_type_traits_cpu(PARAM_TYPE)->from_float;
GGML_ASSERT(nb00 == ggml_type_size(src0->type));
GGML_ASSERT(nb10 == ggml_type_size(src1->type));
GGML_ASSERT(nb0 == sizeof(float));
GGML_ASSERT(nb0 <= nb1);
GGML_ASSERT(nb1 <= nb2);
GGML_ASSERT(nb2 <= nb3);
GGML_ASSERT(ne03 == 1);
GGML_ASSERT(ne13 == 1);
GGML_ASSERT(ne3  == 1);
GGML_ASSERT(src1->type == GGML_TYPE_F32);
const int n_ids = ids->ne[0];
const int n_as  = ne02;
const size_t nbw1 = ggml_row_size(PARAM_TYPE, ne10);
const size_t nbw2 = nbw1*ne11;
const size_t nbw3 = nbw2*ne12;
struct mmid_row_mapping {
int32_t i1;
int32_t i2;
};
GGML_ASSERT(params->wsize >= (GGML_PAD(nbw3, sizeof(int64_t)) + n_as * sizeof(int64_t) +
n_as * ne12 * sizeof(mmid_row_mapping)));
auto * wdata             = (char *)     params->wdata;
auto * wdata_src1_end    = (char *)     wdata + GGML_PAD(nbw3, sizeof(int64_t));
auto * matrix_row_counts = (int64_t *) (wdata_src1_end);
struct mmid_row_mapping * matrix_rows = (struct mmid_row_mapping *) (matrix_row_counts + n_as);
for (int64_t i12 = 0; i12 < ne12; ++i12) {
for (int64_t i11 = ith; i11 < ne11; i11 += nth) {
from_float((float *)((char *) src1->data + i12 * nb12 + i11 * nb11),
(void *)               (wdata + i12 * nbw2 + i11 * nbw1),
ne10);
}
}
#define MMID_MATRIX_ROW(row_id, i1) matrix_rows[(row_id) * ne12 + (i1)]
if (ith == 0) {
memset(matrix_row_counts, 0, n_as * sizeof(int64_t));
for (int32_t iid1 = 0; iid1 < ids->ne[1]; ++iid1) {
for (int32_t id = 0; id < n_ids; ++id) {
const int32_t i02 =
*(const int32_t *) ((const char *) ids->data + iid1 * ids->nb[1] + id * ids->nb[0]);
GGML_ASSERT(i02 >= 0 && i02 < n_as);
MMID_MATRIX_ROW(i02, matrix_row_counts[i02]) = { id, iid1 };
matrix_row_counts[i02] += 1;
}
}
}
ggml_barrier(params->threadpool);
for (int cur_a = 0; cur_a < n_as; ++cur_a) {
const int64_t cne1 = matrix_row_counts[cur_a];
if (cne1 == 0) {
continue;
}
const auto * src0_cur = (const char *) src0->data + cur_a*nb02;
const int64_t nr1 = cne1;
int64_t src0_cur_start = (ith * ne01) / nth;
int64_t src0_cur_end   = ((ith + 1) * ne01) / nth;
src0_cur_start = (src0_cur_start % NB_COLS) ? src0_cur_start + NB_COLS - (src0_cur_start % NB_COLS) : src0_cur_start;
src0_cur_end   = (src0_cur_end   % NB_COLS) ? src0_cur_end   + NB_COLS - (src0_cur_end   % NB_COLS) : src0_cur_end;
if (src0_cur_start >= src0_cur_end) {
return;
}
for (int ir1 = 0; ir1 < nr1; ir1++) {
struct mmid_row_mapping row_mapping = MMID_MATRIX_ROW(cur_a, ir1);
const int id = row_mapping.i1;
const int64_t i11 = id % ne11;
const int64_t i12 = row_mapping.i2;
const int64_t i1 = id;
const int64_t i2 = i12;
const auto * src1_col = (const char *) wdata + (i11 * nbw1 + i12 * nbw2);
gemv<BLOC_TYPE, INTER_SIZE, NB_COLS, PARAM_TYPE>(ne00,
(float *)((char *) dst->data + (i1 * nb1 + i2 * nb2)) + src0_cur_start, ne01,
src0_cur + src0_cur_start * nb01,
src1_col, 1, src0_cur_end - src0_cur_start);
}
}
#undef MMID_MATRIX_ROW
}
int repack(struct ggml_tensor * t, const void * data, size_t data_size) override {
GGML_LOG_DEBUG("%s: repack tensor %s with %s_%dx%d\n", __func__, t->name, ggml_type_name(t->type),
(int) NB_COLS, (int) INTER_SIZE);
return ggml::cpu::aarch64::repack<BLOC_TYPE, INTER_SIZE, NB_COLS>(t, data, data_size);
}
};
static const tensor_traits<block_q4_0, 4, 4, GGML_TYPE_Q8_0> q4_0_4x4_q8_0;
static const tensor_traits<block_q4_0, 8, 4, GGML_TYPE_Q8_0> q4_0_4x8_q8_0;
static const tensor_traits<block_q4_0, 8, 8, GGML_TYPE_Q8_0> q4_0_8x8_q8_0;
static const tensor_traits<block_q4_K, 8, 8, GGML_TYPE_Q8_K> q4_K_8x8_q8_K;
static const tensor_traits<block_iq4_nl, 4, 4, GGML_TYPE_Q8_0> iq4_nl_4x4_q8_0;
}
static const ggml::cpu::tensor_traits * ggml_aarch64_get_optimal_repack_type(const struct ggml_tensor * cur) {
if (cur->type == GGML_TYPE_Q4_0) {
if (ggml_cpu_has_avx2() || (ggml_cpu_has_sve() && ggml_cpu_has_matmul_int8() && ggml_cpu_get_sve_cnt() == QK8_0)) {
if (cur->ne[1] % 8 == 0) {
return &ggml::cpu::aarch64::q4_0_8x8_q8_0;
}
}
if (ggml_cpu_has_neon() && ggml_cpu_has_matmul_int8()) {
if (cur->ne[1] % 4 == 0) {
return &ggml::cpu::aarch64::q4_0_4x8_q8_0;
}
}
if (ggml_cpu_has_neon() && ggml_cpu_has_dotprod()) {
if (cur->ne[1] % 4 == 0) {
return &ggml::cpu::aarch64::q4_0_4x4_q8_0;
}
}
} else if (cur->type == GGML_TYPE_Q4_K) {
if (ggml_cpu_has_avx2()) {
if (cur->ne[1] % 8 == 0) {
return &ggml::cpu::aarch64::q4_K_8x8_q8_K;
}
}
} else if (cur->type == GGML_TYPE_IQ4_NL) {
if (ggml_cpu_has_neon() && ggml_cpu_has_dotprod()) {
if (cur->ne[1] % 4 == 0) {
return &ggml::cpu::aarch64::iq4_nl_4x4_q8_0;
}
}
}
return nullptr;
}
static enum ggml_status ggml_backend_cpu_aarch64_buffer_init_tensor(ggml_backend_buffer_t buffer, struct ggml_tensor * tensor) {
tensor->extra = (void *) const_cast<ggml::cpu::tensor_traits *>(ggml_aarch64_get_optimal_repack_type(tensor));
GGML_UNUSED(buffer);
return GGML_STATUS_SUCCESS;
}
static void ggml_backend_cpu_aarch64_buffer_set_tensor(ggml_backend_buffer_t buffer, struct ggml_tensor * tensor,
const void * data, size_t offset, size_t size) {
GGML_ASSERT(offset == 0);
GGML_ASSERT(size == ggml_nbytes(tensor));
auto tensor_traits = (ggml::cpu::aarch64::tensor_traits_base *) tensor->extra;
auto OK            = tensor_traits->repack(tensor, data, size);
GGML_ASSERT(OK == 0);
GGML_UNUSED(buffer);
}
static const char * ggml_backend_cpu_aarch64_buffer_type_get_name(ggml_backend_buffer_type_t buft) {
return "CPU_AARCH64";
GGML_UNUSED(buft);
}
static ggml_backend_buffer_t ggml_backend_cpu_aarch64_buffer_type_alloc_buffer(ggml_backend_buffer_type_t buft, size_t size) {
ggml_backend_buffer_t buffer = ggml_backend_buft_alloc_buffer(ggml_backend_cpu_buffer_type(), size);
if (buffer == nullptr) {
return nullptr;
}
buffer->buft              = buft;
buffer->iface.init_tensor = ggml_backend_cpu_aarch64_buffer_init_tensor;
buffer->iface.set_tensor  = ggml_backend_cpu_aarch64_buffer_set_tensor;
buffer->iface.get_tensor  = nullptr;
buffer->iface.cpy_tensor  = nullptr;
return buffer;
}
static size_t ggml_backend_cpu_aarch64_buffer_type_get_alignment(ggml_backend_buffer_type_t buft) {
return TENSOR_ALIGNMENT;
GGML_UNUSED(buft);
}
namespace ggml::cpu::aarch64 {
class extra_buffer_type : ggml::cpu::extra_buffer_type {
bool supports_op(ggml_backend_dev_t, const struct ggml_tensor * op) override {
if (    op->op == GGML_OP_MUL_MAT &&
op->src[0]->buffer &&
(ggml_n_dims(op->src[0]) == 2) &&
op->src[0]->buffer->buft == ggml_backend_cpu_aarch64_buffer_type() &&
ggml_aarch64_get_optimal_repack_type(op->src[0])
) {
if (op->src[1]->buffer && !ggml_backend_buft_is_host(op->src[1]->buffer->buft)) {
return false;
}
if (op->src[1]->type == GGML_TYPE_F32) {
return true;
}
} else if (op->op == GGML_OP_MUL_MAT_ID
&& op->src[0]->buffer
&& (ggml_n_dims(op->src[0]) == 3)
&& op->src[0]->buffer->buft == ggml_backend_cpu_aarch64_buffer_type()
&& ggml_aarch64_get_optimal_repack_type(op->src[0])
) {
if (op->src[1]->buffer && !ggml_backend_buft_is_host(op->src[1]->buffer->buft)) {
return false;
}
if (op->src[1]->type == GGML_TYPE_F32) {
return true;
}
}
return false;
}
ggml::cpu::tensor_traits * get_tensor_traits(const struct ggml_tensor * op) override {
if (op->op == GGML_OP_MUL_MAT || op->op == GGML_OP_MUL_MAT_ID) {
if (op->src[0]->buffer && op->src[0]->buffer->buft == ggml_backend_cpu_aarch64_buffer_type()) {
return (ggml::cpu::tensor_traits *) op->src[0]->extra;
}
}
return nullptr;
}
};
}
ggml_backend_buffer_type_t ggml_backend_cpu_aarch64_buffer_type(void) {
static struct ggml_backend_buffer_type ggml_backend_cpu_buffer_type_aarch64 = {
{
ggml_backend_cpu_aarch64_buffer_type_get_name,
ggml_backend_cpu_aarch64_buffer_type_alloc_buffer,
ggml_backend_cpu_aarch64_buffer_type_get_alignment,
nullptr,
nullptr,
nullptr,
},
ggml_backend_reg_dev_get(ggml_backend_cpu_reg(), 0),
new ggml::cpu::aarch64::extra_buffer_type(),
};
return &ggml_backend_cpu_buffer_type_aarch64;
}