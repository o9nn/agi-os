#include <stdlib.h>
#include <openssl/obj_mac.h>
#include <openssl/ec.h>
#include <openssl/bn.h>
#if defined(__SUNPRO_C)
# if __SUNPRO_C >= 0x520
# pragma error_messages (off,E_ARRAY_OF_INCOMPLETE_NONAME,E_ARRAY_OF_INCOMPLETE)
# endif
#endif
#define EC_FLAGS_DEFAULT_OCT 0x1
struct ec_method_st {
int flags;
int field_type;
int (*group_init) (EC_GROUP *);
void (*group_finish) (EC_GROUP *);
void (*group_clear_finish) (EC_GROUP *);
int (*group_copy) (EC_GROUP *, const EC_GROUP *);
int (*group_set_curve) (EC_GROUP *, const BIGNUM *p, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int (*group_get_curve) (const EC_GROUP *, BIGNUM *p, BIGNUM *a, BIGNUM *b,
BN_CTX *);
int (*group_get_degree) (const EC_GROUP *);
int (*group_check_discriminant) (const EC_GROUP *, BN_CTX *);
int (*point_init) (EC_POINT *);
void (*point_finish) (EC_POINT *);
void (*point_clear_finish) (EC_POINT *);
int (*point_copy) (EC_POINT *, const EC_POINT *);
int (*point_set_to_infinity) (const EC_GROUP *, EC_POINT *);
int (*point_set_Jprojective_coordinates_GFp) (const EC_GROUP *,
EC_POINT *, const BIGNUM *x,
const BIGNUM *y,
const BIGNUM *z, BN_CTX *);
int (*point_get_Jprojective_coordinates_GFp) (const EC_GROUP *,
const EC_POINT *, BIGNUM *x,
BIGNUM *y, BIGNUM *z,
BN_CTX *);
int (*point_set_affine_coordinates) (const EC_GROUP *, EC_POINT *,
const BIGNUM *x, const BIGNUM *y,
BN_CTX *);
int (*point_get_affine_coordinates) (const EC_GROUP *, const EC_POINT *,
BIGNUM *x, BIGNUM *y, BN_CTX *);
int (*point_set_compressed_coordinates) (const EC_GROUP *, EC_POINT *,
const BIGNUM *x, int y_bit,
BN_CTX *);
size_t (*point2oct) (const EC_GROUP *, const EC_POINT *,
point_conversion_form_t form, unsigned char *buf,
size_t len, BN_CTX *);
int (*oct2point) (const EC_GROUP *, EC_POINT *, const unsigned char *buf,
size_t len, BN_CTX *);
int (*add) (const EC_GROUP *, EC_POINT *r, const EC_POINT *a,
const EC_POINT *b, BN_CTX *);
int (*dbl) (const EC_GROUP *, EC_POINT *r, const EC_POINT *a, BN_CTX *);
int (*invert) (const EC_GROUP *, EC_POINT *, BN_CTX *);
int (*is_at_infinity) (const EC_GROUP *, const EC_POINT *);
int (*is_on_curve) (const EC_GROUP *, const EC_POINT *, BN_CTX *);
int (*point_cmp) (const EC_GROUP *, const EC_POINT *a, const EC_POINT *b,
BN_CTX *);
int (*make_affine) (const EC_GROUP *, EC_POINT *, BN_CTX *);
int (*points_make_affine) (const EC_GROUP *, size_t num, EC_POINT *[],
BN_CTX *);
int (*mul) (const EC_GROUP *group, EC_POINT *r, const BIGNUM *scalar,
size_t num, const EC_POINT *points[], const BIGNUM *scalars[],
BN_CTX *);
int (*precompute_mult) (EC_GROUP *group, BN_CTX *);
int (*have_precompute_mult) (const EC_GROUP *group);
int (*field_mul) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int (*field_sqr) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a, BN_CTX *);
int (*field_div) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int (*field_encode) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
BN_CTX *);
int (*field_decode) (const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
BN_CTX *);
int (*field_set_to_one) (const EC_GROUP *, BIGNUM *r, BN_CTX *);
} ;
typedef struct ec_extra_data_st {
struct ec_extra_data_st *next;
void *data;
void *(*dup_func) (void *);
void (*free_func) (void *);
void (*clear_free_func) (void *);
} EC_EXTRA_DATA;
struct ec_group_st {
const EC_METHOD *meth;
EC_POINT *generator;
BIGNUM order, cofactor;
int curve_name;
int asn1_flag;
point_conversion_form_t asn1_form;
unsigned char *seed;
size_t seed_len;
EC_EXTRA_DATA *extra_data;
BIGNUM field;
int poly[6];
BIGNUM a, b;
int a_is_minus3;
void *field_data1;
void *field_data2;
int (*field_mod_func) (BIGNUM *, const BIGNUM *, const BIGNUM *,
BN_CTX *);
} ;
struct ec_key_st {
int version;
EC_GROUP *group;
EC_POINT *pub_key;
BIGNUM *priv_key;
unsigned int enc_flag;
point_conversion_form_t conv_form;
int references;
int flags;
EC_EXTRA_DATA *method_data;
} ;
int EC_EX_DATA_set_data(EC_EXTRA_DATA **, void *data,
void *(*dup_func) (void *),
void (*free_func) (void *),
void (*clear_free_func) (void *));
void *EC_EX_DATA_get_data(const EC_EXTRA_DATA *, void *(*dup_func) (void *),
void (*free_func) (void *),
void (*clear_free_func) (void *));
void EC_EX_DATA_free_data(EC_EXTRA_DATA **, void *(*dup_func) (void *),
void (*free_func) (void *),
void (*clear_free_func) (void *));
void EC_EX_DATA_clear_free_data(EC_EXTRA_DATA **, void *(*dup_func) (void *),
void (*free_func) (void *),
void (*clear_free_func) (void *));
void EC_EX_DATA_free_all_data(EC_EXTRA_DATA **);
void EC_EX_DATA_clear_free_all_data(EC_EXTRA_DATA **);
struct ec_point_st {
const EC_METHOD *meth;
BIGNUM X;
BIGNUM Y;
BIGNUM Z;
int Z_is_one;
} ;
int ec_wNAF_mul(const EC_GROUP *group, EC_POINT *r, const BIGNUM *scalar,
size_t num, const EC_POINT *points[], const BIGNUM *scalars[],
BN_CTX *);
int ec_wNAF_precompute_mult(EC_GROUP *group, BN_CTX *);
int ec_wNAF_have_precompute_mult(const EC_GROUP *group);
int ec_GFp_simple_group_init(EC_GROUP *);
void ec_GFp_simple_group_finish(EC_GROUP *);
void ec_GFp_simple_group_clear_finish(EC_GROUP *);
int ec_GFp_simple_group_copy(EC_GROUP *, const EC_GROUP *);
int ec_GFp_simple_group_set_curve(EC_GROUP *, const BIGNUM *p,
const BIGNUM *a, const BIGNUM *b, BN_CTX *);
int ec_GFp_simple_group_get_curve(const EC_GROUP *, BIGNUM *p, BIGNUM *a,
BIGNUM *b, BN_CTX *);
int ec_GFp_simple_group_get_degree(const EC_GROUP *);
int ec_GFp_simple_group_check_discriminant(const EC_GROUP *, BN_CTX *);
int ec_GFp_simple_point_init(EC_POINT *);
void ec_GFp_simple_point_finish(EC_POINT *);
void ec_GFp_simple_point_clear_finish(EC_POINT *);
int ec_GFp_simple_point_copy(EC_POINT *, const EC_POINT *);
int ec_GFp_simple_point_set_to_infinity(const EC_GROUP *, EC_POINT *);
int ec_GFp_simple_set_Jprojective_coordinates_GFp(const EC_GROUP *,
EC_POINT *, const BIGNUM *x,
const BIGNUM *y,
const BIGNUM *z, BN_CTX *);
int ec_GFp_simple_get_Jprojective_coordinates_GFp(const EC_GROUP *,
const EC_POINT *, BIGNUM *x,
BIGNUM *y, BIGNUM *z,
BN_CTX *);
int ec_GFp_simple_point_set_affine_coordinates(const EC_GROUP *, EC_POINT *,
const BIGNUM *x,
const BIGNUM *y, BN_CTX *);
int ec_GFp_simple_point_get_affine_coordinates(const EC_GROUP *,
const EC_POINT *, BIGNUM *x,
BIGNUM *y, BN_CTX *);
int ec_GFp_simple_set_compressed_coordinates(const EC_GROUP *, EC_POINT *,
const BIGNUM *x, int y_bit,
BN_CTX *);
size_t ec_GFp_simple_point2oct(const EC_GROUP *, const EC_POINT *,
point_conversion_form_t form,
unsigned char *buf, size_t len, BN_CTX *);
int ec_GFp_simple_oct2point(const EC_GROUP *, EC_POINT *,
const unsigned char *buf, size_t len, BN_CTX *);
int ec_GFp_simple_add(const EC_GROUP *, EC_POINT *r, const EC_POINT *a,
const EC_POINT *b, BN_CTX *);
int ec_GFp_simple_dbl(const EC_GROUP *, EC_POINT *r, const EC_POINT *a,
BN_CTX *);
int ec_GFp_simple_invert(const EC_GROUP *, EC_POINT *, BN_CTX *);
int ec_GFp_simple_is_at_infinity(const EC_GROUP *, const EC_POINT *);
int ec_GFp_simple_is_on_curve(const EC_GROUP *, const EC_POINT *, BN_CTX *);
int ec_GFp_simple_cmp(const EC_GROUP *, const EC_POINT *a, const EC_POINT *b,
BN_CTX *);
int ec_GFp_simple_make_affine(const EC_GROUP *, EC_POINT *, BN_CTX *);
int ec_GFp_simple_points_make_affine(const EC_GROUP *, size_t num,
EC_POINT *[], BN_CTX *);
int ec_GFp_simple_field_mul(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int ec_GFp_simple_field_sqr(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
BN_CTX *);
int ec_GFp_mont_group_init(EC_GROUP *);
int ec_GFp_mont_group_set_curve(EC_GROUP *, const BIGNUM *p, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
void ec_GFp_mont_group_finish(EC_GROUP *);
void ec_GFp_mont_group_clear_finish(EC_GROUP *);
int ec_GFp_mont_group_copy(EC_GROUP *, const EC_GROUP *);
int ec_GFp_mont_field_mul(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int ec_GFp_mont_field_sqr(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
BN_CTX *);
int ec_GFp_mont_field_encode(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
BN_CTX *);
int ec_GFp_mont_field_decode(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
BN_CTX *);
int ec_GFp_mont_field_set_to_one(const EC_GROUP *, BIGNUM *r, BN_CTX *);
int ec_GFp_nist_group_copy(EC_GROUP *dest, const EC_GROUP *src);
int ec_GFp_nist_group_set_curve(EC_GROUP *, const BIGNUM *p, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int ec_GFp_nist_field_mul(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int ec_GFp_nist_field_sqr(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
BN_CTX *);
int ec_GF2m_simple_group_init(EC_GROUP *);
void ec_GF2m_simple_group_finish(EC_GROUP *);
void ec_GF2m_simple_group_clear_finish(EC_GROUP *);
int ec_GF2m_simple_group_copy(EC_GROUP *, const EC_GROUP *);
int ec_GF2m_simple_group_set_curve(EC_GROUP *, const BIGNUM *p,
const BIGNUM *a, const BIGNUM *b,
BN_CTX *);
int ec_GF2m_simple_group_get_curve(const EC_GROUP *, BIGNUM *p, BIGNUM *a,
BIGNUM *b, BN_CTX *);
int ec_GF2m_simple_group_get_degree(const EC_GROUP *);
int ec_GF2m_simple_group_check_discriminant(const EC_GROUP *, BN_CTX *);
int ec_GF2m_simple_point_init(EC_POINT *);
void ec_GF2m_simple_point_finish(EC_POINT *);
void ec_GF2m_simple_point_clear_finish(EC_POINT *);
int ec_GF2m_simple_point_copy(EC_POINT *, const EC_POINT *);
int ec_GF2m_simple_point_set_to_infinity(const EC_GROUP *, EC_POINT *);
int ec_GF2m_simple_point_set_affine_coordinates(const EC_GROUP *, EC_POINT *,
const BIGNUM *x,
const BIGNUM *y, BN_CTX *);
int ec_GF2m_simple_point_get_affine_coordinates(const EC_GROUP *,
const EC_POINT *, BIGNUM *x,
BIGNUM *y, BN_CTX *);
int ec_GF2m_simple_set_compressed_coordinates(const EC_GROUP *, EC_POINT *,
const BIGNUM *x, int y_bit,
BN_CTX *);
size_t ec_GF2m_simple_point2oct(const EC_GROUP *, const EC_POINT *,
point_conversion_form_t form,
unsigned char *buf, size_t len, BN_CTX *);
int ec_GF2m_simple_oct2point(const EC_GROUP *, EC_POINT *,
const unsigned char *buf, size_t len, BN_CTX *);
int ec_GF2m_simple_add(const EC_GROUP *, EC_POINT *r, const EC_POINT *a,
const EC_POINT *b, BN_CTX *);
int ec_GF2m_simple_dbl(const EC_GROUP *, EC_POINT *r, const EC_POINT *a,
BN_CTX *);
int ec_GF2m_simple_invert(const EC_GROUP *, EC_POINT *, BN_CTX *);
int ec_GF2m_simple_is_at_infinity(const EC_GROUP *, const EC_POINT *);
int ec_GF2m_simple_is_on_curve(const EC_GROUP *, const EC_POINT *, BN_CTX *);
int ec_GF2m_simple_cmp(const EC_GROUP *, const EC_POINT *a, const EC_POINT *b,
BN_CTX *);
int ec_GF2m_simple_make_affine(const EC_GROUP *, EC_POINT *, BN_CTX *);
int ec_GF2m_simple_points_make_affine(const EC_GROUP *, size_t num,
EC_POINT *[], BN_CTX *);
int ec_GF2m_simple_field_mul(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int ec_GF2m_simple_field_sqr(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
BN_CTX *);
int ec_GF2m_simple_field_div(const EC_GROUP *, BIGNUM *r, const BIGNUM *a,
const BIGNUM *b, BN_CTX *);
int ec_GF2m_simple_mul(const EC_GROUP *group, EC_POINT *r,
const BIGNUM *scalar, size_t num,
const EC_POINT *points[], const BIGNUM *scalars[],
BN_CTX *);
int ec_GF2m_precompute_mult(EC_GROUP *group, BN_CTX *ctx);
int ec_GF2m_have_precompute_mult(const EC_GROUP *group);
#ifndef OPENSSL_NO_EC_NISTP_64_GCC_128
int ec_GFp_nistp224_group_init(EC_GROUP *group);
int ec_GFp_nistp224_group_set_curve(EC_GROUP *group, const BIGNUM *p,
const BIGNUM *a, const BIGNUM *n,
BN_CTX *);
int ec_GFp_nistp224_point_get_affine_coordinates(const EC_GROUP *group,
const EC_POINT *point,
BIGNUM *x, BIGNUM *y,
BN_CTX *ctx);
int ec_GFp_nistp224_mul(const EC_GROUP *group, EC_POINT *r,
const BIGNUM *scalar, size_t num,
const EC_POINT *points[], const BIGNUM *scalars[],
BN_CTX *);
int ec_GFp_nistp224_points_mul(const EC_GROUP *group, EC_POINT *r,
const BIGNUM *scalar, size_t num,
const EC_POINT *points[],
const BIGNUM *scalars[], BN_CTX *ctx);
int ec_GFp_nistp224_precompute_mult(EC_GROUP *group, BN_CTX *ctx);
int ec_GFp_nistp224_have_precompute_mult(const EC_GROUP *group);
int ec_GFp_nistp256_group_init(EC_GROUP *group);
int ec_GFp_nistp256_group_set_curve(EC_GROUP *group, const BIGNUM *p,
const BIGNUM *a, const BIGNUM *n,
BN_CTX *);
int ec_GFp_nistp256_point_get_affine_coordinates(const EC_GROUP *group,
const EC_POINT *point,
BIGNUM *x, BIGNUM *y,
BN_CTX *ctx);
int ec_GFp_nistp256_mul(const EC_GROUP *group, EC_POINT *r,
const BIGNUM *scalar, size_t num,
const EC_POINT *points[], const BIGNUM *scalars[],
BN_CTX *);
int ec_GFp_nistp256_points_mul(const EC_GROUP *group, EC_POINT *r,
const BIGNUM *scalar, size_t num,
const EC_POINT *points[],
const BIGNUM *scalars[], BN_CTX *ctx);
int ec_GFp_nistp256_precompute_mult(EC_GROUP *group, BN_CTX *ctx);
int ec_GFp_nistp256_have_precompute_mult(const EC_GROUP *group);
int ec_GFp_nistp521_group_init(EC_GROUP *group);
int ec_GFp_nistp521_group_set_curve(EC_GROUP *group, const BIGNUM *p,
const BIGNUM *a, const BIGNUM *n,
BN_CTX *);
int ec_GFp_nistp521_point_get_affine_coordinates(const EC_GROUP *group,
const EC_POINT *point,
BIGNUM *x, BIGNUM *y,
BN_CTX *ctx);
int ec_GFp_nistp521_mul(const EC_GROUP *group, EC_POINT *r,
const BIGNUM *scalar, size_t num,
const EC_POINT *points[], const BIGNUM *scalars[],
BN_CTX *);
int ec_GFp_nistp521_points_mul(const EC_GROUP *group, EC_POINT *r,
const BIGNUM *scalar, size_t num,
const EC_POINT *points[],
const BIGNUM *scalars[], BN_CTX *ctx);
int ec_GFp_nistp521_precompute_mult(EC_GROUP *group, BN_CTX *ctx);
int ec_GFp_nistp521_have_precompute_mult(const EC_GROUP *group);
void ec_GFp_nistp_points_make_affine_internal(size_t num, void *point_array,
size_t felem_size,
void *tmp_felems,
void (*felem_one) (void *out),
int (*felem_is_zero) (const void
*in),
void (*felem_assign) (void *out,
const void
*in),
void (*felem_square) (void *out,
const void
*in),
void (*felem_mul) (void *out,
const void
*in1,
const void
*in2),
void (*felem_inv) (void *out,
const void
*in),
void (*felem_contract) (void
*out,
const
void
*in));
void ec_GFp_nistp_recode_scalar_bits(unsigned char *sign,
unsigned char *digit, unsigned char in);
#endif