#define BLOCK_CIPHER_ecb_loop() \
size_t i, bl; \
bl = ctx->cipher->block_size;\
if(inl < bl) return 1;\
inl -= bl; \
for(i=0; i <= inl; i+=bl)
#define BLOCK_CIPHER_func_ecb(cname, cprefix, kstruct, ksched) \
static int cname##_ecb_cipher(EVP_CIPHER_CTX *ctx, unsigned char *out, const unsigned char *in, size_t inl) \
{\
BLOCK_CIPHER_ecb_loop() \
cprefix##_ecb_encrypt(in + i, out + i, &((kstruct *)ctx->cipher_data)->ksched, ctx->encrypt);\
return 1;\
}
#define EVP_MAXCHUNK ((size_t)1<<(sizeof(long)*8-2))
#define BLOCK_CIPHER_func_ofb(cname, cprefix, cbits, kstruct, ksched) \
static int cname##_ofb_cipher(EVP_CIPHER_CTX *ctx, unsigned char *out, const unsigned char *in, size_t inl) \
{\
while(inl>=EVP_MAXCHUNK)\
{\
cprefix##_ofb##cbits##_encrypt(in, out, (long)EVP_MAXCHUNK, &((kstruct *)ctx->cipher_data)->ksched, ctx->iv, &ctx->num);\
inl-=EVP_MAXCHUNK;\
in +=EVP_MAXCHUNK;\
out+=EVP_MAXCHUNK;\
}\
if (inl)\
cprefix##_ofb##cbits##_encrypt(in, out, (long)inl, &((kstruct *)ctx->cipher_data)->ksched, ctx->iv, &ctx->num);\
return 1;\
}
#define BLOCK_CIPHER_func_cbc(cname, cprefix, kstruct, ksched) \
static int cname##_cbc_cipher(EVP_CIPHER_CTX *ctx, unsigned char *out, const unsigned char *in, size_t inl) \
{\
while(inl>=EVP_MAXCHUNK) \
{\
cprefix##_cbc_encrypt(in, out, (long)EVP_MAXCHUNK, &((kstruct *)ctx->cipher_data)->ksched, ctx->iv, ctx->encrypt);\
inl-=EVP_MAXCHUNK;\
in +=EVP_MAXCHUNK;\
out+=EVP_MAXCHUNK;\
}\
if (inl)\
cprefix##_cbc_encrypt(in, out, (long)inl, &((kstruct *)ctx->cipher_data)->ksched, ctx->iv, ctx->encrypt);\
return 1;\
}
#define BLOCK_CIPHER_func_cfb(cname, cprefix, cbits, kstruct, ksched) \
static int cname##_cfb##cbits##_cipher(EVP_CIPHER_CTX *ctx, unsigned char *out, const unsigned char *in, size_t inl) \
{\
size_t chunk=EVP_MAXCHUNK;\
if (cbits==1)  chunk>>=3;\
if (inl<chunk) chunk=inl;\
while(inl && inl>=chunk)\
{\
cprefix##_cfb##cbits##_encrypt(in, out, (long)((cbits==1) && !(ctx->flags & EVP_CIPH_FLAG_LENGTH_BITS) ?inl*8:inl), &((kstruct *)ctx->cipher_data)->ksched, ctx->iv, &ctx->num, ctx->encrypt);\
inl-=chunk;\
in +=chunk;\
out+=chunk;\
if(inl<chunk) chunk=inl;\
}\
return 1;\
}
#define BLOCK_CIPHER_all_funcs(cname, cprefix, cbits, kstruct, ksched) \
BLOCK_CIPHER_func_cbc(cname, cprefix, kstruct, ksched) \
BLOCK_CIPHER_func_cfb(cname, cprefix, cbits, kstruct, ksched) \
BLOCK_CIPHER_func_ecb(cname, cprefix, kstruct, ksched) \
BLOCK_CIPHER_func_ofb(cname, cprefix, cbits, kstruct, ksched)
#define BLOCK_CIPHER_def1(cname, nmode, mode, MODE, kstruct, nid, block_size, \
key_len, iv_len, flags, init_key, cleanup, \
set_asn1, get_asn1, ctrl) \
static const EVP_CIPHER cname##_##mode = { \
nid##_##nmode, block_size, key_len, iv_len, \
flags | EVP_CIPH_##MODE##_MODE, \
init_key, \
cname##_##mode##_cipher, \
cleanup, \
sizeof(kstruct), \
set_asn1, get_asn1,\
ctrl, \
NULL \
}; \
const EVP_CIPHER *EVP_##cname##_##mode(void) { return &cname##_##mode; }
#define BLOCK_CIPHER_def_cbc(cname, kstruct, nid, block_size, key_len, \
iv_len, flags, init_key, cleanup, set_asn1, \
get_asn1, ctrl) \
BLOCK_CIPHER_def1(cname, cbc, cbc, CBC, kstruct, nid, block_size, key_len, \
iv_len, flags, init_key, cleanup, set_asn1, get_asn1, ctrl)
#define BLOCK_CIPHER_def_cfb(cname, kstruct, nid, key_len, \
iv_len, cbits, flags, init_key, cleanup, \
set_asn1, get_asn1, ctrl) \
BLOCK_CIPHER_def1(cname, cfb##cbits, cfb##cbits, CFB, kstruct, nid, 1, \
key_len, iv_len, flags, init_key, cleanup, set_asn1, \
get_asn1, ctrl)
#define BLOCK_CIPHER_def_ofb(cname, kstruct, nid, key_len, \
iv_len, cbits, flags, init_key, cleanup, \
set_asn1, get_asn1, ctrl) \
BLOCK_CIPHER_def1(cname, ofb##cbits, ofb, OFB, kstruct, nid, 1, \
key_len, iv_len, flags, init_key, cleanup, set_asn1, \
get_asn1, ctrl)
#define BLOCK_CIPHER_def_ecb(cname, kstruct, nid, block_size, key_len, \
flags, init_key, cleanup, set_asn1, \
get_asn1, ctrl) \
BLOCK_CIPHER_def1(cname, ecb, ecb, ECB, kstruct, nid, block_size, key_len, \
0, flags, init_key, cleanup, set_asn1, get_asn1, ctrl)
#define BLOCK_CIPHER_defs(cname, kstruct, \
nid, block_size, key_len, iv_len, cbits, flags, \
init_key, cleanup, set_asn1, get_asn1, ctrl) \
BLOCK_CIPHER_def_cbc(cname, kstruct, nid, block_size, key_len, iv_len, flags, \
init_key, cleanup, set_asn1, get_asn1, ctrl) \
BLOCK_CIPHER_def_cfb(cname, kstruct, nid, key_len, iv_len, cbits, \
flags, init_key, cleanup, set_asn1, get_asn1, ctrl) \
BLOCK_CIPHER_def_ofb(cname, kstruct, nid, key_len, iv_len, cbits, \
flags, init_key, cleanup, set_asn1, get_asn1, ctrl) \
BLOCK_CIPHER_def_ecb(cname, kstruct, nid, block_size, key_len, flags, \
init_key, cleanup, set_asn1, get_asn1, ctrl)
#define IMPLEMENT_BLOCK_CIPHER(cname, ksched, cprefix, kstruct, nid, \
block_size, key_len, iv_len, cbits, \
flags, init_key, \
cleanup, set_asn1, get_asn1, ctrl) \
BLOCK_CIPHER_all_funcs(cname, cprefix, cbits, kstruct, ksched) \
BLOCK_CIPHER_defs(cname, kstruct, nid, block_size, key_len, iv_len, \
cbits, flags, init_key, cleanup, set_asn1, \
get_asn1, ctrl)
#define EVP_C_DATA(kstruct, ctx)        ((kstruct *)(ctx)->cipher_data)
#define IMPLEMENT_CFBR(cipher,cprefix,kstruct,ksched,keysize,cbits,iv_len) \
BLOCK_CIPHER_func_cfb(cipher##_##keysize,cprefix,cbits,kstruct,ksched) \
BLOCK_CIPHER_def_cfb(cipher##_##keysize,kstruct, \
NID_##cipher##_##keysize, keysize/8, iv_len, cbits, \
0, cipher##_init_key, NULL, \
EVP_CIPHER_set_asn1_iv, \
EVP_CIPHER_get_asn1_iv, \
NULL)
struct evp_pkey_ctx_st {
const EVP_PKEY_METHOD *pmeth;
ENGINE *engine;
EVP_PKEY *pkey;
EVP_PKEY *peerkey;
int operation;
void *data;
void *app_data;
EVP_PKEY_gen_cb *pkey_gencb;
int *keygen_info;
int keygen_info_count;
}  ;
#define EVP_PKEY_FLAG_DYNAMIC   1
struct evp_pkey_method_st {
int pkey_id;
int flags;
int (*init) (EVP_PKEY_CTX *ctx);
int (*copy) (EVP_PKEY_CTX *dst, EVP_PKEY_CTX *src);
void (*cleanup) (EVP_PKEY_CTX *ctx);
int (*paramgen_init) (EVP_PKEY_CTX *ctx);
int (*paramgen) (EVP_PKEY_CTX *ctx, EVP_PKEY *pkey);
int (*keygen_init) (EVP_PKEY_CTX *ctx);
int (*keygen) (EVP_PKEY_CTX *ctx, EVP_PKEY *pkey);
int (*sign_init) (EVP_PKEY_CTX *ctx);
int (*sign) (EVP_PKEY_CTX *ctx, unsigned char *sig, size_t *siglen,
const unsigned char *tbs, size_t tbslen);
int (*verify_init) (EVP_PKEY_CTX *ctx);
int (*verify) (EVP_PKEY_CTX *ctx,
const unsigned char *sig, size_t siglen,
const unsigned char *tbs, size_t tbslen);
int (*verify_recover_init) (EVP_PKEY_CTX *ctx);
int (*verify_recover) (EVP_PKEY_CTX *ctx,
unsigned char *rout, size_t *routlen,
const unsigned char *sig, size_t siglen);
int (*signctx_init) (EVP_PKEY_CTX *ctx, EVP_MD_CTX *mctx);
int (*signctx) (EVP_PKEY_CTX *ctx, unsigned char *sig, size_t *siglen,
EVP_MD_CTX *mctx);
int (*verifyctx_init) (EVP_PKEY_CTX *ctx, EVP_MD_CTX *mctx);
int (*verifyctx) (EVP_PKEY_CTX *ctx, const unsigned char *sig, int siglen,
EVP_MD_CTX *mctx);
int (*encrypt_init) (EVP_PKEY_CTX *ctx);
int (*encrypt) (EVP_PKEY_CTX *ctx, unsigned char *out, size_t *outlen,
const unsigned char *in, size_t inlen);
int (*decrypt_init) (EVP_PKEY_CTX *ctx);
int (*decrypt) (EVP_PKEY_CTX *ctx, unsigned char *out, size_t *outlen,
const unsigned char *in, size_t inlen);
int (*derive_init) (EVP_PKEY_CTX *ctx);
int (*derive) (EVP_PKEY_CTX *ctx, unsigned char *key, size_t *keylen);
int (*ctrl) (EVP_PKEY_CTX *ctx, int type, int p1, void *p2);
int (*ctrl_str) (EVP_PKEY_CTX *ctx, const char *type, const char *value);
}  ;
void evp_pkey_set_cb_translate(BN_GENCB *cb, EVP_PKEY_CTX *ctx);
int PKCS5_v2_PBKDF2_keyivgen(EVP_CIPHER_CTX *ctx, const char *pass,
int passlen, ASN1_TYPE *param,
const EVP_CIPHER *c, const EVP_MD *md,
int en_de);
#ifdef OPENSSL_FIPS
# ifdef OPENSSL_DOING_MAKEDEPEND
#  undef SHA1_Init
#  undef SHA1_Update
#  undef SHA224_Init
#  undef SHA256_Init
#  undef SHA384_Init
#  undef SHA512_Init
#  undef DES_set_key_unchecked
# endif
# define RIPEMD160_Init  private_RIPEMD160_Init
# define WHIRLPOOL_Init  private_WHIRLPOOL_Init
# define MD5_Init        private_MD5_Init
# define MD4_Init        private_MD4_Init
# define MD2_Init        private_MD2_Init
# define MDC2_Init       private_MDC2_Init
# define SHA_Init        private_SHA_Init
# define SHA1_Init       private_SHA1_Init
# define SHA224_Init     private_SHA224_Init
# define SHA256_Init     private_SHA256_Init
# define SHA384_Init     private_SHA384_Init
# define SHA512_Init     private_SHA512_Init
# define BF_set_key      private_BF_set_key
# define CAST_set_key    private_CAST_set_key
# define idea_set_encrypt_key    private_idea_set_encrypt_key
# define SEED_set_key    private_SEED_set_key
# define RC2_set_key     private_RC2_set_key
# define RC4_set_key     private_RC4_set_key
# define DES_set_key_unchecked   private_DES_set_key_unchecked
# define Camellia_set_key        private_Camellia_set_key
#endif