struct asn1_pctx_st {
unsigned long flags;
unsigned long nm_flags;
unsigned long cert_flags;
unsigned long oid_flags;
unsigned long str_flags;
} ;
struct evp_pkey_asn1_method_st {
int pkey_id;
int pkey_base_id;
unsigned long pkey_flags;
char *pem_str;
char *info;
int (*pub_decode) (EVP_PKEY *pk, X509_PUBKEY *pub);
int (*pub_encode) (X509_PUBKEY *pub, const EVP_PKEY *pk);
int (*pub_cmp) (const EVP_PKEY *a, const EVP_PKEY *b);
int (*pub_print) (BIO *out, const EVP_PKEY *pkey, int indent,
ASN1_PCTX *pctx);
int (*priv_decode) (EVP_PKEY *pk, PKCS8_PRIV_KEY_INFO *p8inf);
int (*priv_encode) (PKCS8_PRIV_KEY_INFO *p8, const EVP_PKEY *pk);
int (*priv_print) (BIO *out, const EVP_PKEY *pkey, int indent,
ASN1_PCTX *pctx);
int (*pkey_size) (const EVP_PKEY *pk);
int (*pkey_bits) (const EVP_PKEY *pk);
int (*param_decode) (EVP_PKEY *pkey,
const unsigned char **pder, int derlen);
int (*param_encode) (const EVP_PKEY *pkey, unsigned char **pder);
int (*param_missing) (const EVP_PKEY *pk);
int (*param_copy) (EVP_PKEY *to, const EVP_PKEY *from);
int (*param_cmp) (const EVP_PKEY *a, const EVP_PKEY *b);
int (*param_print) (BIO *out, const EVP_PKEY *pkey, int indent,
ASN1_PCTX *pctx);
int (*sig_print) (BIO *out,
const X509_ALGOR *sigalg, const ASN1_STRING *sig,
int indent, ASN1_PCTX *pctx);
void (*pkey_free) (EVP_PKEY *pkey);
int (*pkey_ctrl) (EVP_PKEY *pkey, int op, long arg1, void *arg2);
int (*old_priv_decode) (EVP_PKEY *pkey,
const unsigned char **pder, int derlen);
int (*old_priv_encode) (const EVP_PKEY *pkey, unsigned char **pder);
int (*item_verify) (EVP_MD_CTX *ctx, const ASN1_ITEM *it, void *asn,
X509_ALGOR *a, ASN1_BIT_STRING *sig, EVP_PKEY *pkey);
int (*item_sign) (EVP_MD_CTX *ctx, const ASN1_ITEM *it, void *asn,
X509_ALGOR *alg1, X509_ALGOR *alg2,
ASN1_BIT_STRING *sig);
} ;
#define X509_CRL_METHOD_DYNAMIC 1
struct x509_crl_method_st {
int flags;
int (*crl_init) (X509_CRL *crl);
int (*crl_free) (X509_CRL *crl);
int (*crl_lookup) (X509_CRL *crl, X509_REVOKED **ret,
ASN1_INTEGER *ser, X509_NAME *issuer);
int (*crl_verify) (X509_CRL *crl, EVP_PKEY *pk);
};