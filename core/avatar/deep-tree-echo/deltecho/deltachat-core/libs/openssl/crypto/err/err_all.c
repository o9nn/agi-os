#include <stdio.h>
#include <openssl/asn1.h>
#include <openssl/bn.h>
#ifndef OPENSSL_NO_EC
# include <openssl/ec.h>
#endif
#include <openssl/buffer.h>
#include <openssl/bio.h>
#ifndef OPENSSL_NO_COMP
# include <openssl/comp.h>
#endif
#ifndef OPENSSL_NO_RSA
# include <openssl/rsa.h>
#endif
#ifndef OPENSSL_NO_DH
# include <openssl/dh.h>
#endif
#ifndef OPENSSL_NO_DSA
# include <openssl/dsa.h>
#endif
#ifndef OPENSSL_NO_ECDSA
# include <openssl/ecdsa.h>
#endif
#ifndef OPENSSL_NO_ECDH
# include <openssl/ecdh.h>
#endif
#include <openssl/evp.h>
#include <openssl/objects.h>
#include <openssl/pem2.h>
#include <openssl/x509.h>
#include <openssl/x509v3.h>
#include <openssl/conf.h>
#include <openssl/pkcs12.h>
#include <openssl/rand.h>
#include <openssl/dso.h>
#ifndef OPENSSL_NO_ENGINE
# include <openssl/engine.h>
#endif
#include <openssl/ui.h>
#include <openssl/ocsp.h>
#include <openssl/err.h>
#ifdef OPENSSL_FIPS
# include <openssl/fips.h>
#endif
#include <openssl/ts.h>
#ifndef OPENSSL_NO_CMS
# include <openssl/cms.h>
#endif
#ifndef OPENSSL_NO_JPAKE
# include <openssl/jpake.h>
#endif
void ERR_load_crypto_strings(void)
{
#ifndef OPENSSL_NO_ERR
ERR_load_ERR_strings();
ERR_load_BN_strings();
# ifndef OPENSSL_NO_RSA
ERR_load_RSA_strings();
# endif
# ifndef OPENSSL_NO_DH
ERR_load_DH_strings();
# endif
ERR_load_EVP_strings();
ERR_load_BUF_strings();
ERR_load_OBJ_strings();
ERR_load_PEM_strings();
# ifndef OPENSSL_NO_DSA
ERR_load_DSA_strings();
# endif
ERR_load_X509_strings();
ERR_load_ASN1_strings();
ERR_load_CONF_strings();
ERR_load_CRYPTO_strings();
# ifndef OPENSSL_NO_COMP
ERR_load_COMP_strings();
# endif
# ifndef OPENSSL_NO_EC
ERR_load_EC_strings();
# endif
# ifndef OPENSSL_NO_ECDSA
ERR_load_ECDSA_strings();
# endif
# ifndef OPENSSL_NO_ECDH
ERR_load_ECDH_strings();
# endif
ERR_load_BIO_strings();
ERR_load_PKCS7_strings();
ERR_load_X509V3_strings();
ERR_load_PKCS12_strings();
ERR_load_RAND_strings();
ERR_load_DSO_strings();
ERR_load_TS_strings();
# ifndef OPENSSL_NO_ENGINE
ERR_load_ENGINE_strings();
# endif
ERR_load_OCSP_strings();
ERR_load_UI_strings();
# ifdef OPENSSL_FIPS
ERR_load_FIPS_strings();
# endif
# ifndef OPENSSL_NO_CMS
ERR_load_CMS_strings();
# endif
# ifndef OPENSSL_NO_JPAKE
ERR_load_JPAKE_strings();
# endif
#endif
}