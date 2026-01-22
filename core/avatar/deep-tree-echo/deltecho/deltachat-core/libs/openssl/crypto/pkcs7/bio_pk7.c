#include <openssl/asn1.h>
#include <openssl/pkcs7.h>
#include <openssl/bio.h>
#if !defined(OPENSSL_SYSNAME_NETWARE) && !defined(OPENSSL_SYSNAME_VXWORKS)
# include <memory.h>
#endif
#include <stdio.h>
BIO *BIO_new_PKCS7(BIO *out, PKCS7 *p7)
{
return BIO_new_NDEF(out, (ASN1_VALUE *)p7, ASN1_ITEM_rptr(PKCS7));
}