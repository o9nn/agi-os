#ifndef KSSL_LCL_H
# define KSSL_LCL_H
# include <openssl/kssl.h>
# ifndef OPENSSL_NO_KRB5
#ifdef  __cplusplus
extern "C" {
#endif
void print_krb5_data(char *label, krb5_data *kdata);
void print_krb5_authdata(char *label, krb5_authdata **adata);
void print_krb5_keyblock(char *label, krb5_keyblock *keyblk);
char *kstring(char *string);
char *knumber(int len, krb5_octet *contents);
const EVP_CIPHER *kssl_map_enc(krb5_enctype enctype);
int kssl_keytab_is_available(KSSL_CTX *kssl_ctx);
int kssl_tgt_is_available(KSSL_CTX *kssl_ctx);
#ifdef  __cplusplus
}
#endif
# endif
#endif