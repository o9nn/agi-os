#include <stdio.h>
#include <openssl/crypto.h>
#include <openssl/bio.h>
#include <openssl/x509.h>
#include <openssl/pem.h>
#include <openssl/err.h>
static STACK_OF(X509) *load_certs_from_file(const char *filename)
{
STACK_OF(X509) *certs;
BIO *bio;
X509 *x;
bio = BIO_new_file(filename, "r");
if (bio == NULL) {
return NULL;
}
certs = sk_X509_new_null();
if (certs == NULL) {
BIO_free(bio);
return NULL;
}
ERR_set_mark();
do {
x = PEM_read_bio_X509(bio, NULL, 0, NULL);
if (x != NULL && !sk_X509_push(certs, x)) {
sk_X509_pop_free(certs, X509_free);
BIO_free(bio);
return NULL;
} else if (x == NULL) {
ERR_pop_to_mark();
}
} while (x != NULL);
BIO_free(bio);
return certs;
}
static int test_alt_chains_cert_forgery(void)
{
int ret = 0;
int i;
X509 *x = NULL;
STACK_OF(X509) *untrusted = NULL;
BIO *bio = NULL;
X509_STORE_CTX *sctx = NULL;
X509_STORE *store = NULL;
X509_LOOKUP *lookup = NULL;
store = X509_STORE_new();
if (store == NULL)
goto err;
lookup = X509_STORE_add_lookup(store, X509_LOOKUP_file());
if (lookup == NULL)
goto err;
if(!X509_LOOKUP_load_file(lookup, "certs/roots.pem", X509_FILETYPE_PEM))
goto err;
untrusted = load_certs_from_file("certs/untrusted.pem");
if ((bio = BIO_new_file("certs/bad.pem", "r")) == NULL)
goto err;
if((x = PEM_read_bio_X509(bio, NULL, 0, NULL)) == NULL)
goto err;
sctx = X509_STORE_CTX_new();
if (sctx == NULL)
goto err;
if (!X509_STORE_CTX_init(sctx, store, x, untrusted))
goto err;
i = X509_verify_cert(sctx);
if(i == 0 && X509_STORE_CTX_get_error(sctx)
== X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT) {
ret = 1;
}
err:
X509_STORE_CTX_free(sctx);
X509_free(x);
BIO_free(bio);
sk_X509_pop_free(untrusted, X509_free);
X509_STORE_free(store);
if (ret != 1)
ERR_print_errors_fp(stderr);
return ret;
}
int main(void)
{
CRYPTO_malloc_debug_init();
CRYPTO_set_mem_debug_options(V_CRYPTO_MDEBUG_ALL);
CRYPTO_mem_ctrl(CRYPTO_MEM_CHECK_ON);
ERR_load_crypto_strings();
OpenSSL_add_all_digests();
if (!test_alt_chains_cert_forgery()) {
fprintf(stderr, "Test alt chains cert forgery failed\n");
return 1;
}
EVP_cleanup();
CRYPTO_cleanup_all_ex_data();
ERR_remove_thread_state(NULL);
ERR_free_strings();
CRYPTO_mem_leaks_fp(stderr);
printf("PASS\n");
return 0;
}