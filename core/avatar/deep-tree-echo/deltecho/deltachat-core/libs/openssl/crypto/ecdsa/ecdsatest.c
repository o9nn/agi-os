#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/opensslconf.h>
#ifdef OPENSSL_NO_ECDSA
int main(int argc, char *argv[])
{
puts("Elliptic curves are disabled.");
return 0;
}
#else
# include <openssl/crypto.h>
# include <openssl/bio.h>
# include <openssl/evp.h>
# include <openssl/bn.h>
# include <openssl/ecdsa.h>
# ifndef OPENSSL_NO_ENGINE
#  include <openssl/engine.h>
# endif
# include <openssl/err.h>
# include <openssl/rand.h>
static const char rnd_seed[] = "string to make the random number generator "
"think it has entropy";
int x9_62_tests(BIO *);
int x9_62_test_internal(BIO *out, int nid, const char *r, const char *s);
int test_builtin(BIO *);
int change_rand(void);
int restore_rand(void);
int fbytes(unsigned char *buf, int num);
RAND_METHOD fake_rand;
const RAND_METHOD *old_rand;
int change_rand(void)
{
if ((old_rand = RAND_get_rand_method()) == NULL)
return 0;
fake_rand.seed = old_rand->seed;
fake_rand.cleanup = old_rand->cleanup;
fake_rand.add = old_rand->add;
fake_rand.status = old_rand->status;
fake_rand.bytes = fbytes;
fake_rand.pseudorand = old_rand->bytes;
if (!RAND_set_rand_method(&fake_rand))
return 0;
return 1;
}
int restore_rand(void)
{
if (!RAND_set_rand_method(old_rand))
return 0;
else
return 1;
}
static int fbytes_counter = 0;
static const char *numbers[8] = {
"651056770906015076056810763456358567190100156695615665659",
"6140507067065001063065065565667405560006161556565665656654",
"8763001015071075675010661307616710783570106710677817767166"
"71676178726717",
"7000000175690566466555057817571571075705015757757057795755"
"55657156756655",
"1275552191113212300012030439187146164646146646466749494799",
"1542725565216523985789236956265265265235675811949404040041",
"1456427555219115346513212300075341203043918714616464614664"
"64667494947990",
"1712787255652165239672857892369562652652652356758119494040"
"40041670216363"
};
int fbytes(unsigned char *buf, int num)
{
int ret;
BIGNUM *tmp = NULL;
if (fbytes_counter >= 8)
return 0;
tmp = BN_new();
if (!tmp)
return 0;
if (!BN_dec2bn(&tmp, numbers[fbytes_counter])) {
BN_free(tmp);
return 0;
}
fbytes_counter++;
if (num != BN_num_bytes(tmp) || !BN_bn2bin(tmp, buf))
ret = 0;
else
ret = 1;
if (tmp)
BN_free(tmp);
return ret;
}
int x9_62_test_internal(BIO *out, int nid, const char *r_in, const char *s_in)
{
int ret = 0;
const char message[] = "abc";
unsigned char digest[20];
unsigned int dgst_len = 0;
EVP_MD_CTX md_ctx;
EC_KEY *key = NULL;
ECDSA_SIG *signature = NULL;
BIGNUM *r = NULL, *s = NULL;
EVP_MD_CTX_init(&md_ctx);
EVP_DigestInit(&md_ctx, EVP_ecdsa());
EVP_DigestUpdate(&md_ctx, (const void *)message, 3);
EVP_DigestFinal(&md_ctx, digest, &dgst_len);
BIO_printf(out, "testing %s: ", OBJ_nid2sn(nid));
if ((key = EC_KEY_new_by_curve_name(nid)) == NULL)
goto x962_int_err;
if (!EC_KEY_generate_key(key))
goto x962_int_err;
BIO_printf(out, ".");
(void)BIO_flush(out);
signature = ECDSA_do_sign(digest, 20, key);
if (signature == NULL)
goto x962_int_err;
BIO_printf(out, ".");
(void)BIO_flush(out);
if ((r = BN_new()) == NULL || (s = BN_new()) == NULL)
goto x962_int_err;
if (!BN_dec2bn(&r, r_in) || !BN_dec2bn(&s, s_in))
goto x962_int_err;
if (BN_cmp(signature->r, r) || BN_cmp(signature->s, s))
goto x962_int_err;
BIO_printf(out, ".");
(void)BIO_flush(out);
if (ECDSA_do_verify(digest, 20, signature, key) != 1)
goto x962_int_err;
BIO_printf(out, ".");
(void)BIO_flush(out);
BIO_printf(out, " ok\n");
ret = 1;
x962_int_err:
if (!ret)
BIO_printf(out, " failed\n");
if (key)
EC_KEY_free(key);
if (signature)
ECDSA_SIG_free(signature);
if (r)
BN_free(r);
if (s)
BN_free(s);
EVP_MD_CTX_cleanup(&md_ctx);
return ret;
}
int x9_62_tests(BIO *out)
{
int ret = 0;
BIO_printf(out, "some tests from X9.62:\n");
if (!change_rand())
goto x962_err;
if (!x9_62_test_internal(out, NID_X9_62_prime192v1,
"3342403536405981729393488334694600415596881826869351677613",
"5735822328888155254683894997897571951568553642892029982342"))
goto x962_err;
if (!x9_62_test_internal(out, NID_X9_62_prime239v1,
"3086361431751678114926225473006680188549593787585317781474"
"62058306432176",
"3238135532097973577080787768312505059318910517550078427819"
"78505179448783"))
goto x962_err;
# ifndef OPENSSL_NO_EC2M
if (!x9_62_test_internal(out, NID_X9_62_c2tnb191v1,
"87194383164871543355722284926904419997237591535066528048",
"308992691965804947361541664549085895292153777025772063598"))
goto x962_err;
if (!x9_62_test_internal(out, NID_X9_62_c2tnb239v1,
"2159633321041961198501834003903461262881815148684178964245"
"5876922391552",
"1970303740007316867383349976549972270528498040721988191026"
"49413465737174"))
goto x962_err;
# endif
ret = 1;
x962_err:
if (!restore_rand())
ret = 0;
return ret;
}
int test_builtin(BIO *out)
{
EC_builtin_curve *curves = NULL;
size_t crv_len = 0, n = 0;
EC_KEY *eckey = NULL, *wrong_eckey = NULL;
EC_GROUP *group;
ECDSA_SIG *ecdsa_sig = NULL;
unsigned char digest[20], wrong_digest[20];
unsigned char *signature = NULL;
const unsigned char *sig_ptr;
unsigned char *sig_ptr2;
unsigned char *raw_buf = NULL;
unsigned int sig_len, degree, r_len, s_len, bn_len, buf_len;
int nid, ret = 0;
if (RAND_pseudo_bytes(digest, 20) <= 0 ||
RAND_pseudo_bytes(wrong_digest, 20) <= 0) {
BIO_printf(out, "ERROR: unable to get random data\n");
goto builtin_err;
}
BIO_printf(out, "\ntesting ECDSA_sign() and ECDSA_verify() "
"with some internal curves:\n");
crv_len = EC_get_builtin_curves(NULL, 0);
curves = OPENSSL_malloc(sizeof(EC_builtin_curve) * crv_len);
if (curves == NULL) {
BIO_printf(out, "malloc error\n");
goto builtin_err;
}
if (!EC_get_builtin_curves(curves, crv_len)) {
BIO_printf(out, "unable to get internal curves\n");
goto builtin_err;
}
for (n = 0; n < crv_len; n++) {
unsigned char dirt, offset;
nid = curves[n].nid;
if (nid == NID_ipsec4)
continue;
if ((eckey = EC_KEY_new()) == NULL)
goto builtin_err;
group = EC_GROUP_new_by_curve_name(nid);
if (group == NULL)
goto builtin_err;
if (EC_KEY_set_group(eckey, group) == 0)
goto builtin_err;
EC_GROUP_free(group);
degree = EC_GROUP_get_degree(EC_KEY_get0_group(eckey));
if (degree < 160)
{
EC_KEY_free(eckey);
eckey = NULL;
continue;
}
BIO_printf(out, "%s: ", OBJ_nid2sn(nid));
if (!EC_KEY_generate_key(eckey)) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
if ((wrong_eckey = EC_KEY_new()) == NULL)
goto builtin_err;
group = EC_GROUP_new_by_curve_name(nid);
if (group == NULL)
goto builtin_err;
if (EC_KEY_set_group(wrong_eckey, group) == 0)
goto builtin_err;
EC_GROUP_free(group);
if (!EC_KEY_generate_key(wrong_eckey)) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
BIO_printf(out, ".");
(void)BIO_flush(out);
if (!EC_KEY_check_key(eckey)) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
BIO_printf(out, ".");
(void)BIO_flush(out);
sig_len = ECDSA_size(eckey);
if ((signature = OPENSSL_malloc(sig_len)) == NULL)
goto builtin_err;
if (!ECDSA_sign(0, digest, 20, signature, &sig_len, eckey)) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
BIO_printf(out, ".");
(void)BIO_flush(out);
if (ECDSA_verify(0, digest, 20, signature, sig_len, eckey) != 1) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
BIO_printf(out, ".");
(void)BIO_flush(out);
if (ECDSA_verify(0, digest, 20, signature, sig_len, wrong_eckey) == 1) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
BIO_printf(out, ".");
(void)BIO_flush(out);
if (ECDSA_verify(0, wrong_digest, 20, signature, sig_len, eckey) == 1) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
BIO_printf(out, ".");
(void)BIO_flush(out);
if (ECDSA_verify(0, digest, 20, signature, sig_len - 1, eckey) == 1) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
BIO_printf(out, ".");
(void)BIO_flush(out);
sig_ptr = signature;
if ((ecdsa_sig = d2i_ECDSA_SIG(NULL, &sig_ptr, sig_len)) == NULL) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
r_len = BN_num_bytes(ecdsa_sig->r);
s_len = BN_num_bytes(ecdsa_sig->s);
bn_len = (degree + 7) / 8;
if ((r_len > bn_len) || (s_len > bn_len)) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
buf_len = 2 * bn_len;
if ((raw_buf = OPENSSL_malloc(buf_len)) == NULL)
goto builtin_err;
memset(raw_buf, 0, buf_len);
BN_bn2bin(ecdsa_sig->r, raw_buf + bn_len - r_len);
BN_bn2bin(ecdsa_sig->s, raw_buf + buf_len - s_len);
offset = raw_buf[10] % buf_len;
dirt = raw_buf[11] ? raw_buf[11] : 1;
raw_buf[offset] ^= dirt;
if ((BN_bin2bn(raw_buf, bn_len, ecdsa_sig->r) == NULL) ||
(BN_bin2bn(raw_buf + bn_len, bn_len, ecdsa_sig->s) == NULL))
goto builtin_err;
sig_ptr2 = signature;
sig_len = i2d_ECDSA_SIG(ecdsa_sig, &sig_ptr2);
if (ECDSA_verify(0, digest, 20, signature, sig_len, eckey) == 1) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
raw_buf[offset] ^= dirt;
if ((BN_bin2bn(raw_buf, bn_len, ecdsa_sig->r) == NULL) ||
(BN_bin2bn(raw_buf + bn_len, bn_len, ecdsa_sig->s) == NULL))
goto builtin_err;
sig_ptr2 = signature;
sig_len = i2d_ECDSA_SIG(ecdsa_sig, &sig_ptr2);
if (ECDSA_verify(0, digest, 20, signature, sig_len, eckey) != 1) {
BIO_printf(out, " failed\n");
goto builtin_err;
}
BIO_printf(out, ".");
(void)BIO_flush(out);
BIO_printf(out, " ok\n");
ERR_clear_error();
OPENSSL_free(signature);
signature = NULL;
EC_KEY_free(eckey);
eckey = NULL;
EC_KEY_free(wrong_eckey);
wrong_eckey = NULL;
ECDSA_SIG_free(ecdsa_sig);
ecdsa_sig = NULL;
OPENSSL_free(raw_buf);
raw_buf = NULL;
}
ret = 1;
builtin_err:
if (eckey)
EC_KEY_free(eckey);
if (wrong_eckey)
EC_KEY_free(wrong_eckey);
if (ecdsa_sig)
ECDSA_SIG_free(ecdsa_sig);
if (signature)
OPENSSL_free(signature);
if (raw_buf)
OPENSSL_free(raw_buf);
if (curves)
OPENSSL_free(curves);
return ret;
}
int main(void)
{
int ret = 1;
BIO *out;
out = BIO_new_fp(stdout, BIO_NOCLOSE);
if (!((getenv("OPENSSL_DEBUG_MEMORY") != NULL) &&
(0 == strcmp(getenv("OPENSSL_DEBUG_MEMORY"), "off")))) {
CRYPTO_malloc_debug_init();
CRYPTO_set_mem_debug_options(V_CRYPTO_MDEBUG_ALL);
} else {
CRYPTO_set_mem_debug_functions(0, 0, 0, 0, 0);
}
CRYPTO_mem_ctrl(CRYPTO_MEM_CHECK_ON);
ERR_load_crypto_strings();
RAND_seed(rnd_seed, sizeof(rnd_seed));
if (!x9_62_tests(out))
goto err;
if (!test_builtin(out))
goto err;
ret = 0;
err:
if (ret)
BIO_printf(out, "\nECDSA test failed\n");
else
BIO_printf(out, "\nECDSA test passed\n");
if (ret)
ERR_print_errors(out);
CRYPTO_cleanup_all_ex_data();
ERR_remove_thread_state(NULL);
ERR_free_strings();
CRYPTO_mem_leaks(out);
if (out != NULL)
BIO_free(out);
return ret;
}
#endif