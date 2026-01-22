#ifndef PASSWORD_SCHEME_H
#define PASSWORD_SCHEME_H
#define AUTH_LOG_MSG_PASSWORD_MISMATCH "Password mismatch"
struct hash_method;
enum password_encoding {
PW_ENCODING_NONE,
PW_ENCODING_BASE64,
PW_ENCODING_HEX
};
struct password_generate_params {
const char *user;
unsigned int rounds;
};
struct password_scheme {
const char *name;
enum password_encoding default_encoding;
unsigned int raw_password_len;
bool weak;
int (*password_verify)(const char *plaintext,
const struct password_generate_params *params,
const unsigned char *raw_password, size_t size,
const char **error_r);
void (*password_generate)(const char *plaintext,
const struct password_generate_params *params,
const unsigned char **raw_password_r,
size_t *size_r);
};
ARRAY_DEFINE_TYPE(password_scheme_p, const struct password_scheme *);
void password_schemes_get(ARRAY_TYPE(password_scheme_p) *schemes_r);
extern unsigned int password_scheme_encryption_rounds;
int password_verify(const char *plaintext,
const struct password_generate_params *params,
const char *scheme,
const unsigned char *raw_password, size_t size,
const char **error_r);
const char *password_get_scheme(const char **password);
int password_decode(const char *password, const char *scheme,
const unsigned char **raw_password_r, size_t *size_r,
const char **error_r);
bool password_generate(const char *plaintext,
const struct password_generate_params *params,
const char *scheme,
const unsigned char **raw_password_r, size_t *size_r);
bool password_generate_encoded(const char *plaintext,
const struct password_generate_params *params,
const char *scheme, const char **password_r);
bool password_scheme_is_alias(const char *scheme1, const char *scheme2);
const char *
password_scheme_detect(const char *plain_password, const char *crypted_password,
const struct password_generate_params *params);
void password_scheme_register(const struct password_scheme *scheme);
void password_scheme_unregister(const struct password_scheme *scheme);
void password_schemes_init(void);
void password_schemes_allow_weak(bool allow);
void password_schemes_deinit(void);
void password_set_encryption_rounds(unsigned int rounds);
const char *password_generate_salt(size_t len);
const char *password_generate_md5_crypt(const char *pw, const char *salt);
int password_generate_otp(const char *pw, const char *state_data,
unsigned int algo, const char **result_r)
ATTR_NULL(2);
int crypt_verify(const char *plaintext,
const struct password_generate_params *params,
const unsigned char *raw_password, size_t size,
const char **error_r);
int scram_scheme_parse(const struct hash_method *hmethod, const char *name,
const unsigned char *credentials, size_t size,
unsigned int *iter_count_r, const char **salt_r,
unsigned char stored_key_r[],
unsigned char server_key_r[], const char **error_r);
int scram_verify(const struct hash_method *hmethod, const char *scheme_name,
const char *plaintext, const unsigned char *raw_password,
size_t size, const char **error_r);
void scram_generate(const struct hash_method *hmethod, const char *plaintext,
unsigned int rounds, const unsigned char **raw_password_r,
size_t *size_r);
int scram_sha1_verify(const char *plaintext,
const struct password_generate_params *params ATTR_UNUSED,
const unsigned char *raw_password, size_t size,
const char **error_r ATTR_UNUSED);
void scram_sha1_generate(const char *plaintext,
const struct password_generate_params *params ATTR_UNUSED,
const unsigned char **raw_password_r, size_t *size_r);
int scram_sha256_verify(const char *plaintext,
const struct password_generate_params *params ATTR_UNUSED,
const unsigned char *raw_password, size_t size,
const char **error_r);
void scram_sha256_generate(const char *plaintext,
const struct password_generate_params *params ATTR_UNUSED,
const unsigned char **raw_password_r, size_t *size_r);
void pbkdf2_generate(const char *plaintext,
const struct password_generate_params *params ATTR_UNUSED,
const unsigned char **raw_password_r, size_t *size_r);
int pbkdf2_verify(const char *plaintext,
const struct password_generate_params *params ATTR_UNUSED,
const unsigned char *raw_password, size_t size,
const char **error_r);
void password_scheme_register_crypt(void);
#ifdef HAVE_LIBSODIUM
void password_scheme_register_sodium(void);
#endif
#endif