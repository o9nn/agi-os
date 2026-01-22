#ifndef CRYPT_BLOWFISH_H
#define CRYPT_BLOWFISH_H
extern int crypt_output_magic(const char *setting, char *output, size_t size);
extern char *crypt_blowfish_rn(const char *key, const char *setting,
char *output, size_t size);
extern char *crypt_gensalt_blowfish_rn(const char *prefix,
unsigned long count,
const char *input, size_t size, char *output, size_t output_size);
#endif