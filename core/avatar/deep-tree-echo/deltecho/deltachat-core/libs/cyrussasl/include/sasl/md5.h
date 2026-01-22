typedef struct {
UINT4 state[4];
UINT4 count[2];
unsigned char buffer[64];
} MD5_CTX;
#ifdef __cplusplus
extern "C" {
#endif
void _sasl_MD5Init (MD5_CTX *);
void _sasl_MD5Update (MD5_CTX *, const unsigned char *, unsigned int);
void _sasl_MD5Final (unsigned char [16], MD5_CTX *);
#ifdef __cplusplus
}
#endif