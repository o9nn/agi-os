#define DES_MAXLEN      65536
#define DES_QUICKLEN    16
#ifdef HEADER_DES_H
# undef ENCRYPT
# undef DECRYPT
#endif
enum desdir { ENCRYPT, DECRYPT };
enum desmode { CBC, ECB };
struct desparams {
unsigned char des_key[8];
enum desdir des_dir;
enum desmode des_mode;
unsigned char des_ivec[8];
unsigned des_len;
union {
unsigned char UDES_data[DES_QUICKLEN];
unsigned char *UDES_buf;
} UDES;
#define des_data UDES.UDES_data
#define des_buf  UDES.UDES_buf
};
#define DESIOCBLOCK     _IOWR('d', 6, struct desparams)
#define DESIOCQUICK     _IOWR('d', 7, struct desparams)