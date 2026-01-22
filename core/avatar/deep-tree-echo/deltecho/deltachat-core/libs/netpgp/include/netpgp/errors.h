#ifndef ERRORS_H_
#define ERRORS_H_
#include <errno.h>
#ifndef __printflike
#define __printflike(n, m) __attribute__((format(printf,n,m)))
#endif
typedef enum {
PGP_E_OK = 0x0000,
PGP_E_FAIL = 0x0001,
PGP_E_SYSTEM_ERROR = 0x0002,
PGP_E_UNIMPLEMENTED = 0x0003,
PGP_E_R = 0x1000,
PGP_E_R_READ_FAILED = PGP_E_R + 1,
PGP_E_R_EARLY_EOF = PGP_E_R + 2,
PGP_E_R_BAD_FORMAT = PGP_E_R + 3,
PGP_E_R_UNSUPPORTED = PGP_E_R + 4,
PGP_E_R_UNCONSUMED_DATA = PGP_E_R + 5,
PGP_E_W = 0x2000,
PGP_E_W_WRITE_FAILED = PGP_E_W + 1,
PGP_E_W_WRITE_TOO_SHORT = PGP_E_W + 2,
PGP_E_P = 0x3000,
PGP_E_P_NOT_ENOUGH_DATA = PGP_E_P + 1,
PGP_E_P_UNKNOWN_TAG = PGP_E_P + 2,
PGP_E_P_PACKET_CONSUMED = PGP_E_P + 3,
PGP_E_P_MPI_FORMAT_ERROR = PGP_E_P + 4,
PGP_E_P_PACKET_NOT_CONSUMED = PGP_E_P + 5,
PGP_E_P_DECOMPRESSION_ERROR = PGP_E_P + 6,
PGP_E_P_NO_USERID = PGP_E_P + 7,
PGP_E_C = 0x4000,
PGP_E_V = 0x5000,
PGP_E_V_BAD_SIGNATURE = PGP_E_V + 1,
PGP_E_V_NO_SIGNATURE = PGP_E_V + 2,
PGP_E_V_UNKNOWN_SIGNER = PGP_E_V + 3,
PGP_E_V_BAD_HASH = PGP_E_V + 4,
PGP_E_ALG = 0x6000,
PGP_E_ALG_UNSUPPORTED_SYMMETRIC_ALG = PGP_E_ALG + 1,
PGP_E_ALG_UNSUPPORTED_PUBLIC_KEY_ALG = PGP_E_ALG + 2,
PGP_E_ALG_UNSUPPORTED_SIGNATURE_ALG = PGP_E_ALG + 3,
PGP_E_ALG_UNSUPPORTED_HASH_ALG = PGP_E_ALG + 4,
PGP_E_ALG_UNSUPPORTED_COMPRESS_ALG = PGP_E_ALG + 5,
PGP_E_PROTO = 0x7000,
PGP_E_PROTO_BAD_SYMMETRIC_DECRYPT = PGP_E_PROTO + 2,
PGP_E_PROTO_UNKNOWN_SS = PGP_E_PROTO + 3,
PGP_E_PROTO_CRITICAL_SS_IGNORED = PGP_E_PROTO + 4,
PGP_E_PROTO_BAD_PUBLIC_KEY_VRSN = PGP_E_PROTO + 5,
PGP_E_PROTO_BAD_SIGNATURE_VRSN = PGP_E_PROTO + 6,
PGP_E_PROTO_BAD_ONE_PASS_SIG_VRSN = PGP_E_PROTO + 7,
PGP_E_PROTO_BAD_PKSK_VRSN = PGP_E_PROTO + 8,
PGP_E_PROTO_DECRYPTED_MSG_WRONG_LEN = PGP_E_PROTO + 9,
PGP_E_PROTO_BAD_SK_CHECKSUM = PGP_E_PROTO + 10
} pgp_errcode_t;
typedef struct pgp_error {
pgp_errcode_t errcode;
int sys_errno;
char *comment;
const char *file;
int line;
struct pgp_error *next;
} pgp_error_t;
const char *pgp_errcode(const pgp_errcode_t);
void
pgp_push_error(pgp_error_t **, pgp_errcode_t,
int,
const char *, int, const char *,...) __printflike(6, 7);
void pgp_print_error(pgp_error_t *);
void pgp_print_errors(pgp_error_t *);
void pgp_free_errors(pgp_error_t *);
int pgp_has_error(pgp_error_t *, pgp_errcode_t);
#define PGP_SYSTEM_ERROR_1(err,code,sys,fmt,arg) do { \
pgp_push_error(err,PGP_E_SYSTEM_ERROR,errno,__FILE__,__LINE__,sys);\
pgp_push_error(err,code,0,__FILE__,__LINE__,fmt,arg); \
} while(0)
#define PGP_MEMORY_ERROR(err) { \
fprintf(stderr, "Memory error\n"); \
}
#define PGP_ERROR_1(err,code,fmt,arg) do { \
pgp_push_error(err,code,0,__FILE__,__LINE__,fmt,arg); \
} while(0)
#define PGP_ERROR_2(err,code,fmt,arg,arg2) do { \
pgp_push_error(err,code,0,__FILE__,__LINE__,fmt,arg,arg2); \
} while(0)
#define PGP_ERROR_3(err,code,fmt,arg,arg2,arg3) do { \
pgp_push_error(err,code,0,__FILE__,__LINE__,fmt,arg,arg2,arg3); \
} while(0)
#define PGP_ERROR_4(err,code,fmt,arg,arg2,arg3,arg4) do { \
pgp_push_error(err,code,0,__FILE__,__LINE__,fmt,arg,arg2,arg3,arg4); \
} while(0)
#endif