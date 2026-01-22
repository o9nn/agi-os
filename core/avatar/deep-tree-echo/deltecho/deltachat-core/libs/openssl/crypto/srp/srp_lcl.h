#ifndef HEADER_SRP_LCL_H
# define HEADER_SRP_LCL_H
# include <openssl/srp.h>
# include <openssl/sha.h>
# if 0
#  define srp_bn_print(a) {fprintf(stderr, #a "="); BN_print_fp(stderr,a); \
fprintf(stderr,"\n");}
# else
#  define   srp_bn_print(a)
# endif
#ifdef  __cplusplus
extern "C" {
#endif
#ifdef  __cplusplus
}
#endif
#endif