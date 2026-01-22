#ifndef NETPGPSDK_H_
#define NETPGPSDK_H_
#include "keyring.h"
#include "crypto.h"
#include "signature.h"
#include "packet-show.h"
#ifndef __printflike
#define __printflike(n, m) __attribute__((format(printf,n,m)))
#endif
void pgp_validate_result_free(pgp_validation_t *);
unsigned
pgp_validate_all_sigs(pgp_validation_t *,
const pgp_keyring_t *,
pgp_cb_ret_t cb(const pgp_packet_t *, pgp_cbdata_t *));
unsigned pgp_check_sig(const uint8_t *,
unsigned, const pgp_sig_t *, const pgp_pubkey_t *);
const char *pgp_get_info(const char *type);
int pgp_asprintf(char **, const char *, ...) __printflike(2, 3);
void netpgp_log(const char *, ...) __printflike(1, 2);
int netpgp_strcasecmp(const char *, const char *);
char *netpgp_strdup(const char *);
#endif