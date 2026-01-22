#include "ssl_locl.h"
#ifndef OPENSSL_NO_UNIT_TEST
static const struct openssl_ssl_test_functions ssl_test_functions = {
ssl_init_wbio_buffer,
ssl3_setup_buffers,
tls1_process_heartbeat,
dtls1_process_heartbeat
};
const struct openssl_ssl_test_functions *SSL_test_functions(void)
{
return &ssl_test_functions;
}
#endif