#include <stdio.h>
#include <stdlib.h>
#include <openssl/conf.h>
#include <openssl/err.h>
main()
{
LHASH *conf;
long eline;
char *s, *s2;
#ifdef USE_WIN32
CONF_set_default_method(CONF_WIN32);
#endif
conf = CONF_load(NULL, "ssleay.cnf", &eline);
if (conf == NULL) {
ERR_load_crypto_strings();
printf("unable to load configuration, line %ld\n", eline);
ERR_print_errors_fp(stderr);
exit(1);
}
lh_stats(conf, stdout);
lh_node_stats(conf, stdout);
lh_node_usage_stats(conf, stdout);
s = CONF_get_string(conf, NULL, "init2");
printf("init2=%s\n", (s == NULL) ? "NULL" : s);
s = CONF_get_string(conf, NULL, "cipher1");
printf("cipher1=%s\n", (s == NULL) ? "NULL" : s);
s = CONF_get_string(conf, "s_client", "cipher1");
printf("s_client:cipher1=%s\n", (s == NULL) ? "NULL" : s);
printf("---------------------------- DUMP ------------------------\n");
CONF_dump_fp(conf, stdout);
exit(0);
}