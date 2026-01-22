#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <openssl/lhash.h>
main()
{
LHASH *conf;
char buf[256];
int i;
conf = lh_new(lh_strhash, strcmp);
for (;;) {
char *p;
buf[0] = '\0';
fgets(buf, 256, stdin);
if (buf[0] == '\0')
break;
i = strlen(buf);
p = OPENSSL_malloc(i + 1);
memcpy(p, buf, i + 1);
lh_insert(conf, p);
}
lh_node_stats(conf, stdout);
lh_stats(conf, stdout);
lh_node_usage_stats(conf, stdout);
exit(0);
}