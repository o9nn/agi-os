#include	"common.h"
#include	"smtp.h"
#include	<ndb.h>
int
rmtdns(char *net, char *path)
{
int fd, n, nb, r;
char *domain, *cp, buf[Maxdomain + 5];
if(net == 0 || path == 0)
return 0;
domain = strdup(path);
cp = strchr(domain, '!');
if(cp){
*cp = 0;
n = cp-domain;
} else
n = strlen(domain);
if(*domain == '[' && domain[n-1] == ']'){
domain[n-1] = 0;
r = strcmp(ipattr(domain+1), "ip");
domain[n-1] = ']';
} else
r = strcmp(ipattr(domain), "ip");
if(r == 0){
free(domain);
return 0;
}
snprint(buf, sizeof buf, "%s/dns", net);
fd = open(buf, ORDWR);
if(fd < 0){
free(domain);
return 0;
}
n = snprint(buf, sizeof buf, "%s all", domain);
free(domain);
seek(fd, 0, 0);
nb = write(fd, buf, n);
close(fd);
if(nb != n){
rerrstr(buf, sizeof buf);
if (strcmp(buf, "dns: name does not exist") == 0)
return -1;
}
return 0;
}