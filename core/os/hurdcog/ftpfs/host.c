#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <netdb.h>
#include <ftpconn.h>
static error_t
split_server_name (const char *server, char **host, char **user, char **passwd)
{
size_t plim;
const char *p = server, *sep;
*host = 0;
*user = 0;
*passwd = 0;
sep = strrchr (p, '@');
if (sep)
{
*host = strdup (sep + 1);
if (! *host)
return ENOMEM;
plim = sep - server;
}
else
{
sep = strchr (server, ':');
if (sep)
{
*host = strndup (server, sep - server);
if (! *host)
return ENOMEM;
p = sep + 1;
plim = strlen (p);
}
else
{
*host = strdup (server);
if (! *host)
return ENOMEM;
return 0;
}
}
sep = memchr (p, ':', plim);
if (sep)
{
*user = strndup (p, sep - p);
*passwd = strndup (sep + 1, plim - (sep + 1 - p));
if (!*user || !*passwd)
{
if (*user)
free (*user);
if (*passwd)
free (*passwd);
free (*host);
return ENOMEM;
}
}
else
{
*user = strndup (p, plim);
if (! *user)
free (*user);
}
return 0;
}
error_t
lookup_server (const char *server, struct ftp_conn_params **params, int *h_err)
{
char hostent_data[2048];
struct hostent _he, *he;
char *host, *user, *passwd;
error_t err = split_server_name (server, &host, &user, &passwd);
if (err)
return err;
if (gethostbyname_r (host, &_he, hostent_data, sizeof hostent_data,
&he, h_err) == 0)
{
*params = malloc (sizeof (struct ftp_conn_params));
if (! *params)
err = ENOMEM;
else
{
(*params)->addr = malloc (he->h_length);
if (! (*params)->addr)
{
free (*params);
err = ENOMEM;
}
else
{
bcopy (he->h_addr_list[0], (*params)->addr, he->h_length);
(*params)->addr_len = he->h_length;
(*params)->addr_type = he->h_addrtype;
(*params)->user = user;
(*params)->pass = passwd;
(*params)->acct = 0;
}
}
}
else
err = EINVAL;
free (host);
if (err)
{
free (user);
free (passwd);
}
return err;
}