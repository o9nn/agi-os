#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailimap_sender.h"
#include "quota_sender.h"
#include "quota_types.h"
int mailimap_quota_getquota_send(mailstream * fd,
const char * quotaroot)
{
int r;
r = mailimap_token_send(fd, "GETQUOTA");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_astring_send(fd, quotaroot);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_quota_getquotaroot_send(mailstream * fd,
const char * list_mb)
{
int r;
r = mailimap_token_send(fd, "GETQUOTAROOT");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, list_mb);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}