#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailimap_sender.h"
#include "acl_types.h"
static int mailimap_acl_identifier_send(mailstream * fd,
const char * identifier)
{
return mailimap_astring_send(fd, identifier);
}
static int mailimap_acl_mod_rights_send(mailstream * fd,
const char * mod_rights)
{
return mailimap_astring_send(fd, mod_rights);
}
int mailimap_acl_setacl_send(mailstream * fd,
const char * mailbox,
const char * identifier,
const char * mod_rights)
{
int r;
r = mailimap_token_send(fd, "SETACL");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mailbox);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_identifier_send(fd, identifier);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_mod_rights_send(fd, mod_rights);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_acl_deleteacl_send(mailstream * fd,
const char * mailbox,
const char * identifier)
{
int r;
r = mailimap_token_send(fd, "DELETEACL");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mailbox);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_identifier_send(fd, identifier);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_acl_getacl_send(mailstream * fd,
const char * mailbox)
{
int r;
r = mailimap_token_send(fd, "GETACL");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mailbox);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_acl_listrights_send(mailstream * fd,
const char * mailbox,
const char * identifier)
{
int r;
r = mailimap_token_send(fd, "LISTRIGHTS");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mailbox);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_acl_identifier_send(fd, identifier);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}
int mailimap_acl_myrights_send(mailstream * fd,
const char * mailbox)
{
int r;
r = mailimap_token_send(fd, "MYRIGHTS");
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_space_send(fd);
if (r != MAILIMAP_NO_ERROR)
return r;
r = mailimap_mailbox_send(fd, mailbox);
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}