#include "namespace_sender.h"
int mailimap_namespace_send(mailstream * fd)
{
int r;
r = mailimap_token_send(fd, "NAMESPACE");
if (r != MAILIMAP_NO_ERROR)
return r;
return MAILIMAP_NO_ERROR;
}