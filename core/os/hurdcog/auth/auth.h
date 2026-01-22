#ifndef __AUTH_H__
#define __AUTH_H__
#include <hurd/ports.h>
extern struct port_bucket *auth_bucket;
extern struct port_class *authhandle_portclass;
#endif