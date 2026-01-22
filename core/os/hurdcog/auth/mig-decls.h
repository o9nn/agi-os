#ifndef __AUTH_MIG_DECLS_H__
#define __AUTH_MIG_DECLS_H__
#include "auth.h"
typedef struct authhandle *authhandle_t;
static inline struct authhandle * __attribute__ ((unused))
auth_port_to_handle (mach_port_t auth)
{
return ports_lookup_port (auth_bucket, auth, authhandle_portclass);
}
static inline struct authhandle * __attribute__ ((unused))
auth_payload_to_handle (uintptr_t payload)
{
return ports_lookup_payload (auth_bucket, payload, authhandle_portclass);
}
static inline void __attribute__ ((unused))
end_using_authhandle (struct authhandle *auth)
{
if (auth)
ports_port_deref (auth);
}
#endif