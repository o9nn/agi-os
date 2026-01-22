#define AUTH_INTRAN authhandle_t auth_port_to_handle (auth_t)
#define AUTH_INTRAN_PAYLOAD authhandle_t auth_payload_to_handle
#define AUTH_DESTRUCTOR end_using_authhandle (authhandle_t)
#define AUTH_IMPORTS import "mig-decls.h";