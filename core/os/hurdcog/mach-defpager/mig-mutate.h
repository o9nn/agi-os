#define MEMORY_OBJECT_INTRAN default_pager_t begin_using_default_pager (mach_port_t)
#define MEMORY_OBJECT_INTRAN_PAYLOAD \
default_pager_t begin_using_default_pager_payload
#define MEMORY_OBJECT_IMPORTS import "mig-decls.h";
#define DEFAULT_PAGER_IMPORTS import "mig-decls.h";