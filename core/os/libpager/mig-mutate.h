#define MEMORY_OBJECT_INTRAN pager_t begin_using_pager (memory_object_t)
#define MEMORY_OBJECT_INTRAN_PAYLOAD pager_t begin_using_pager_payload
#define MEMORY_OBJECT_DESTRUCTOR end_using_pager (pager_t)
#define MEMORY_OBJECT_IMPORTS import "mig-decls.h";