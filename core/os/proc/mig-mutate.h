#define PROCESS_INTRAN						\
pstruct_t begin_using_proc_port (process_t)
#define PROCESS_INTRAN_PAYLOAD					\
pstruct_t begin_using_proc_payload
#define PROCESS_DESTRUCTOR					\
end_using_proc (pstruct_t)
#define PROCESS_IMPORTS						\
import "mig-decls.h";
#define TASK_NOTIFY_INTRAN					\
port_info_t begin_using_port_info_port (mach_port_t)
#define TASK_NOTIFY_INTRAN_PAYLOAD				\
port_info_t begin_using_port_info_payload
#define TASK_NOTIFY_DESTRUCTOR					\
end_using_port_info (port_info_t)
#define TASK_NOTIFY_IMPORTS					\
import "libports/mig-decls.h";