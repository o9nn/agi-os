#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_exec (trivfs_protid_t exec_file,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
mach_port_t exec_task,
int flags,
const_data_t argv,
mach_msg_type_number_t argvCnt,
const_data_t envp,
mach_msg_type_number_t envpCnt,
const_portarray_t fdarray,
mach_msg_type_number_t fdarrayCnt,
const_portarray_t portarray,
mach_msg_type_number_t portarrayCnt,
const_intarray_t intarray,
mach_msg_type_number_t intarrayCnt,
const_mach_port_array_t deallocnames,
mach_msg_type_number_t deallocnamesCnt,
const_mach_port_array_t destroynames,
mach_msg_type_number_t destroynamesCnt)
{
return EOPNOTSUPP;
}
kern_return_t
trivfs_S_file_exec_paths (trivfs_protid_t exec_file,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
mach_port_t exec_task,
int flags,
const_string_t path,
const_string_t abspath,
const_data_t argv,
mach_msg_type_number_t argvCnt,
const_data_t envp,
mach_msg_type_number_t envpCnt,
const_portarray_t fdarray,
mach_msg_type_number_t fdarrayCnt,
const_portarray_t portarray,
mach_msg_type_number_t portarrayCnt,
const_intarray_t intarray,
mach_msg_type_number_t intarrayCnt,
const_mach_port_array_t deallocnames,
mach_msg_type_number_t deallocnamesCnt,
const_mach_port_array_t destroynames,
mach_msg_type_number_t destroynamesCnt)
{
return EOPNOTSUPP;
}