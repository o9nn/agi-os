#include "priv.h"
error_t (*trivfs_check_access_hook) (struct trivfs_control *cntl,
struct iouser *user,
mach_port_t realnode,
int *allowed)
__attribute__ ((weak));
error_t (*trivfs_check_open_hook) (struct trivfs_control *cntl,
struct iouser *user, int flags)
__attribute__ ((weak));
error_t (*trivfs_open_hook) (struct trivfs_control *fsys,
struct iouser *user,
mach_port_t dotdot,
int flags,
mach_port_t realnode,
struct trivfs_protid **cred)
__attribute__ ((weak));
error_t (*trivfs_protid_create_hook) (struct trivfs_protid *)
__attribute__ ((weak));
error_t (*trivfs_peropen_create_hook) (struct trivfs_peropen *)
__attribute__ ((weak));
void (*trivfs_protid_destroy_hook) (struct trivfs_protid *)
__attribute__ ((weak));
void (*trivfs_peropen_destroy_hook) (struct trivfs_peropen *)
__attribute__ ((weak));
error_t (*trivfs_getroot_hook) (struct trivfs_control *cntl,
mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
mach_port_t dotdot,
const uid_t *uids, mach_msg_type_number_t nuids, const uid_t *gids, mach_msg_type_number_t ngids,
int flags,
retry_type *do_retry, char *retry_name,
mach_port_t *node, mach_msg_type_name_t *node_type)
__attribute__ ((weak));