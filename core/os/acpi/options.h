#ifndef OPTIONS_H
#define OPTIONS_H
#include <stdint.h>
#include <sys/types.h>
#include <argp.h>
#include <acpifs.h>
#define STR2(x)  #x
#define STR(x)  STR2(x)
struct parse_hook
{
struct acpifs_perm perm;
size_t ncache_len;
mach_port_t next_task;
mach_port_t host_priv_port;
mach_port_t dev_master_port;
};
static const struct argp_option options[] = {
{0, 0, 0, 0, "These apply to the whole acpi tree:", 1},
{"uid", 'U', "UID", 0, "User ID to give permissions to"},
{"gid", 'G', "GID", 0, "Group ID to give permissions to"},
{"next-task", 'N', "TASK", 0, "Next bootstrap task"},
{"host-priv-port", 'H', "PORT", 0, "Port for bootstrapping host"},
{"device-master-port", 'P', "PORT", 0, "Port for bootstrapping device master"},
{0}
};
static const char doc[] = "Permissions on acpi are currently global.";
#endif