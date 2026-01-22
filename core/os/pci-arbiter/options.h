#ifndef OPTIONS_H
#define OPTIONS_H
#include <stdint.h>
#include <sys/types.h>
#include <argp.h>
#include "pcifs.h"
#define STR2(x)  #x
#define STR(x)  STR2(x)
struct parse_hook
{
struct pcifs_perm *permsets;
size_t num_permsets;
struct pcifs_perm *curset;
size_t ncache_len;
mach_port_t next_task;
mach_port_t host_priv_port;
mach_port_t dev_master_port;
};
static const struct argp_option options[] = {
{0, 0, 0, 0, "Permission scope:", 1},
{"class", 'C', "CLASS", 0, "Device class in hexadecimal"},
{"subclass", 'c', "SUBCLASS", 0,
"Device subclass in hexadecimal, requires -C"},
{"domain", 'd', "DOMAIN", 0, "Device domain in hexadecimal"},
{"bus", 'b', "BUS", 0, "Device bus in hexadecimal"},
{"slot", 's', "SLOT", 0, "Device slot in hexadecimal, requires -b"},
{"func", 'f', "FUNC", 0, "Device func in hexadecimal, requires -s"},
{"device", 'D', "DEVICE", 0,
"Device address in format [<domain>:]<bus>:<slot>.<func>"},
{0, 0, 0, 0, "These apply to a given permission scope:", 2},
{"uid", 'U', "UID", 0, "User ID to give permissions to"},
{"gid", 'G', "GID", 0, "Group ID to give permissions to"},
{0, 0, 0, 0, "Global configuration options:", 3},
{"ncache", 'n', "LENGTH", 0,
"Node cache length. " STR (NODE_CACHE_MAX) " by default"},
{"next-task", 'N', "TASK", 0, "Next bootstrap task"},
{"host-priv-port", 'H', "PORT", 0, "Port for bootstrapping host"},
{"device-master-port", 'P', "PORT", 0, "Port for bootstrapping device master"},
{0}
};
static const char doc[] = "More than one permission scope may be specified. \
-G and -U options create a new permission scope if the current one already \
has a value for that option. If one device is covered by more than one \
permission scope, only the first permission is applied.";
#endif