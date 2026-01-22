#include <hurd/ports.h>
#include "mount_types.h"
struct mount
{
struct port_info pi;
struct mount_fsys *fsys;
int mode;
fsys_t translator;
struct timespec timestamp;
char *mount_point;
struct mount *next;
};
struct mount_fsys
{
char *key;
enum mount_key_class key_class;
enum mount_state state;
enum mount_excl excl;
struct mount *mounts;
};