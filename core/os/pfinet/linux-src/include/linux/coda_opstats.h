#define CFS_MOUNT_STATS 0
#define CFS_UMOUNT_STATS 1
#define CFS_ROOT_STATS 2
#define CFS_STATFS_STATS 3
#define CFS_SYNC_STATS 4
#define CFS_VGET_STATS 5
#define CFS_VFSOPS_SIZE 6
#define CFS_OPEN_STATS 0
#define CFS_CLOSE_STATS 1
#define CFS_RDWR_STATS 2
#define CFS_IOCTL_STATS 3
#define CFS_SELECT_STATS 4
#define CFS_GETATTR_STATS 5
#define CFS_SETATTR_STATS 6
#define CFS_ACCESS_STATS 7
#define CFS_READLINK_STATS 8
#define CFS_FSYNC_STATS 9
#define CFS_INACTIVE_STATS 10
#define CFS_LOOKUP_STATS 11
#define CFS_CREATE_STATS 12
#define CFS_REMOVE_STATS 13
#define CFS_LINK_STATS 14
#define CFS_RENAME_STATS 15
#define CFS_MKDIR_STATS 16
#define CFS_RMDIR_STATS 17
#define CFS_SYMLINK_STATS 18
#define CFS_READDIR_STATS 19
#define CFS_VNODEOPS_SIZE 20
struct cfs_op_stats {
int opcode;
long entries;
long sat_intrn;
long unsat_intrn;
long gen_intrn;
};