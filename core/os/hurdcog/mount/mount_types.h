#define MOUNT_READ 0x1
#define MOUNT_WRITE 0x2
#define MOUNT_FORCE 0x4
enum mount_state
{
MOUNT_STATE_UNKNOWN,
MOUNT_STATE_SUSPICIOUS,
MOUNT_STATE_DIRTY,
MOUNT_STATE_CLEAN
};
typedef enum mount_state mount_state_t;
enum mount_key_class
{
MOUNT_KEY_UNKNOWN,
MOUNT_KEY_FILE,
MOUNT_KEY_DEVICE
};
typedef enum mount_key_class mount_key_class_t;
enum mount_excl
{
MOUNT_EXCL_NONE,
MOUNT_EXCL_WRITE,
MOUNT_EXCL_RDWR
};
typedef enum mount_excl mount_excl_t;