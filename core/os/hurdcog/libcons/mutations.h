#define FS_NOTIFY_INTRAN cons_notify_t begin_using_notify_port (fs_notify_t)
#define FS_NOTIFY_INTRAN_PAYLOAD cons_notify_t begin_using_notify_payload
#define FS_NOTIFY_DESTRUCTOR end_using_notify_port (cons_notify_t)
#define FS_NOTIFY_IMPORTS import "priv.h";