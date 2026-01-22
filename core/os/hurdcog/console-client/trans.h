#include <hurd/netfs.h>
struct consnode
{
char *name;
int id;
struct node *node;
error_t (*read) (struct protid *user, char **data,
mach_msg_type_number_t *datalen, off_t offset,
mach_msg_type_number_t amount);
error_t (*write) (struct protid *user, const char *data,
mach_msg_type_number_t datalen, off_t offset,
vm_size_t *amount);
error_t (*select) (struct protid *user, mach_port_t reply,
mach_msg_type_name_t replytype,
struct timespec *tsp, int *type);
void (*open) (void);
void (*close) (void);
int (*demuxer) (mach_msg_header_t *inp, mach_msg_header_t *outp);
error_t (*readlink) (struct iouser *user, struct node *np, char *buf);
error_t (*mksymlink) (struct iouser *cred, struct node *np, const char *name);
struct consnode *next;
};
typedef struct consnode *consnode_t;
void console_register_consnode (consnode_t cn);
void console_unregister_consnode (consnode_t cn);
error_t console_create_consnode (const char *name, consnode_t *cn);
void console_destroy_consnode (consnode_t cn);
error_t console_setup_node (char *path);