#ifndef __USERMUX_H__
#define __USERMUX_H__
#include <hurd/netfs.h>
#include <pthread.h>
#include <maptime.h>
struct passwd;
#define USERMUX_FILENO_UID_OFFSET	10
extern volatile struct mapped_time_value *usermux_maptime;
struct usermux
{
struct usermux_name *names;
pthread_rwlock_t names_lock;
char *trans_template;
size_t trans_template_len;
char *user_pat;
char *home_pat;
char *uid_pat;
struct stat stat_template;
file_t underlying;
};
struct usermux_name
{
const char *name;
struct node *node;
struct usermux_name *next;
};
struct netnode
{
struct usermux *mux;
struct usermux_name *name;
char *trans;
size_t trans_len;
};
error_t create_user_node (struct usermux *mux, struct usermux_name *name,
struct passwd *pw, struct node **node);
#ifndef USERMUX_EI
# define USERMUX_EI __extern_inline
#endif
#endif