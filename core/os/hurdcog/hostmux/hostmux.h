#ifndef __HOSTMUX_H__
#define __HOSTMUX_H__
#include <hurd/netfs.h>
#include <pthread.h>
#include <maptime.h>
#include <features.h>
#ifdef HOSTMUX_DEFINE_EI
#define HOSTMUX_EI
#else
#define HOSTMUX_EI __extern_inline
#endif
extern volatile struct mapped_time_value *hostmux_maptime;
struct hostmux
{
struct hostmux_name *names;
pthread_rwlock_t names_lock;
ino_t next_fileno;
char *trans_template;
size_t trans_template_len;
char *host_pat;
boolean_t canonicalize;
struct stat stat_template;
file_t underlying;
};
struct hostmux_name
{
const char *name;
const char *canon;
struct node *node;
ino_t fileno;
struct hostmux_name *next;
};
struct netnode
{
struct hostmux *mux;
struct hostmux_name *name;
};
#endif