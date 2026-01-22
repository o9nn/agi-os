#ifndef _HACK_FS_H
#define _HACK_FS_H
#include <linux/net.h>
struct inode
{
union
{
int i_garbage;
struct socket socket_i;
} u;
};
#define i_uid u.i_garbage
#define i_gid u.i_garbage
#define i_sock u.i_garbage
#define i_ino u.i_garbage
#define i_mode u.i_garbage
#endif