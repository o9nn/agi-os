#include "priv.h"
char *diskfs_extra_version __attribute__ ((weak)) = "";
int diskfs_shortcut_symlink __attribute__ ((weak)) = 0;
int diskfs_shortcut_chrdev __attribute__ ((weak)) = 0;
int diskfs_shortcut_blkdev __attribute__ ((weak)) = 0;
int diskfs_shortcut_fifo __attribute__ ((weak)) = 0;
int diskfs_shortcut_ifsock __attribute__ ((weak)) = 0;
error_t (*diskfs_create_symlink_hook)(struct node *np, const char *target)
__attribute__ ((weak));
error_t (*diskfs_read_symlink_hook)(struct node *np, char *target)
__attribute__ ((weak));