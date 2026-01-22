#ifndef __FSTAB_H__
#define __FSTAB_H__
#include <mntent.h>
#include <hurd.h>
struct fs
{
struct fstab *fstab;
struct mntent mntent;
char *storage;
struct fstype *type;
int readonly, mounted;
fsys_t fsys;
struct fs *next, **self;
};
struct fstab
{
struct fs *entries;
struct fstypes *types;
};
struct fstype
{
char *name;
char *program;
struct fstype *next;
};
struct fstypes
{
struct fstype *entries;
char *program_search_fmts;
size_t program_search_fmts_len;
};
error_t fstab_create (struct fstypes *types, struct fstab **fstab);
void fstab_free (struct fstab *fstab);
error_t fstypes_create (const char *search_fmts, size_t search_fmts_len,
struct fstypes **types);
error_t fstypes_get (struct fstypes *types,
const char *name, struct fstype **fstype);
error_t fs_set_mntent (struct fs *fs, const struct mntent *mntent);
error_t fs_type (struct fs *fs, struct fstype **type);
error_t fs_fsys (struct fs *fs, fsys_t *fsys);
error_t fs_mounted (struct fs *fs, int *mounted);
error_t fs_readonly (struct fs *fs, int *readonly);
error_t fs_set_readonly (struct fs *fs, int readonly);
error_t fs_remount (struct fs *fs);
void fs_free (struct fs *fs);
struct fs *fstab_find_device (const struct fstab *fstab, const char *name);
struct fs *fstab_find_mount (const struct fstab *fstab, const char *name);
struct fs *fstab_find (const struct fstab *fstab, const char *name);
error_t fstab_add_mntent (struct fstab *fstab, const struct mntent *mntent,
struct fs **result);
error_t fstab_add_fs (struct fstab *dst, const struct fs *fs,
struct fs **copy);
error_t fstab_merge (struct fstab *dst, struct fstab *src);
error_t fstab_read (struct fstab *fstab, const char *name);
int fstab_next_pass (const struct fstab *fstab, int pass);
struct argp;
extern const struct argp fstab_argp;
struct fstab_argp_params
{
char *fstab_path;
char *program_search_fmts;
size_t program_search_fmts_len;
int do_all;
char *types;
size_t types_len;
char *exclude;
size_t exclude_len;
char *names;
size_t names_len;
};
struct fstab *fstab_argp_create (struct fstab_argp_params *params,
const char *default_search_fmts,
size_t default_search_fmts_len);
#endif