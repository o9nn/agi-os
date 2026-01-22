#ifndef _CODA_PROC_H
#define _CODA_PROC_H
void coda_sysctl_init(void);
void coda_sysctl_clean(void);
void coda_upcall_stats(int opcode, unsigned long jiffies);
#include <linux/sysctl.h>
#include <linux/coda_fs_i.h>
#include <linux/coda.h>
struct coda_vfs_stats
{
int file_read;
int file_write;
int file_mmap;
int open;
int release;
int fsync;
int readdir;
int create;
int lookup;
int link;
int unlink;
int symlink;
int mkdir;
int rmdir;
int rename;
int permission;
int readpage;
int follow_link;
int readlink;
};
struct coda_upcall_stats_entry
{
int count;
unsigned long time_sum;
unsigned long time_squared_sum;
};
struct coda_permission_stats
{
int count;
int hit_count;
};
struct coda_cache_inv_stats
{
int flush;
int purge_user;
int zap_dir;
int zap_file;
int zap_vnode;
int purge_fid;
int replace;
};
extern struct coda_vfs_stats coda_vfs_stat;
extern struct coda_permission_stats coda_permission_stat;
extern struct coda_cache_inv_stats coda_cache_inv_stat;
extern int coda_upcall_timestamping;
void reset_coda_vfs_stats( void );
void reset_coda_upcall_stats( void );
void reset_coda_permission_stats( void );
void reset_coda_cache_inv_stats( void );
void do_time_stats( struct coda_upcall_stats_entry * pentry,
unsigned long jiffy );
unsigned long get_time_average( const struct coda_upcall_stats_entry * pentry );
unsigned long get_time_std_deviation( const struct coda_upcall_stats_entry * pentry );
int do_reset_coda_vfs_stats( ctl_table * table, int write, struct file * filp,
void * buffer, size_t * lenp );
int do_reset_coda_upcall_stats( ctl_table * table, int write,
struct file * filp, void * buffer,
size_t * lenp );
int do_reset_coda_permission_stats( ctl_table * table, int write,
struct file * filp, void * buffer,
size_t * lenp );
int do_reset_coda_cache_inv_stats( ctl_table * table, int write,
struct file * filp, void * buffer,
size_t * lenp );
int coda_vfs_stats_get_info( char * buffer, char ** start, off_t offset,
int length, int dummy );
int coda_upcall_stats_get_info( char * buffer, char ** start, off_t offset,
int length, int dummy );
int coda_permission_stats_get_info( char * buffer, char ** start, off_t offset,
int length, int dummy );
int coda_cache_inv_stats_get_info( char * buffer, char ** start, off_t offset,
int length, int dummy );
#endif