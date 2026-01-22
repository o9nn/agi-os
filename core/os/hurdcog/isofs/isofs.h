#include <sys/types.h>
#include <sys/mman.h>
#include <hurd/diskfs.h>
#include <hurd/diskfs-pager.h>
#include <hurd/store.h>
#include "rr.h"
struct disknode
{
struct dirrect *dr;
off_t file_start;
struct user_pager_info *fileinfo;
char *link_target;
size_t translen;
char *translator;
};
struct user_pager_info
{
struct node *np;
enum pager_type
{
DISK,
FILE_DATA,
} type;
struct pager *p;
};
struct lookup_context
{
struct dirrect *dr;
struct rrip_lookup rr;
};
extern struct store *store;
extern char *host_name;
extern char *mounted_on;
extern void *disk_image;
extern size_t disk_image_len;
extern size_t logical_block_size;
#define logical_sector_size	2048
extern struct sblock *sblock;
void drop_pager_softrefs (struct node *);
void allow_pager_softrefs (struct node *);
void create_disk_pager (void);
error_t cache_id (struct dirrect *record, struct rrip_lookup *rr, ino_t *idp);
error_t calculate_file_start (struct dirrect *, off_t *, struct rrip_lookup *);
char *isodate_915 (char *, struct timespec *);
char *isodate_84261 (char *, struct timespec *);