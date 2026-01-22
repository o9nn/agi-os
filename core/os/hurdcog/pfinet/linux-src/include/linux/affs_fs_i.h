#ifndef _AFFS_FS_I
#define _AFFS_FS_I
#include <linux/a.out.h>
#include <linux/time.h>
#define AFFS_MAX_PREALLOC 16
#define AFFS_KCSIZE 73
struct key_cache {
struct timeval kc_lru_time;
s32 kc_first;
s32 kc_last;
s32 kc_this_key;
int kc_this_seq;
s32 kc_next_key;
s32 kc_keys[AFFS_KCSIZE];
};
#define EC_SIZE (PAGE_SIZE - 4 * sizeof(struct key_cache) - 4) / 4
struct ext_cache {
struct key_cache kc[4];
s32 ec[EC_SIZE];
int max_ext;
};
struct affs_inode_info {
u32 i_protect;
s32 i_parent;
s32 i_original;
s32 i_data[AFFS_MAX_PREALLOC];
struct ext_cache *i_ec;
int i_cache_users;
int i_lastblock;
short i_pa_cnt;
short i_pa_next;
short i_pa_last;
short i_zone;
unsigned char i_hlink;
unsigned char i_pad;
};
#endif