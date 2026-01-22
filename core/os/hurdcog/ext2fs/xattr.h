#ifndef EXT2_XATTR_H
#define EXT2_XATTR_H
#include "ext2fs.h"
#define EXT2_XATTR_BLOCK_MAGIC 0xEA020000
struct ext2_xattr_header
{
__u32 h_magic;
__u32 h_refcount;
__u32 h_blocks;
__u32 h_hash;
__u32 h_reserved[4];
};
struct ext2_xattr_entry
{
__u8 e_name_len;
__u8 e_name_index;
__u16 e_value_offs;
__u32 e_value_block;
__u32 e_value_size;
__u32 e_hash;
char e_name[0];
};
#define EXT2_XATTR_PAD_BITS 2
#define EXT2_XATTR_PAD (1 << EXT2_XATTR_PAD_BITS)
#define EXT2_XATTR_ROUND (EXT2_XATTR_PAD - 1)
#define EXT2_XATTR_ALIGN(x) (((unsigned long) (x) + \
EXT2_XATTR_ROUND) & \
(~EXT2_XATTR_ROUND))
#define EXT2_XATTR_HEADER(block) ((struct ext2_xattr_header *) block)
#define EXT2_XATTR_ENTRY_SIZE(len) EXT2_XATTR_ALIGN ((sizeof \
(struct ext2_xattr_entry) + \
len))
#define EXT2_XATTR_ENTRY_OFFSET(header, entry) ((off_t) ((char *) entry - \
(char *) header))
#define EXT2_XATTR_ENTRY_FIRST(header) ((struct ext2_xattr_entry *) (header + 1))
#define EXT2_XATTR_ENTRY_NEXT(entry) ((struct ext2_xattr_entry *) \
((char *) entry + \
EXT2_XATTR_ENTRY_SIZE \
(entry->e_name_len)))
#define EXT2_XATTR_ENTRY_LAST(entry) (*(uint32_t *) entry == 0UL)
#endif