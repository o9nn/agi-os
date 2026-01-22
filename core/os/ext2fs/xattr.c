#include "ext2fs.h"
#include "xattr.h"
#include <stdlib.h>
#include <string.h>
#include <sys/xattr.h>
struct _xattr_prefix
{
int index;
char *prefix;
ssize_t size;
};
struct _xattr_prefix
xattr_prefixes[] =
{
{
1, "user.", sizeof "user." - 1},
{
10, "gnu.", sizeof "gnu." - 1},
{
0, NULL, 0}
};
static int
xattr_name_prefix (const char *full_name, int *index, const char **name)
{
int i;
for (i = 0; xattr_prefixes[i].prefix != NULL; i++)
{
if (!strncmp (xattr_prefixes[i].prefix, full_name,
xattr_prefixes[i].size))
{
*name = full_name + xattr_prefixes[i].size;
*index = xattr_prefixes[i].index;
break;
}
}
return i;
}
#define NAME_HASH_SHIFT 5
#define VALUE_HASH_SHIFT 16
static void
xattr_entry_hash (struct ext2_xattr_header *header,
struct ext2_xattr_entry *entry)
{
__u32 hash = 0;
char *name = entry->e_name;
int n;
for (n = 0; n < entry->e_name_len; n++)
{
hash = (hash << NAME_HASH_SHIFT)
^ (hash >> (8 * sizeof (hash) - NAME_HASH_SHIFT))
^ *name++;
}
if (entry->e_value_block == 0 && entry->e_value_size != 0)
{
__u32 *value = (__u32 *) ((char *) header + le16toh (entry->e_value_offs));
for (n = (le32toh (entry->e_value_size) + EXT2_XATTR_ROUND) >>
EXT2_XATTR_PAD_BITS; n; n--)
{
hash = (hash << VALUE_HASH_SHIFT)
^ (hash >> (8 * sizeof (hash) - VALUE_HASH_SHIFT))
^ le32toh(*value++);
}
}
entry->e_hash = htole32 (hash);
}
#undef NAME_HASH_SHIFT
#undef VALUE_HASH_SHIFT
#define BLOCK_HASH_SHIFT 16
static void
xattr_entry_rehash (struct ext2_xattr_header *header,
struct ext2_xattr_entry *entry)
{
__u32 hash = 0;
struct ext2_xattr_entry *position;
xattr_entry_hash (header, entry);
position = EXT2_XATTR_ENTRY_FIRST (header);
while (!EXT2_XATTR_ENTRY_LAST (position))
{
if (le32toh (position->e_hash) == 0)
{
hash = 0;
break;
}
hash = (hash << BLOCK_HASH_SHIFT)
^ (hash >> (8 * sizeof (hash) - BLOCK_HASH_SHIFT))
^ le32toh (position->e_hash);
position = EXT2_XATTR_ENTRY_NEXT (position);
}
header->h_hash = htole32 (hash);
}
#undef BLOCK_HASH_SHIFT
static error_t
xattr_entry_list (struct ext2_xattr_entry *entry, char *buffer, size_t *len)
{
int i;
size_t size;
for (i = 0; xattr_prefixes[i].prefix != NULL; i++)
{
if (entry->e_name_index == xattr_prefixes[i].index)
break;
}
if (xattr_prefixes[i].prefix == NULL)
return EOPNOTSUPP;
size = xattr_prefixes[i].size + entry->e_name_len + 1;
if (buffer)
{
if (size <= *len)
{
memcpy (buffer, xattr_prefixes[i].prefix, xattr_prefixes[i].size);
buffer += xattr_prefixes[i].size;
memcpy (buffer, entry->e_name, entry->e_name_len);
buffer += entry->e_name_len;
*buffer++ = 0;
}
else
{
return ERANGE;
}
}
*len -= size;
return 0;
}
static error_t
xattr_entry_get (void *block, struct ext2_xattr_entry *entry,
const char *full_name, char *value, size_t *len, int *cmp)
{
int i;
int index;
int tmp_cmp;
const char *name;
i = xattr_name_prefix (full_name, &index, &name);
if (xattr_prefixes[i].prefix == NULL)
return EOPNOTSUPP;
tmp_cmp = index - entry->e_name_index;
if (!tmp_cmp)
tmp_cmp = strlen (name) - entry->e_name_len;
if (!tmp_cmp)
tmp_cmp = strncmp (name, entry->e_name, entry->e_name_len);
if (tmp_cmp)
{
if (cmp)
*cmp = tmp_cmp;
return ENODATA;
}
if (value)
{
if (*len < le32toh (entry->e_value_size))
{
return ERANGE;
}
memcpy (value, block + le16toh (entry->e_value_offs), le32toh (entry->e_value_size));
}
*len = le32toh (entry->e_value_size);
return 0;
}
static error_t
xattr_entry_create (struct ext2_xattr_header *header,
struct ext2_xattr_entry *last,
struct ext2_xattr_entry *position,
const char *full_name, const char *value,
size_t len, size_t rest)
{
int i;
size_t name_len;
off_t start;
off_t end;
size_t entry_size;
size_t value_size;
int index;
const char *name;
i = xattr_name_prefix (full_name, &index, &name);
if (xattr_prefixes[i].prefix == NULL)
return EOPNOTSUPP;
name_len = strlen (name);
entry_size = EXT2_XATTR_ENTRY_SIZE (name_len);
value_size = EXT2_XATTR_ALIGN (len);
if (rest < 4 || entry_size + value_size > rest - 4)
{
return ERANGE;
}
start = EXT2_XATTR_ENTRY_OFFSET (header, position);
end = EXT2_XATTR_ENTRY_OFFSET (header, last);
memmove ((char *) position + entry_size, position, end - start);
position->e_name_len = name_len;
position->e_name_index = index;
position->e_value_offs = htole16 (end + rest - value_size);
position->e_value_block = 0;
position->e_value_size = htole32 (len);
strncpy (position->e_name, name, name_len);
memcpy ((char *) header + le16toh (position->e_value_offs), value, len);
memset ((char *) header + le16toh (position->e_value_offs) + len, 0,
value_size - len);
return 0;
}
static error_t
xattr_entry_remove (struct ext2_xattr_header *header,
struct ext2_xattr_entry *last,
struct ext2_xattr_entry *position, size_t rest)
{
size_t size;
off_t start;
off_t end;
struct ext2_xattr_entry *entry;
size = EXT2_XATTR_ALIGN (le32toh (position->e_value_size));
start = EXT2_XATTR_ENTRY_OFFSET (header, last) + rest;
end = le16toh (position->e_value_offs);
memmove ((char *) header + start + size, (char *) header + start,
end - start);
memset ((char *) header + start, 0, size);
entry = EXT2_XATTR_ENTRY_FIRST (header);
while (!EXT2_XATTR_ENTRY_LAST (entry))
{
if (le16toh (entry->e_value_offs) < end)
entry->e_value_offs = htole16 (le16toh (entry->e_value_offs) + size);
entry = EXT2_XATTR_ENTRY_NEXT (entry);
}
size = EXT2_XATTR_ENTRY_SIZE (position->e_name_len);
start = EXT2_XATTR_ENTRY_OFFSET (header, position);
end = EXT2_XATTR_ENTRY_OFFSET (header, last);
memmove ((char *) header + start , (char *) header + start + size,
end - (start + size));
memset ((char *) header + end - size, 0, size);
return 0;
}
static error_t
xattr_entry_replace (struct ext2_xattr_header *header,
struct ext2_xattr_entry *last,
struct ext2_xattr_entry *position,
const char *value, size_t len, size_t rest)
{
size_t old_size;
size_t new_size;
old_size = EXT2_XATTR_ALIGN (le32toh (position->e_value_size));
new_size = EXT2_XATTR_ALIGN (len);
if (rest < 4 || new_size - old_size > rest - 4)
return ERANGE;
if (new_size != old_size)
{
off_t start;
off_t end;
struct ext2_xattr_entry *entry;
start = EXT2_XATTR_ENTRY_OFFSET (header, last) + rest;
end = le16toh (position->e_value_offs);
memmove ((char *) header + start + old_size, (char *) header + start,
end - start);
entry = EXT2_XATTR_ENTRY_FIRST (header);
while (!EXT2_XATTR_ENTRY_LAST (entry))
{
if (le16toh (entry->e_value_offs) < end)
entry->e_value_offs = htole16 ( le16toh (entry->e_value_offs) + old_size);
entry = EXT2_XATTR_ENTRY_NEXT (entry);
}
position->e_value_offs = htole16 (start - (new_size - old_size));
}
position->e_value_size = htole32 (len);
memcpy ((char *) header + le16toh (position->e_value_offs), value, len);
memset ((char *) header + le16toh (position->e_value_offs) + len, 0, new_size - len);
return 0;
}
static int
xattr_header_valid(struct ext2_xattr_header *header)
{
return header->h_magic != htole32 (EXT2_XATTR_BLOCK_MAGIC)
|| header->h_blocks != htole32 (1);
}
error_t
ext2_free_xattr_block (struct node *np)
{
error_t err;
block_t blkno;
void *block;
struct ext2_inode *ei;
struct ext2_xattr_header *header;
if (!EXT2_HAS_COMPAT_FEATURE (sblock, EXT2_FEATURE_COMPAT_EXT_ATTR))
{
ext2_debug ("Filesystem has no support for extended attributes.");
return EOPNOTSUPP;
}
err = 0;
block = NULL;
ei = dino_ref (np->cache_id);
blkno = ei->i_file_acl;
if (blkno == 0)
{
err = 0;
goto cleanup;
}
assert_backtrace (!diskfs_readonly);
block = disk_cache_block_ref (blkno);
header = EXT2_XATTR_HEADER (block);
if (xattr_header_valid(header))
{
ext2_warning ("Invalid extended attribute block.");
err = EIO;
goto cleanup;
}
if (le32toh (header->h_refcount) == 1)
{
ext2_debug("free block %d", blkno);
disk_cache_block_deref (block);
ext2_free_blocks(blkno, 1);
np->dn_stat.st_blocks -= 1 << log2_stat_blocks_per_fs_block;
np->dn_stat.st_mode &= ~S_IPTRANS;
np->dn_set_ctime = 1;
}
else
{
ext2_debug("h_refcount: %d", le32toh (header->h_refcount));
header->h_refcount = htole32 (le32toh (header->h_refcount) - 1);
record_global_poke (block);
}
ei->i_file_acl = 0;
record_global_poke (ei);
return err;
cleanup:
if (block)
disk_cache_block_deref (block);
dino_deref (ei);
return err;
}
error_t
ext2_list_xattr (struct node *np, char *buffer, size_t *len)
{
error_t err;
block_t blkno;
void *block;
struct ext2_inode *ei;
struct ext2_xattr_header *header;
struct ext2_xattr_entry *entry;
if (!EXT2_HAS_COMPAT_FEATURE (sblock, EXT2_FEATURE_COMPAT_EXT_ATTR))
{
ext2_debug ("Filesystem has no support for extended attributes.");
return EOPNOTSUPP;
}
if (!len)
return EINVAL;
size_t size = *len;
ei = dino_ref (np->cache_id);
blkno = ei->i_file_acl;
dino_deref (ei);
if (blkno == 0)
{
*len = 0;
return 0;
}
err = EIO;
block = disk_cache_block_ref (blkno);
header = EXT2_XATTR_HEADER (block);
if (xattr_header_valid(header))
{
ext2_warning ("Invalid extended attribute block.");
err = EIO;
goto cleanup;
}
entry = EXT2_XATTR_ENTRY_FIRST (header);
while (!EXT2_XATTR_ENTRY_LAST (entry))
{
err = xattr_entry_list (entry, buffer, &size);
if (err)
goto cleanup;
if (buffer)
buffer += strlen (buffer) + 1;
entry = EXT2_XATTR_ENTRY_NEXT (entry);
}
*len = *len - size;
cleanup:
disk_cache_block_deref (block);
return err;
}
error_t
ext2_get_xattr (struct node *np, const char *name, char *value, size_t *len)
{
size_t size;
int err;
void *block;
struct ext2_inode *ei;
struct ext2_xattr_header *header;
struct ext2_xattr_entry *entry;
if (!EXT2_HAS_COMPAT_FEATURE (sblock, EXT2_FEATURE_COMPAT_EXT_ATTR))
{
ext2_debug ("Filesystem has no support for extended attributes.");
return EOPNOTSUPP;
}
if (!name || !len)
return EINVAL;
if (strlen(name) > 255)
return ERANGE;
ei = dino_ref (np->cache_id);
if (ei->i_file_acl == 0)
{
dino_deref (ei);
return ENODATA;
}
block = disk_cache_block_ref (ei->i_file_acl);
dino_deref (ei);
header = EXT2_XATTR_HEADER (block);
if (xattr_header_valid(header))
{
ext2_warning ("Invalid extended attribute block.");
err = EIO;
goto cleanup;
}
err = ENODATA;
entry = EXT2_XATTR_ENTRY_FIRST (header);
while (!EXT2_XATTR_ENTRY_LAST (entry))
{
size = *len;
err = xattr_entry_get (block, entry, name, value, &size, NULL);
if (err!= ENODATA)
break;
entry = EXT2_XATTR_ENTRY_NEXT (entry);
}
if (!err)
*len = size;
cleanup:
disk_cache_block_deref (block);
return err;
}
error_t
ext2_set_xattr (struct node *np, const char *name, const char *value,
size_t len, int flags)
{
int found;
size_t rest;
error_t err;
block_t blkno;
void *block = NULL;
struct ext2_inode *ei;
struct ext2_xattr_header *header;
struct ext2_xattr_entry *entry;
struct ext2_xattr_entry *location;
if (!EXT2_HAS_COMPAT_FEATURE (sblock, EXT2_FEATURE_COMPAT_EXT_ATTR))
{
ext2_warning ("Filesystem has no support for extended attributes.");
return EOPNOTSUPP;
}
if (!name)
return EINVAL;
if (strlen(name) > 255 || len > block_size)
return ERANGE;
ei = dino_ref (np->cache_id);
blkno = ei->i_file_acl;
if (blkno == 0 && value == NULL)
{
block = NULL;
err = ENODATA;
goto cleanup;
}
if (blkno == 0)
{
block_t goal;
assert_backtrace (!diskfs_readonly);
goal = le32toh (sblock->s_first_data_block) + np->dn->info.i_block_group *
EXT2_BLOCKS_PER_GROUP (sblock);
blkno = ext2_new_block (goal, 0, 0, 0);
if (blkno == 0)
{
err = ENOSPC;
goto cleanup;
}
block = disk_cache_block_ref (blkno);
memset (block, 0, block_size);
header = EXT2_XATTR_HEADER (block);
header->h_magic = htole32 (EXT2_XATTR_BLOCK_MAGIC);
header->h_blocks = htole32 (1);
header->h_refcount = htole32 (1);
}
else
{
block = disk_cache_block_ref (blkno);
header = EXT2_XATTR_HEADER (block);
if (xattr_header_valid(header))
{
ext2_warning ("Invalid extended attribute block.");
err = EIO;
goto cleanup;
}
}
entry = EXT2_XATTR_ENTRY_FIRST (header);
location = NULL;
rest = block_size;
err = ENODATA;
found = FALSE;
while (!EXT2_XATTR_ENTRY_LAST (entry))
{
size_t size;
int cmp;
err = xattr_entry_get (NULL, entry, name, NULL, &size, &cmp);
if (err == 0)
{
location = entry;
found = TRUE;
}
else if (err == ENODATA)
{
if (cmp < 0 && !found)
{
location = entry;
found = FALSE;
}
}
else
{
break;
}
rest -= EXT2_XATTR_ALIGN (le32toh (entry->e_value_size));
entry = EXT2_XATTR_ENTRY_NEXT (entry);
}
if (err != 0 && err != ENODATA)
{
goto cleanup;
}
if (location == NULL)
location = entry;
rest = rest - EXT2_XATTR_ENTRY_OFFSET (header, entry);
ext2_debug("space rest: %d", rest);
if (rest < 4)
{
err = ENOSPC;
goto cleanup;
}
if (value && flags & XATTR_CREATE)
{
if (found)
{
err = EEXIST;
goto cleanup;
}
else
err = xattr_entry_create (header, entry, location, name, value, len,
rest);
}
else if (value && flags & XATTR_REPLACE)
{
if (!found)
{
err = ENODATA;
goto cleanup;
}
else
err = xattr_entry_replace (header, entry, location, value, len, rest);
}
else if (value)
{
if (found)
err = xattr_entry_replace (header, entry, location, value, len, rest);
else
err = xattr_entry_create (header, entry, location, name, value, len,
rest);
}
else
{
if (flags & XATTR_REPLACE || flags & XATTR_CREATE)
{
err = EINVAL;
goto cleanup;
}
else if (!found)
{
err = ENODATA;
goto cleanup;
}
else
err = xattr_entry_remove (header, entry, location, rest);
}
entry = EXT2_XATTR_ENTRY_FIRST (header);
int empty = EXT2_XATTR_ENTRY_LAST (entry);
if (err == 0)
{
if (empty)
{
disk_cache_block_deref (block);
dino_deref (ei);
return ext2_free_xattr_block (np);
}
else
{
xattr_entry_rehash (header, location);
record_global_poke (block);
if (ei->i_file_acl == 0)
{
np->dn_stat.st_blocks += 1 << log2_stat_blocks_per_fs_block;
np->dn_set_ctime = 1;
ei->i_file_acl = blkno;
record_global_poke (ei);
}
else
dino_deref (ei);
return 0;
}
}
cleanup:
if (block)
disk_cache_block_deref (block);
if (ei->i_file_acl == 0 && blkno != 0)
ext2_free_blocks (blkno, 1);
dino_deref (ei);
return err;
}