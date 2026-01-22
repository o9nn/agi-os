#include <stdlib.h>
#include <assert-backtrace.h>
#include <string.h>
#include <pthread.h>
#include "virt-inode.h"
vi_key_t vi_zero_key = {0, 0};
struct v_inode
{
vi_key_t key;
};
#define LOG2_TABLE_PAGE_SIZE 10
#define TABLE_PAGE_SIZE (1 << LOG2_TABLE_PAGE_SIZE)
struct table_page
{
struct table_page *next;
struct v_inode vi[TABLE_PAGE_SIZE];
};
struct table_page *inode_table;
pthread_spinlock_t inode_table_lock = PTHREAD_SPINLOCK_INITIALIZER;
error_t
_vi_new(vi_key_t key, ino_t *inode, inode_t *v_inode)
{
struct table_page *table = inode_table;
struct table_page *prev_table = 0;
int page = 0;
int offset = 0;
while (table && memcmp(&vi_zero_key, &table->vi[offset].key, sizeof(vi_key_t)))
{
offset++;
if (offset == TABLE_PAGE_SIZE)
{
offset = 0;
page++;
prev_table = table;
table = table->next;
}
}
if (table)
{
table->vi[offset].key = key;
*inode = (page << LOG2_TABLE_PAGE_SIZE) + offset + 1;
*v_inode = &table->vi[offset];
}
else
{
struct table_page **pagep;
if (prev_table)
pagep = &prev_table->next;
else
pagep = &inode_table;
*pagep = (struct table_page *) malloc (sizeof (struct table_page));
if (!*pagep)
{
return ENOSPC;
}
memset (*pagep, 0, sizeof (struct table_page));
(*pagep)->vi[0].key = key;
*inode = (page << LOG2_TABLE_PAGE_SIZE) + 1;
*v_inode = &(*pagep)->vi[0];
}
return 0;
}
error_t
vi_new(vi_key_t key, ino_t *inode, inode_t *v_inode)
{
error_t err;
assert_backtrace (memcmp(&vi_zero_key, &key, sizeof (vi_key_t)));
pthread_spin_lock (&inode_table_lock);
err = _vi_new(key, inode, v_inode);
pthread_spin_unlock (&inode_table_lock);
return err;
}
vi_key_t
vi_key(inode_t v_inode)
{
return v_inode->key;
}
inode_t
vi_lookup(ino_t inode)
{
struct table_page *table = inode_table;
int page = (inode - 1) >> LOG2_TABLE_PAGE_SIZE;
int offset = (inode - 1) & (TABLE_PAGE_SIZE - 1);
inode_t v_inode = 0;
pthread_spin_lock (&inode_table_lock);
while (table && page > 0)
{
page--;
table = table->next;
}
if (table)
v_inode = &table->vi[offset];
pthread_spin_unlock (&inode_table_lock);
return v_inode;
}
error_t
vi_rlookup(vi_key_t key, ino_t *inode, inode_t *v_inode, int create)
{
error_t err = 0;
struct table_page *table = inode_table;
int page = 0;
int offset = 0;
assert_backtrace (memcmp(&vi_zero_key, &key, sizeof (vi_key_t)));
pthread_spin_lock (&inode_table_lock);
while (table && memcmp(&table->vi[offset].key, &key, sizeof (vi_key_t)))
{
offset++;
if (offset == TABLE_PAGE_SIZE)
{
offset = 0;
page++;
table = table->next;
}
}
if (table)
{
*inode = (page << LOG2_TABLE_PAGE_SIZE) + offset + 1;
*v_inode = &table->vi[offset];
}
else
{
if (create)
err = _vi_new (key, inode, v_inode);
else
err = EINVAL;
}
pthread_spin_unlock (&inode_table_lock);
return err;
}
vi_key_t vi_change(inode_t v_inode, vi_key_t key)
{
vi_key_t okey = v_inode->key;
assert_backtrace (memcmp(&vi_zero_key, &key, sizeof (vi_key_t)));
v_inode->key = key;
return okey;
}
vi_key_t vi_free(inode_t v_inode)
{
vi_key_t key;
pthread_spin_lock (&inode_table_lock);
key = v_inode->key;
v_inode->key = vi_zero_key;
pthread_spin_unlock (&inode_table_lock);
return key;
}