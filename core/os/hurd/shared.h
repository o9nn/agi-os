#include <pthread.h>
#include <sys/types.h>
struct shared_io
{
int shared_page_magic;
pthread_spinlock_t lock;
enum
{
USER_HAS_CONCH,
USER_COULD_HAVE_CONCH,
USER_RELEASE_CONCH,
USER_HAS_NOT_CONCH,
} conch_status;
int append_mode;
int eof_notify;
int do_sigio;
int use_file_size;
int use_read_size;
loff_t read_size;
blksize_t optimal_transfer_size;
enum
{
RBR_NO_DATA,
RBR_BUFFER_FULL,
}
read_block_reason;
int seekable;
int use_prenotify_size;
int use_postnotify_size;
int use_readnotify_size;
loff_t prenotify_size;
loff_t postnotify_size;
loff_t readnotify_size;
loff_t rd_file_pointer;
loff_t wr_file_pointer;
loff_t xx_file_pointer;
loff_t file_size;
int written;
int accessed;
int indexes_changed;
int use_structure;
struct iomap_structure
{
int file_pointer_start;
int object_start;
int auxil_length;
int data_length;
} structure[0];
};
#define SHARED_PAGE_MAGIC 0xaabbccdd