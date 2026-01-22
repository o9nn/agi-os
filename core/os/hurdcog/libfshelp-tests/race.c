#include <stdio.h>
#include <error.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include "fs_U.h"
#include <hurd.h>
#include "io_U.h"
int main (int argc, char **argv)
{
error_t err;
struct flock64 lock;
mach_port_t rendezvous = MACH_PORT_NULL;
int fd;
int i;
mach_msg_type_number_t n_read;
vm_size_t v;
int blocked = 0;
char buf[10] = "";
char *bufp;
if (argc != 4)
error (1, 0, "Usage: %s file start len", argv[0]);
lock.l_whence = SEEK_SET;
lock.l_start = atoi (argv[2]);
lock.l_len = atoi (argv[3]);
fd = file_name_lookup (argv[1], O_READ | O_WRITE | O_CREAT, 0666);
if (fd == MACH_PORT_NULL)
error (1, errno, "file_name_lookup");
for (i = 0; i < 10000; i ++)
{
lock.l_type = F_WRLCK;
err = file_record_lock (fd, F_SETLK64, &lock, rendezvous, MACH_MSG_TYPE_MAKE_SEND);
if (err)
{
blocked ++;
err = file_record_lock (fd, F_SETLKW64, &lock, rendezvous, MACH_MSG_TYPE_MAKE_SEND);
}
if (err)
error (1, err, "file_record_lock");
v = n_read = sizeof (buf);
bufp = buf;
io_read (fd, &bufp, &n_read, 0, v);
v = atoi (bufp);
sprintf (buf, "%d\n", (int) (v + 1));
v = 10;
io_write (fd, buf, sizeof (buf), 0, &v);
if (v == 0)
error (1, errno, "write (%d)", i);
lock.l_type = F_UNLCK;
file_record_lock (fd, F_SETLK64, &lock, rendezvous, MACH_MSG_TYPE_MAKE_SEND);
}
mach_port_deallocate (mach_task_self (), fd);
printf ("Was blocked %d times\n", blocked);
return 0;
}