#include <assert-backtrace.h>
#include <stdint.h>
#include "ftpfs.h"
struct ftpfs_conn
{
struct ftp_conn *conn;
struct ftpfs_conn *next;
};
static unsigned conn_id = 0;
error_t
ftpfs_get_ftp_conn (struct ftpfs *fs, struct ftp_conn **conn)
{
struct ftpfs_conn *fsc;
pthread_spin_lock (&fs->conn_lock);
fsc = fs->free_conns;
if (fsc)
fs->free_conns = fsc->next;
pthread_spin_unlock (&fs->conn_lock);
if (! fsc)
{
error_t err;
fsc = malloc (sizeof (struct ftpfs_conn));
if (! fsc)
return ENOMEM;
err = ftp_conn_create (fs->ftp_params, fs->ftp_hooks, &fsc->conn);
if (! err)
{
err = ftp_conn_set_type (fsc->conn, "I");
if (err)
ftp_conn_free (fsc->conn);
}
if (err)
{
free (fsc);
return err;
}
fsc->conn->hook = (void *)(uintptr_t)conn_id++;
}
pthread_spin_lock (&fs->conn_lock);
fsc->next = fs->conns;
fs->conns = fsc;
pthread_spin_unlock (&fs->conn_lock);
*conn = fsc->conn;
return 0;
}
void
ftpfs_release_ftp_conn (struct ftpfs *fs, struct ftp_conn *conn)
{
struct ftpfs_conn *fsc, *pfsc;
pthread_spin_lock (&fs->conn_lock);
for (pfsc = 0, fsc = fs->conns; fsc; pfsc = fsc, fsc = fsc->next)
if (fsc->conn == conn)
{
if (pfsc)
pfsc->next = fsc->next;
else
fs->conns = fsc->next;
fsc->next = fs->free_conns;
fs->free_conns = fsc;
break;
}
assert_backtrace (fsc);
pthread_spin_unlock (&fs->conn_lock);
}