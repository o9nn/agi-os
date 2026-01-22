#ifndef _HURD_IOHELP_
#define _HURD_IOHELP_
#include <mach.h>
#include <hurd/hurd_types.h>
#include <pthread.h>
#include <hurd/shared.h>
struct conch
{
pthread_mutex_t *lock;
pthread_cond_t wait;
void *holder;
struct shared_io *holder_shared_page;
};
void iohelp_initialize_conch (struct conch *, pthread_mutex_t *);
void iohelp_handle_io_get_conch (struct conch *, void *,
struct shared_io *);
void iohelp_get_conch (struct conch *);
void iohelp_handle_io_release_conch (struct conch *, void *);
error_t iohelp_verify_user_conch (struct conch *, void *);
void iohelp_fetch_shared_data (void *);
void iohelp_put_shared_data (void *);
#include <idvec.h>
struct iouser
{
struct idvec *uids, *gids;
void *hook;
};
error_t iohelp_dup_iouser (struct iouser **clone, struct iouser *iouser);
void iohelp_free_iouser (struct iouser *iouser);
error_t iohelp_create_iouser (struct iouser **user, struct idvec *uids,
struct idvec *gids);
error_t iohelp_create_complex_iouser (struct iouser **user,
const uid_t *uids, int nuids,
const gid_t *gids, int ngids);
error_t iohelp_create_simple_iouser (struct iouser **user,
uid_t uid, gid_t gid);
error_t iohelp_create_empty_iouser (struct iouser **user);
error_t iohelp_restrict_iouser (struct iouser **new_user,
const struct iouser *old_user,
const uid_t *uids, int nuids,
const gid_t *gids, int ngids);
error_t iohelp_reauth (struct iouser **user, auth_t authserver,
mach_port_t rend_port, mach_port_t newright,
int permit_failure);
error_t iohelp_return_malloced_buffer (char *buf, size_t len,
char **rbuf,
mach_msg_type_number_t *rlen);
#endif