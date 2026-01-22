#ifndef __IDVEC_H__
#define __IDVEC_H__
#include <sys/types.h>
#include <hurd/hurd_types.h>
#include <string.h>
#include <features.h>
#ifdef IDVEC_DEFINE_EI
#define IDVEC_EI
#else
#define IDVEC_EI __extern_inline
#endif
struct idvec
{
uid_t *ids;
unsigned num, alloced;
};
#define IDVEC_INIT { 0 }
struct idvec *make_idvec (void);
void idvec_free_contents (struct idvec *idvec);
#define idvec_fini idvec_free_contents
void idvec_free_wrapper (struct idvec *idvec);
void idvec_free (struct idvec *idvec);
extern void idvec_clear (struct idvec *idvec);
extern int idvec_is_empty (const struct idvec *idvec);
extern int idvec_equal (const struct idvec *idvec1, const struct idvec *idvec2);
#if defined(__USE_EXTERN_INLINES) || defined(IDVEC_DEFINE_EI)
IDVEC_EI void
idvec_clear (struct idvec *idvec)
{
idvec->num = 0;
}
IDVEC_EI int
idvec_is_empty (const struct idvec *idvec)
{
return idvec->num == 0;
}
IDVEC_EI int
idvec_equal (const struct idvec *idvec1, const struct idvec *idvec2)
{
size_t num = idvec1->num;
return idvec2->num == num
&& (num == 0
|| memcmp (idvec1->ids, idvec2->ids, num * sizeof *idvec1->ids) == 0);
}
#endif
error_t idvec_ensure (struct idvec *idvec, unsigned num);
error_t idvec_grow (struct idvec *idvec, unsigned inc);
int idvec_tail_contains (const struct idvec *idvec, unsigned pos, uid_t id);
extern int idvec_contains (const struct idvec *idvec, uid_t id);
#if defined(__USE_EXTERN_INLINES) || defined(IDVEC_DEFINE_EI)
IDVEC_EI int
idvec_contains (const struct idvec *idvec, uid_t id)
{
return idvec_tail_contains (idvec, 0, id);
}
#endif
error_t idvec_insert (struct idvec *idvec, unsigned pos, uid_t id);
error_t idvec_add (struct idvec *idvec, uid_t id);
error_t idvec_add_new (struct idvec *idvec, uid_t id);
error_t idvec_insert_new (struct idvec *idvec, unsigned pos, uid_t id);
error_t idvec_set_ids (struct idvec *idvec, const uid_t *ids, unsigned num);
error_t idvec_set (struct idvec *idvec, const struct idvec *new);
error_t idvec_merge_ids (struct idvec *idvec, const uid_t *ids, unsigned num);
error_t idvec_merge (struct idvec *idvec, const struct idvec *new);
int idvec_subtract (struct idvec *idvec, const struct idvec *sub);
int idvec_keep (struct idvec *idvec, const struct idvec *keep);
int idvec_remove (struct idvec *idvec, unsigned pos, uid_t id);
void idvec_delete (struct idvec *idvec, unsigned pos);
error_t idvec_insert_only (struct idvec *idvec, unsigned pos, uid_t id);
error_t idvec_setid (struct idvec *eff, struct idvec *avail, uid_t id,
int *secure);
error_t idvec_merge_auth (struct idvec *eff_uids, struct idvec *avail_uids,
struct idvec *eff_gids, struct idvec *avail_gids,
auth_t auth);
error_t idvec_merge_implied_gids (struct idvec *gids, const struct idvec *uids);
error_t idvec_verify (const struct idvec *uids, const struct idvec *gids,
const struct idvec *have_uids,
const struct idvec *have_gids,
char *(*getpass_fn) (const char *prompt,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *getpass_hook,
error_t (*verify_fn) (const char *password,
uid_t id, int is_group,
void *pwd_or_grp, void *hook),
void *verify_hook);
char *idvec_rep (const struct idvec *idvec,
int show_values, int show_names,
char *(*id_name_fn) (uid_t id),
const char *sep);
char *idvec_uids_rep (const struct idvec *idvec,
int show_values, int show_names,
const char *sep);
char *idvec_gids_rep (const struct idvec *idvec,
int show_values, int show_names,
const char *sep);
#endif