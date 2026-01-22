#include <malloc.h>
#include <string.h>
#include "idvec.h"
struct idvec *
make_idvec (void)
{
struct idvec *idvec = malloc (sizeof (struct idvec));
if (idvec)
{
idvec->alloced = idvec->num = 0;
idvec->ids = 0;
}
return idvec;
}
void
idvec_free_wrapper (struct idvec *idvec)
{
free (idvec);
}
void
idvec_free_contents (struct idvec *idvec)
{
if (idvec->alloced)
free (idvec->ids);
}
void
idvec_free (struct idvec *idvec)
{
idvec_free_contents (idvec);
idvec_free_wrapper (idvec);
}
error_t
idvec_ensure (struct idvec *idvec, unsigned num)
{
if (num > idvec->alloced)
{
uid_t *_ids = realloc (idvec->ids, num * sizeof (uid_t));
if (! _ids)
return ENOMEM;
idvec->ids = _ids;
idvec->alloced = num;
}
return 0;
}
error_t
idvec_grow (struct idvec *idvec, unsigned inc)
{
return idvec_ensure (idvec, idvec->num + inc);
}
int
idvec_tail_contains (const struct idvec *idvec, unsigned pos, uid_t id)
{
uid_t *ids = idvec->ids, *end = ids + idvec->num, *p = ids + pos;
while (p < end)
if (*p++ == id)
return 1;
return 0;
}
error_t
idvec_insert (struct idvec *idvec, unsigned pos, uid_t id)
{
error_t err = 0;
unsigned num = idvec->num;
unsigned new_num = (pos < num ? num + 1 : pos + 1);
if (idvec->alloced == num)
err = idvec_ensure (idvec, new_num + num);
else
err = idvec_ensure (idvec, new_num);
if (! err)
{
uid_t *ids = idvec->ids;
if (pos < num)
memmove (ids + pos + 1, ids + pos, (num - pos) * sizeof (uid_t));
else if (pos > num)
memset (ids + num, 0, (pos - num) * sizeof(uid_t));
ids[pos] = id;
idvec->num = new_num;
}
return err;
}
error_t
idvec_add (struct idvec *idvec, uid_t id)
{
return idvec_insert (idvec, idvec->num, id);
}
error_t
idvec_add_new (struct idvec *idvec, uid_t id)
{
if (idvec_contains (idvec, id))
return 0;
else
return idvec_add (idvec, id);
}
error_t
idvec_insert_new (struct idvec *idvec, unsigned pos, uid_t id)
{
if (idvec_tail_contains (idvec, pos, id))
return 0;
else
return idvec_insert (idvec, pos, id);
}
error_t
idvec_set_ids (struct idvec *idvec, const uid_t *ids, unsigned num)
{
error_t err;
err = idvec_ensure (idvec, num);
if (!err)
{
if (num)
memcpy (idvec->ids, ids, num * sizeof (uid_t));
idvec->num = num;
}
return err;
}
error_t
idvec_set (struct idvec *idvec, const struct idvec *new)
{
return idvec_set_ids (idvec, new->ids, new->num);
}
error_t
idvec_merge_ids (struct idvec *idvec, const uid_t *ids, unsigned num)
{
error_t err = 0;
unsigned num_old = idvec->num;
while (num-- > 0 && !err)
{
unsigned int i;
for (i = 0; i < num_old; i++)
if (idvec->ids[i] == *ids)
break;
if (i == num_old)
err = idvec_add (idvec, *ids);
ids++;
}
return err;
}
error_t
idvec_merge (struct idvec *idvec, const struct idvec *new)
{
return idvec_merge_ids (idvec, new->ids, new->num);
}
int
idvec_remove (struct idvec *idvec, unsigned pos, uid_t id)
{
if (pos < idvec->num)
{
int left = idvec->num - pos;
uid_t *ids = idvec->ids + pos, *targ = ids;
while (left--)
{
if (*ids != id)
{
if (ids != targ)
*targ = *ids;
targ++;
}
ids++;
}
if (ids == targ)
return 0;
idvec->num = targ - idvec->ids;
return 1;
}
else
return 0;
}
int
idvec_subtract (struct idvec *idvec, const struct idvec *sub)
{
unsigned int i;
int done = 0;
for (i = 0; i < sub->num; i++)
done |= idvec_remove (idvec, 0, sub->ids[i]);
return done;
}
int
idvec_keep (struct idvec *idvec, const struct idvec *keep)
{
uid_t *old = idvec->ids, *new = old, *end = old + idvec->num;
while (old < end)
{
uid_t id = *old++;
if (idvec_contains (keep, id))
{
if (old != new)
*new = id;
new++;
}
}
if (old != new)
{
idvec->num = new - idvec->ids;
return 1;
}
else
return 0;
}
void
idvec_delete (struct idvec *idvec, unsigned pos)
{
unsigned num = idvec->num;
if (pos < num)
{
uid_t *ids = idvec->ids;
idvec->num = --num;
if (num > pos)
memmove (ids + pos, ids + pos + 1, (num - pos) * sizeof (uid_t));
}
}
error_t
idvec_insert_only (struct idvec *idvec, unsigned pos, uid_t id)
{
if (idvec->num > pos && idvec->ids[pos] == id)
return 0;
else
{
idvec_remove (idvec, pos, id);
return idvec_insert (idvec, pos, id);
}
}
error_t
idvec_setid (struct idvec *eff, struct idvec *avail, uid_t id, int *secure)
{
error_t err;
int _secure = !idvec_contains (eff, id) && !idvec_contains (avail, id);
if (eff->num > 0)
{
err = idvec_add_new (avail, eff->ids[0]);
if (!err)
eff->ids[0] = id;
}
else
err = idvec_add (eff, id);
if (avail->num > 0 && !err)
err = idvec_insert_only (avail, 1, id);
if (err)
return err;
if (_secure && secure && !*secure)
*secure = 1;
return 0;
}