#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "store.h"
error_t
store_set_children (struct store *store,
struct store *const *children, size_t num_children)
{
unsigned size = num_children * sizeof (struct store *);
struct store **copy = malloc (size);
if (!copy)
return ENOMEM;
if (store->children)
free (store->children);
memcpy (copy, children, size);
store->children = copy;
store->num_children = num_children;
return 0;
}
error_t
store_allocate_child_encodings (const struct store *store,
struct store_enc *enc)
{
int i;
error_t err = 0;
for (i = 0; i < store->num_children && !err; i++)
{
struct store *k = store->children[i];
if (k->class->allocate_encoding)
(*k->class->allocate_encoding) (k, enc);
else
err = EOPNOTSUPP;
}
return err;
}
error_t
store_encode_children (const struct store *store, struct store_enc *enc)
{
int i;
error_t err = 0;
for (i = 0; i < store->num_children && !err; i++)
{
struct store *k = store->children[i];
if (k->class->encode)
(*k->class->encode) (k, enc);
else
err = EOPNOTSUPP;
}
return err;
}
error_t
store_decode_children (struct store_enc *enc, int num_children,
const struct store_class *const *classes,
struct store **children)
{
int i;
error_t err = 0;
for (i = 0; i < num_children && !err; i++)
err = store_decode (enc, classes, &children[i]);
if (err)
while (--i >= 0)
store_free (children[i]);
return err;
}
error_t
store_set_child_flags (struct store *store, int flags)
{
int i;
error_t err = 0;
int old_child_flags[store->num_children];
for (i = 0; i < store->num_children && !err; i++)
{
old_child_flags[i] = store->children[i]->flags;
err = store_set_flags (store->children[i], flags);
}
if (err)
while (i-- > 0)
store_clear_flags (store->children[i], flags & ~old_child_flags[i]);
else
store->flags |= flags;
return err;
}
error_t
store_clear_child_flags (struct store *store, int flags)
{
int i;
error_t err = 0;
int old_child_flags[store->num_children];
for (i = 0; i < store->num_children && !err; i++)
{
old_child_flags[i] = store->children[i]->flags;
err = store_clear_flags (store->children[i], flags);
}
if (err)
while (i-- > 0)
store_set_flags (store->children[i], flags & ~old_child_flags[i]);
else
store->flags &= ~flags;
return err;
}
error_t
store_open_children (const char *name, int flags,
const struct store_class *const *classes,
struct store ***stores, size_t *num_stores)
{
char *pfx = 0;
size_t pfx_len = 0;
char sep = *name;
if (sep && isalnum (sep))
{
const char *pfx_end = name;
while (isalnum (*pfx_end))
pfx_end++;
if (*pfx_end++ != ':')
return EINVAL;
pfx = strndupa (name, pfx_end - name);
pfx_len = pfx_end - name;
sep = *pfx_end;
}
if (sep)
{
int k;
const char *p, *end;
error_t err = 0;
size_t count = 0;
for (p = name; p && p[1]; p = strchr (p + 1, sep))
count++;
*stores = malloc (count * sizeof (struct store *));
*num_stores = count;
if (! *stores)
return ENOMEM;
memset (*stores, 0, count * sizeof(struct store *));
for (p = name, k = 0; !err && p && p[1]; p = end, k++)
{
size_t kname_len;
end = strchr (p + 1, sep);
kname_len = (end ? end - p - 1 : strlen (p + 1));
{
char kname[pfx_len + kname_len + 1];
if (pfx)
memcpy (kname, pfx, pfx_len);
memcpy (kname + pfx_len, p + 1, kname_len);
kname[pfx_len + kname_len] = '\0';
err = store_typed_open (kname, flags, classes, &(*stores)[k]);
}
}
if (err)
{
while (--k >= 0)
store_free ((*stores)[k]);
free (*stores);
}
return err;
}
else
{
*stores = 0;
*num_stores = 0;
return 0;
}
}
error_t
store_children_name (const struct store *store, char **name)
{
static char try_seps[] = "@+=,._%|;^!~'&";
struct store **kids = store->children;
size_t num_kids = store->num_children;
if (num_kids == 0)
{
*name = strdup ("");
return *name ? 0 : ENOMEM;
}
else
{
int k;
char *s;
int fail;
size_t total_len = 0;
for (k = 0; k < num_kids; k++)
if (!kids[k] || !kids[k]->name)
return EINVAL;
else
total_len +=
1 + strlen (kids[k]->class->name) + 1 + strlen (kids[k]->name);
for (s = try_seps, fail = 1; *s && fail; s++)
for (k = 0, fail = 0; k < num_kids && !fail; k++)
if (strchr (kids[k]->name, *s))
fail = 1;
if (*s)
{
char *p = malloc (total_len + 1);
if (! p)
return ENOMEM;
*name = p;
for (k = 0; k < num_kids; k++)
p +=
sprintf (p, "%c%s:%s", *s, kids[k]->class->name, kids[k]->name);
return 0;
}
else
return EGRATUITOUS;
}
}