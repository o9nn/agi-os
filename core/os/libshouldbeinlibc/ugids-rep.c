#include <stdlib.h>
#include <string.h>
#include "ugids.h"
char *
ugids_rep (const struct ugids *ugids, int show_values, int show_names,
const char *id_sep, const char *type_sep, const char *hdr_sep)
{
size_t type_sep_len, hdr_sep_len;
int first = 1;
char *rep = 0;
size_t len = 0;
char *euid_rep = 0, *egid_rep = 0, *auid_rep = 0, *agid_rep = 0;
int type_rep (const char *name, const struct idvec *ids, int is_group,
char **rep)
{
if (ids->num > 0)
{
if (first)
first = 0;
else
len += type_sep_len;
len += strlen (name);
len += hdr_sep_len;
*rep =
(is_group ? idvec_gids_rep : idvec_uids_rep)
(ids, show_values, show_names, id_sep);
if (*rep)
len += strlen (*rep);
else
return 0;
}
return 1;
}
void add_type_rep (char **to, const char *name, const char *rep)
{
if (rep)
{
if (first)
first = 0;
else
*to = stpcpy (*to, type_sep);
*to = stpcpy (*to, name);
*to = stpcpy (*to, hdr_sep);
*to = stpcpy (*to, rep);
}
}
if (! type_sep)
type_sep = ", ";
if (! hdr_sep)
hdr_sep = ": ";
type_sep_len = strlen (type_sep);
hdr_sep_len = strlen (hdr_sep);
if (type_rep ("euids", &ugids->eff_uids, 0, &euid_rep)
&& type_rep ("egids", &ugids->eff_gids, 1, &egid_rep)
&& type_rep ("auids", &ugids->avail_uids, 0, &auid_rep)
&& type_rep ("agids", &ugids->avail_gids, 1, &agid_rep))
{
char *p = malloc (len + 1);
if (p)
{
rep = p;
first = 1;
add_type_rep (&p, "euids", euid_rep);
add_type_rep (&p, "egids", egid_rep);
add_type_rep (&p, "auids", auid_rep);
add_type_rep (&p, "agids", agid_rep);
}
}
if (euid_rep)
free (euid_rep);
if (egid_rep)
free (egid_rep);
if (auid_rep)
free (auid_rep);
if (agid_rep)
free (agid_rep);
return rep;
}