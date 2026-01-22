#include <stdio.h>
#include <errno.h>
#include <error.h>
#include <hurd.h>
#include <fcntl.h>
#include <string.h>
#include "nfsd.h"
struct fsys_spec
{
fsys_t fsys;
char *name;
};
static struct fsys_spec *fsystable;
static int nfsys = 0;
static int fsystablesize = 0;
file_t index_file_dir;
char *index_file_compname;
void
init_filesystems (void)
{
int nitems;
char *name;
int index;
int line;
file_t root;
static FILE *index_file;
int i;
fsystable = (struct fsys_spec *) malloc ((fsystablesize = 10)
* sizeof (struct fsys_spec));
for (i = 0; i < fsystablesize; i++)
{
fsystable[i].fsys = MACH_PORT_NULL;
fsystable[i].name = 0;
}
if (!index_file_name)
return;
index_file = fopen (index_file_name, "r");
if (!index_file)
{
error (0, errno, "Cannot open `%s'", index_file_name);
return;
}
for (line = 1; ; line++)
{
nitems = fscanf (index_file, "%d %ms\n", &index, &name);
if (nitems == EOF)
{
fclose (index_file);
return;
}
if (nitems != 2)
{
error (0, 0, "%s:%d Bad syntax", index_file_name, line);
continue;
}
root = file_name_lookup (name, 0, 0);
if (root == MACH_PORT_NULL)
{
error (0, errno, "%s:%d Filesystem `%s'",
index_file_name, line, name);
free (name);
continue;
}
if (index >= fsystablesize)
{
fsystable = (struct fsys_spec *)
realloc (fsystable, index * 2 * sizeof (struct fsys_spec));
for (i = fsystablesize; i < index * 2; i++)
{
fsystable[i].fsys = MACH_PORT_NULL;
fsystable[i].name = 0;
}
fsystablesize = index * 2;
}
if (index + 1 > nfsys)
nfsys = index + 1;
fsystable[index].name = name;
file_getcontrol (root, &fsystable[index].fsys);
mach_port_deallocate (mach_task_self (), root);
}
}
static void
write_filesystems (void)
{
file_t newindex;
FILE *f;
error_t err;
int i;
if (!index_file_name)
return;
if (index_file_dir == MACH_PORT_NULL)
{
index_file_dir = file_name_split (index_file_name, &index_file_compname);
if (index_file_dir == MACH_PORT_NULL)
{
error (0, errno, "`%s'", index_file_name);
index_file_name = 0;
return;
}
}
err = dir_mkfile (index_file_dir, O_WRONLY, 0666, &newindex);
if (err)
{
error (0, err, "`%s'", index_file_name);
index_file_name = 0;
mach_port_deallocate (mach_task_self (), index_file_dir);
index_file_dir = MACH_PORT_NULL;
return;
}
f = fopenport (newindex, "w");
for (i = 0; i < nfsys; i++)
if (fsystable[i].name)
fprintf (f, "%d %s\n", i, fsystable[i].name);
err = dir_link (index_file_dir, newindex, index_file_compname, 0);
if (err)
error (0, err, "`%s'", index_file_name);
fflush (f);
file_sync (newindex, 1, 0);
fclose (f);
}
fsys_t
lookup_filesystem (int id)
{
if (id >= nfsys)
return MACH_PORT_NULL;
return fsystable[id].fsys;
}
int
enter_filesystem (char *name, file_t root)
{
int i;
for (i = 0; i < nfsys; i++)
if (fsystable[i].name && !strcmp (fsystable[i].name, name))
return i;
if (nfsys == fsystablesize)
{
fsystable = (struct fsys_spec *) realloc (fsystable,
(fsystablesize * 2)
* sizeof (struct fsys_spec));
for (i = fsystablesize; i < fsystablesize * 2; i++)
{
fsystable[i].fsys = MACH_PORT_NULL;
fsystable[i].name = 0;
}
fsystablesize *= 2;
}
fsystable[nfsys].name = malloc (strlen (name) + 1);
strcpy (fsystable[nfsys].name, name);
file_getcontrol (root, &fsystable[nfsys].fsys);
nfsys++;
write_filesystems ();
return nfsys - 1;
}