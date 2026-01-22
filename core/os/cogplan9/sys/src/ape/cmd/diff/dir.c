#include "diff.h"
struct dirdata
{
char const **names;
char *data;
};
static int compare_names PARAMS((void const *, void const *));
static int dir_sort PARAMS((struct file_data const *, struct dirdata *));
static int
dir_sort (dir, dirdata)
struct file_data const *dir;
struct dirdata *dirdata;
{
register struct dirent *next;
register int i;
char const **names;
size_t nnames;
char *data;
size_t data_alloc, data_used;
dirdata->names = 0;
dirdata->data = 0;
nnames = 0;
data = 0;
if (dir->desc != -1)
{
register DIR *reading = opendir (dir->name);
if (!reading)
return -1;
data_alloc = max (1, (size_t) dir->stat.st_size);
data_used = 0;
dirdata->data = data = xmalloc (data_alloc);
while ((errno = 0, (next = readdir (reading)) != 0))
{
char *d_name = next->d_name;
size_t d_size = NAMLEN (next) + 1;
if (d_name[0] == '.'
&& (d_name[1] == 0 || (d_name[1] == '.' && d_name[2] == 0)))
continue;
if (excluded_filename (d_name))
continue;
while (data_alloc < data_used + d_size)
dirdata->data = data = xrealloc (data, data_alloc *= 2);
memcpy (data + data_used, d_name, d_size);
data_used += d_size;
nnames++;
}
if (errno)
{
int e = errno;
closedir (reading);
errno = e;
return -1;
}
#if CLOSEDIR_VOID
closedir (reading);
#else
if (closedir (reading) != 0)
return -1;
#endif
}
dirdata->names = names = (char const **) xmalloc (sizeof (char *)
* (nnames + 1));
for (i = 0;  i < nnames;  i++)
{
names[i] = data;
data += strlen (data) + 1;
}
names[nnames] = 0;
qsort (names, nnames, sizeof (char *), compare_names);
return 0;
}
static int
compare_names (file1, file2)
void const *file1, *file2;
{
return filename_cmp (* (char const *const *) file1,
* (char const *const *) file2);
}
int
diff_dirs (filevec, handle_file, depth)
struct file_data const filevec[];
int (*handle_file) PARAMS((char const *, char const *, char const *, char const *, int));
int depth;
{
struct dirdata dirdata[2];
int val = 0;
int i;
for (i = 0; i < 2; i++)
if (dir_sort (&filevec[i], &dirdata[i]) != 0)
{
perror_with_name (filevec[i].name);
val = 2;
}
if (val == 0)
{
register char const * const *names0 = dirdata[0].names;
register char const * const *names1 = dirdata[1].names;
char const *name0 = filevec[0].name;
char const *name1 = filevec[1].name;
if (dir_start_file && depth == 0)
{
while (*names0 && filename_cmp (*names0, dir_start_file) < 0)
names0++;
while (*names1 && filename_cmp (*names1, dir_start_file) < 0)
names1++;
}
while (*names0 || *names1)
{
int nameorder = (!*names0 ? 1 : !*names1 ? -1
: filename_cmp (*names0, *names1));
int v1 = (*handle_file) (name0, 0 < nameorder ? 0 : *names0++,
name1, nameorder < 0 ? 0 : *names1++,
depth + 1);
if (v1 > val)
val = v1;
}
}
for (i = 0; i < 2; i++)
{
if (dirdata[i].names)
free (dirdata[i].names);
if (dirdata[i].data)
free (dirdata[i].data);
}
return val;
}