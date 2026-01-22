enum backup_type
{
none,
simple,
numbered_existing,
numbered
};
extern enum backup_type backup_type;
extern char const *simple_backup_suffix;
#ifndef __BACKUPFILE_P
# if defined __STDC__ || __GNUC__
# define __BACKUPFILE_P(args) args
# else
# define __BACKUPFILE_P(args) ()
# endif
#endif
char *base_name __BACKUPFILE_P ((char const *));
char *find_backup_file_name __BACKUPFILE_P ((char const *));
enum backup_type get_version __BACKUPFILE_P ((char const *));
void addext __BACKUPFILE_P ((char *, char const *, int));