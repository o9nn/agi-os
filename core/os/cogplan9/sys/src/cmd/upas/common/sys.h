#include <u.h>
#include <libc.h>
#include <regexp.h>
#include <bio.h>
#include "String.h"
typedef struct Mlock	Mlock;
struct Mlock {
int fd;
int pid;
String *name;
};
extern char *MAILROOT;
extern char *UPASLOG;
extern char *UPASLIB;
extern char *UPASBIN;
extern char *UPASTMP;
extern char *SHELL;
extern char *POST;
extern int MBOXMODE;
extern char	*sysname_read(void);
extern char	*alt_sysname_read(void);
extern char	*domainname_read(void);
extern char	**sysnames_read(void);
extern char	*getlog(void);
extern char	*thedate(void);
extern Biobuf	*sysopen(char*, char*, ulong);
extern int	sysopentty(void);
extern int	sysclose(Biobuf*);
extern int	sysmkdir(char*, ulong);
extern int	syschgrp(char*, char*);
extern Mlock	*syslock(char *);
extern void	sysunlock(Mlock *);
extern void	syslockrefresh(Mlock *);
extern int	e_nonexistent(void);
extern int	e_locked(void);
extern long	sysfilelen(Biobuf*);
extern int	sysremove(char*);
extern int	sysrename(char*, char*);
extern int	sysexist(char*);
extern int	sysisdir(char*);
extern int	syskill(int);
extern int	syskillpg(int);
extern int	syscreate(char*, int, ulong);
extern Mlock	*trylock(char *);
extern void	exit(int);
extern void	pipesig(int*);
extern void	pipesigoff(void);
extern int	holdon(void);
extern void	holdoff(int);
extern int	syscreatelocked(char*, int, int);
extern int	sysopenlocked(char*, int);
extern int	sysunlockfile(int);
extern int	sysfiles(void);
extern int 	become(char**, char*);
extern int	sysdetach(void);
extern int	sysdirreadall(int, Dir**);
extern String	*username(String*);
extern char*	remoteaddr(int, char*);
extern int	creatembox(char*, char*);
extern String	*readlock(String*);
extern char	*homedir(char*);
extern String	*mboxname(char*, String*);
extern String	*deadletter(String*);
#define MAXPATHLEN 128