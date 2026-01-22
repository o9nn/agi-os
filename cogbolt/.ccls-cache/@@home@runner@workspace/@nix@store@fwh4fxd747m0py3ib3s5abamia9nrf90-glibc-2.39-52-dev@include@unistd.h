#ifndef	_UNISTD_H
#define	_UNISTD_H	1
#include <features.h>
__BEGIN_DECLS
#ifdef __USE_XOPEN2K8
# define _POSIX_VERSION	200809L
#elif defined __USE_XOPEN2K
# define _POSIX_VERSION	200112L
#elif defined __USE_POSIX199506
# define _POSIX_VERSION	199506L
#elif defined __USE_POSIX199309
# define _POSIX_VERSION	199309L
#else
# define _POSIX_VERSION	199009L
#endif
#ifdef __USE_XOPEN2K8
# define __POSIX2_THIS_VERSION	200809L
#elif defined __USE_XOPEN2K
# define __POSIX2_THIS_VERSION	200112L
#elif defined __USE_POSIX199506
# define __POSIX2_THIS_VERSION	199506L
#else
# define __POSIX2_THIS_VERSION	199209L
#endif
#define _POSIX2_VERSION	__POSIX2_THIS_VERSION
#define	_POSIX2_C_VERSION	__POSIX2_THIS_VERSION
#define	_POSIX2_C_BIND	__POSIX2_THIS_VERSION
#define	_POSIX2_C_DEV	__POSIX2_THIS_VERSION
#define	_POSIX2_SW_DEV	__POSIX2_THIS_VERSION
#define _POSIX2_LOCALEDEF       __POSIX2_THIS_VERSION
#ifdef __USE_XOPEN2K8
# define _XOPEN_VERSION	700
#elif defined __USE_XOPEN2K
# define _XOPEN_VERSION	600
#elif defined __USE_UNIX98
# define _XOPEN_VERSION	500
#else
# define _XOPEN_VERSION	4
#endif
#define _XOPEN_XCU_VERSION	4
#define _XOPEN_XPG2	1
#define _XOPEN_XPG3	1
#define _XOPEN_XPG4	1
#define _XOPEN_UNIX	1
#define	_XOPEN_ENH_I18N	1
#define _XOPEN_LEGACY	1
#include <bits/posix_opt.h>
#if defined __USE_UNIX98 || defined __USE_XOPEN2K
# include <bits/environments.h>
#endif
#define	STDIN_FILENO	0
#define	STDOUT_FILENO	1
#define	STDERR_FILENO	2
#include <bits/types.h>
#ifndef	__ssize_t_defined
typedef __ssize_t ssize_t;
# define __ssize_t_defined
#endif
#define	__need_size_t
#define __need_NULL
#include <stddef.h>
#if defined __USE_XOPEN || defined __USE_XOPEN2K
# ifndef __gid_t_defined
typedef __gid_t gid_t;
#  define __gid_t_defined
# endif
# ifndef __uid_t_defined
typedef __uid_t uid_t;
#  define __uid_t_defined
# endif
# ifndef __off_t_defined
#  ifndef __USE_FILE_OFFSET64
typedef __off_t off_t;
#  else
typedef __off64_t off_t;
#  endif
#  define __off_t_defined
# endif
# if defined __USE_LARGEFILE64 && !defined __off64_t_defined
typedef __off64_t off64_t;
#  define __off64_t_defined
# endif
# ifndef __useconds_t_defined
typedef __useconds_t useconds_t;
#  define __useconds_t_defined
# endif
# ifndef __pid_t_defined
typedef __pid_t pid_t;
#  define __pid_t_defined
# endif
#endif
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K
# ifndef __intptr_t_defined
typedef __intptr_t intptr_t;
#  define __intptr_t_defined
# endif
#endif
#if defined __USE_MISC || defined __USE_XOPEN
# ifndef __socklen_t_defined
typedef __socklen_t socklen_t;
#  define __socklen_t_defined
# endif
#endif
#define	R_OK	4
#define	W_OK	2
#define	X_OK	1
#define	F_OK	0
extern int access (const char *__name, int __type) __THROW __nonnull ((1));
#ifdef __USE_GNU
extern int euidaccess (const char *__name, int __type)
__THROW __nonnull ((1));
extern int eaccess (const char *__name, int __type)
__THROW __nonnull ((1));
extern int execveat (int __fd, const char *__path, char *const __argv[],
char *const __envp[], int __flags)
__THROW __nonnull ((2, 3));
#endif
#ifdef __USE_ATFILE
extern int faccessat (int __fd, const char *__file, int __type, int __flag)
__THROW __nonnull ((2)) __wur;
#endif
#ifndef	_STDIO_H
# define SEEK_SET	0
# define SEEK_CUR	1
# define SEEK_END	2
# ifdef __USE_GNU
#  define SEEK_DATA	3
#  define SEEK_HOLE	4
# endif
#endif
#if defined __USE_MISC && !defined L_SET
# define L_SET		SEEK_SET
# define L_INCR		SEEK_CUR
# define L_XTND		SEEK_END
#endif
#ifndef __USE_FILE_OFFSET64
extern __off_t lseek (int __fd, __off_t __offset, int __whence) __THROW;
#else
# ifdef __REDIRECT_NTH
extern __off64_t __REDIRECT_NTH (lseek,
(int __fd, __off64_t __offset, int __whence),
lseek64);
# else
#  define lseek lseek64
# endif
#endif
#ifdef __USE_LARGEFILE64
extern __off64_t lseek64 (int __fd, __off64_t __offset, int __whence)
__THROW;
#endif
extern int close (int __fd);
#ifdef __USE_MISC
extern void closefrom (int __lowfd) __THROW;
#endif
extern ssize_t read (int __fd, void *__buf, size_t __nbytes) __wur
__fortified_attr_access (__write_only__, 2, 3);
extern ssize_t write (int __fd, const void *__buf, size_t __n) __wur
__attr_access ((__read_only__, 2, 3));
#if defined __USE_UNIX98 || defined __USE_XOPEN2K8
# ifndef __USE_FILE_OFFSET64
extern ssize_t pread (int __fd, void *__buf, size_t __nbytes,
__off_t __offset) __wur
__fortified_attr_access (__write_only__, 2, 3);
extern ssize_t pwrite (int __fd, const void *__buf, size_t __n,
__off_t __offset) __wur
__attr_access ((__read_only__, 2, 3));
# else
#  ifdef __REDIRECT
extern ssize_t __REDIRECT (pread, (int __fd, void *__buf, size_t __nbytes,
__off64_t __offset),
pread64) __wur
__fortified_attr_access (__write_only__, 2, 3);
extern ssize_t __REDIRECT (pwrite, (int __fd, const void *__buf,
size_t __nbytes, __off64_t __offset),
pwrite64) __wur
__attr_access ((__read_only__, 2, 3));
#  else
#   define pread pread64
#   define pwrite pwrite64
#  endif
# endif
# ifdef __USE_LARGEFILE64
extern ssize_t pread64 (int __fd, void *__buf, size_t __nbytes,
__off64_t __offset) __wur
__fortified_attr_access (__write_only__, 2, 3);
extern ssize_t pwrite64 (int __fd, const void *__buf, size_t __n,
__off64_t __offset) __wur
__attr_access ((__read_only__, 2, 3));
# endif
#endif
extern int pipe (int __pipedes[2]) __THROW __wur;
#ifdef __USE_GNU
extern int pipe2 (int __pipedes[2], int __flags) __THROW __wur;
#endif
extern unsigned int alarm (unsigned int __seconds) __THROW;
extern unsigned int sleep (unsigned int __seconds);
#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) \
|| defined __USE_MISC
extern __useconds_t ualarm (__useconds_t __value, __useconds_t __interval)
__THROW;
extern int usleep (__useconds_t __useconds);
#endif
extern int pause (void);
extern int chown (const char *__file, __uid_t __owner, __gid_t __group)
__THROW __nonnull ((1)) __wur;
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
extern int fchown (int __fd, __uid_t __owner, __gid_t __group) __THROW __wur;
extern int lchown (const char *__file, __uid_t __owner, __gid_t __group)
__THROW __nonnull ((1)) __wur;
#endif
#ifdef __USE_ATFILE
extern int fchownat (int __fd, const char *__file, __uid_t __owner,
__gid_t __group, int __flag)
__THROW __nonnull ((2)) __wur;
#endif
extern int chdir (const char *__path) __THROW __nonnull ((1)) __wur;
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
extern int fchdir (int __fd) __THROW __wur;
#endif
extern char *getcwd (char *__buf, size_t __size) __THROW __wur;
#ifdef	__USE_GNU
extern char *get_current_dir_name (void) __THROW;
#endif
#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) \
|| defined __USE_MISC
extern char *getwd (char *__buf)
__THROW __nonnull ((1)) __attribute_deprecated__ __wur
__attr_access ((__write_only__, 1));
#endif
extern int dup (int __fd) __THROW __wur;
extern int dup2 (int __fd, int __fd2) __THROW;
#ifdef __USE_GNU
extern int dup3 (int __fd, int __fd2, int __flags) __THROW;
#endif
extern char **__environ;
#ifdef __USE_GNU
extern char **environ;
#endif
extern int execve (const char *__path, char *const __argv[],
char *const __envp[]) __THROW __nonnull ((1, 2));
#ifdef __USE_XOPEN2K8
extern int fexecve (int __fd, char *const __argv[], char *const __envp[])
__THROW __nonnull ((2));
#endif
extern int execv (const char *__path, char *const __argv[])
__THROW __nonnull ((1, 2));
extern int execle (const char *__path, const char *__arg, ...)
__THROW __nonnull ((1, 2));
extern int execl (const char *__path, const char *__arg, ...)
__THROW __nonnull ((1, 2));
extern int execvp (const char *__file, char *const __argv[])
__THROW __nonnull ((1, 2));
extern int execlp (const char *__file, const char *__arg, ...)
__THROW __nonnull ((1, 2));
#ifdef __USE_GNU
extern int execvpe (const char *__file, char *const __argv[],
char *const __envp[])
__THROW __nonnull ((1, 2));
#endif
#if defined __USE_MISC || defined __USE_XOPEN
extern int nice (int __inc) __THROW __wur;
#endif
extern void _exit (int __status) __attribute__ ((__noreturn__));
#include <bits/confname.h>
extern long int pathconf (const char *__path, int __name)
__THROW __nonnull ((1));
extern long int fpathconf (int __fd, int __name) __THROW;
extern long int sysconf (int __name) __THROW;
#ifdef	__USE_POSIX2
extern size_t confstr (int __name, char *__buf, size_t __len) __THROW
__fortified_attr_access (__write_only__, 2, 3);
#endif
extern __pid_t getpid (void) __THROW;
extern __pid_t getppid (void) __THROW;
extern __pid_t getpgrp (void) __THROW;
extern __pid_t __getpgid (__pid_t __pid) __THROW;
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
extern __pid_t getpgid (__pid_t __pid) __THROW;
#endif
extern int setpgid (__pid_t __pid, __pid_t __pgid) __THROW;
#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
extern int setpgrp (void) __THROW;
#endif
extern __pid_t setsid (void) __THROW;
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
extern __pid_t getsid (__pid_t __pid) __THROW;
#endif
extern __uid_t getuid (void) __THROW;
extern __uid_t geteuid (void) __THROW;
extern __gid_t getgid (void) __THROW;
extern __gid_t getegid (void) __THROW;
extern int getgroups (int __size, __gid_t __list[]) __THROW __wur
__fortified_attr_access (__write_only__, 2, 1);
#ifdef	__USE_GNU
extern int group_member (__gid_t __gid) __THROW;
#endif
extern int setuid (__uid_t __uid) __THROW __wur;
#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
extern int setreuid (__uid_t __ruid, __uid_t __euid) __THROW __wur;
#endif
#ifdef __USE_XOPEN2K
extern int seteuid (__uid_t __uid) __THROW __wur;
#endif
extern int setgid (__gid_t __gid) __THROW __wur;
#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
extern int setregid (__gid_t __rgid, __gid_t __egid) __THROW __wur;
#endif
#ifdef __USE_XOPEN2K
extern int setegid (__gid_t __gid) __THROW __wur;
#endif
#ifdef __USE_GNU
extern int getresuid (__uid_t *__ruid, __uid_t *__euid, __uid_t *__suid)
__THROW;
extern int getresgid (__gid_t *__rgid, __gid_t *__egid, __gid_t *__sgid)
__THROW;
extern int setresuid (__uid_t __ruid, __uid_t __euid, __uid_t __suid)
__THROW __wur;
extern int setresgid (__gid_t __rgid, __gid_t __egid, __gid_t __sgid)
__THROW __wur;
#endif
extern __pid_t fork (void) __THROWNL;
#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K8) \
|| defined __USE_MISC
extern __pid_t vfork (void) __THROW;
#endif
#ifdef __USE_GNU
extern __pid_t _Fork (void) __THROW;
#endif
extern char *ttyname (int __fd) __THROW;
extern int ttyname_r (int __fd, char *__buf, size_t __buflen)
__THROW __nonnull ((2)) __wur
__fortified_attr_access (__write_only__, 2, 3);
extern int isatty (int __fd) __THROW;
#ifdef __USE_MISC
extern int ttyslot (void) __THROW;
#endif
extern int link (const char *__from, const char *__to)
__THROW __nonnull ((1, 2)) __wur;
#ifdef __USE_ATFILE
extern int linkat (int __fromfd, const char *__from, int __tofd,
const char *__to, int __flags)
__THROW __nonnull ((2, 4)) __wur;
#endif
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K
extern int symlink (const char *__from, const char *__to)
__THROW __nonnull ((1, 2)) __wur;
extern ssize_t readlink (const char *__restrict __path,
char *__restrict __buf, size_t __len)
__THROW __nonnull ((1, 2)) __wur
__fortified_attr_access (__write_only__, 2, 3);
#endif
#ifdef __USE_ATFILE
extern int symlinkat (const char *__from, int __tofd,
const char *__to) __THROW __nonnull ((1, 3)) __wur;
extern ssize_t readlinkat (int __fd, const char *__restrict __path,
char *__restrict __buf, size_t __len)
__THROW __nonnull ((2, 3)) __wur
__fortified_attr_access (__write_only__, 3, 4);
#endif
extern int unlink (const char *__name) __THROW __nonnull ((1));
#ifdef __USE_ATFILE
extern int unlinkat (int __fd, const char *__name, int __flag)
__THROW __nonnull ((2));
#endif
extern int rmdir (const char *__path) __THROW __nonnull ((1));
extern __pid_t tcgetpgrp (int __fd) __THROW;
extern int tcsetpgrp (int __fd, __pid_t __pgrp_id) __THROW;
extern char *getlogin (void);
#ifdef __USE_POSIX199506
extern int getlogin_r (char *__name, size_t __name_len) __nonnull ((1))
__fortified_attr_access (__write_only__, 1, 2);
#endif
#ifdef	__USE_MISC
extern int setlogin (const char *__name) __THROW __nonnull ((1));
#endif
#ifdef	__USE_POSIX2
# include <bits/getopt_posix.h>
#endif
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K
extern int gethostname (char *__name, size_t __len) __THROW __nonnull ((1))
__fortified_attr_access (__write_only__, 1, 2);
#endif
#if defined __USE_MISC
extern int sethostname (const char *__name, size_t __len)
__THROW __nonnull ((1)) __wur __attr_access ((__read_only__, 1, 2));
extern int sethostid (long int __id) __THROW __wur;
extern int getdomainname (char *__name, size_t __len)
__THROW __nonnull ((1)) __wur
__fortified_attr_access (__write_only__, 1, 2);
extern int setdomainname (const char *__name, size_t __len)
__THROW __nonnull ((1)) __wur __attr_access ((__read_only__, 1, 2));
extern int vhangup (void) __THROW;
extern int revoke (const char *__file) __THROW __nonnull ((1)) __wur;
extern int profil (unsigned short int *__sample_buffer, size_t __size,
size_t __offset, unsigned int __scale)
__THROW __nonnull ((1));
extern int acct (const char *__name) __THROW;
extern char *getusershell (void) __THROW;
extern void endusershell (void) __THROW;
extern void setusershell (void) __THROW;
extern int daemon (int __nochdir, int __noclose) __THROW __wur;
#endif
#if defined __USE_MISC || (defined __USE_XOPEN && !defined __USE_XOPEN2K)
extern int chroot (const char *__path) __THROW __nonnull ((1)) __wur;
extern char *getpass (const char *__prompt) __nonnull ((1));
#endif
extern int fsync (int __fd);
#ifdef __USE_GNU
extern int syncfs (int __fd) __THROW;
#endif
#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
extern long int gethostid (void);
extern void sync (void) __THROW;
# if defined __USE_MISC || !defined __USE_XOPEN2K
extern int getpagesize (void)  __THROW __attribute__ ((__const__));
extern int getdtablesize (void) __THROW;
# endif
#endif
#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
# ifndef __USE_FILE_OFFSET64
extern int truncate (const char *__file, __off_t __length)
__THROW __nonnull ((1)) __wur;
# else
#  ifdef __REDIRECT_NTH
extern int __REDIRECT_NTH (truncate,
(const char *__file, __off64_t __length),
truncate64) __nonnull ((1)) __wur;
#  else
#   define truncate truncate64
#  endif
# endif
# ifdef __USE_LARGEFILE64
extern int truncate64 (const char *__file, __off64_t __length)
__THROW __nonnull ((1)) __wur;
# endif
#endif
#if defined __USE_POSIX199309 \
|| defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K
# ifndef __USE_FILE_OFFSET64
extern int ftruncate (int __fd, __off_t __length) __THROW __wur;
# else
#  ifdef __REDIRECT_NTH
extern int __REDIRECT_NTH (ftruncate, (int __fd, __off64_t __length),
ftruncate64) __wur;
#  else
#   define ftruncate ftruncate64
#  endif
# endif
# ifdef __USE_LARGEFILE64
extern int ftruncate64 (int __fd, __off64_t __length) __THROW __wur;
# endif
#endif
#if (defined __USE_XOPEN_EXTENDED && !defined __USE_XOPEN2K) \
|| defined __USE_MISC
extern int brk (void *__addr) __THROW __wur;
extern void *sbrk (intptr_t __delta) __THROW;
#endif
#ifdef __USE_MISC
extern long int syscall (long int __sysno, ...) __THROW;
#endif
#if (defined __USE_MISC || defined __USE_XOPEN_EXTENDED) && !defined F_LOCK
# define F_ULOCK 0
# define F_LOCK  1
# define F_TLOCK 2
# define F_TEST  3
# ifndef __USE_FILE_OFFSET64
extern int lockf (int __fd, int __cmd, __off_t __len) __wur;
# else
#  ifdef __REDIRECT
extern int __REDIRECT (lockf, (int __fd, int __cmd, __off64_t __len),
lockf64) __wur;
#  else
#   define lockf lockf64
#  endif
# endif
# ifdef __USE_LARGEFILE64
extern int lockf64 (int __fd, int __cmd, __off64_t __len) __wur;
# endif
#endif
#ifdef __USE_GNU
# define TEMP_FAILURE_RETRY(expression) \
(__extension__							      \
({ long int __result;						      \
do __result = (long int) (expression);				      \
while (__result == -1L && errno == EINTR);			      \
__result; }))
ssize_t copy_file_range (int __infd, __off64_t *__pinoff,
int __outfd, __off64_t *__poutoff,
size_t __length, unsigned int __flags);
#endif
#if defined __USE_POSIX199309 || defined __USE_UNIX98
extern int fdatasync (int __fildes);
#endif
#ifdef __USE_MISC
extern char *crypt (const char *__key, const char *__salt)
__THROW __nonnull ((1, 2));
#endif
#ifdef	__USE_XOPEN
extern void swab (const void *__restrict __from, void *__restrict __to,
ssize_t __n) __THROW __nonnull ((1, 2))
__attr_access ((__read_only__, 1, 3))
__attr_access ((__write_only__, 2, 3));
#endif
#if defined __USE_XOPEN && !defined __USE_XOPEN2K
extern char *ctermid (char *__s) __THROW;
extern char *cuserid (char *__s);
#endif
#if defined __USE_UNIX98 && !defined __USE_XOPEN2K
extern int pthread_atfork (void (*__prepare) (void),
void (*__parent) (void),
void (*__child) (void)) __THROW;
#endif
#ifdef __USE_MISC
int getentropy (void *__buffer, size_t __length) __wur
__attr_access ((__write_only__, 1, 2));
#endif
#ifdef __USE_GNU
extern int close_range (unsigned int __fd, unsigned int __max_fd,
int __flags) __THROW;
#endif
#if __USE_FORTIFY_LEVEL > 0 && defined __fortify_function
# include <bits/unistd.h>
#endif
#include <bits/unistd_ext.h>
__END_DECLS
#endif