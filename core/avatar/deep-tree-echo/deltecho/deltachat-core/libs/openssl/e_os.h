#ifndef HEADER_E_OS_H
# define HEADER_E_OS_H
# include <openssl/opensslconf.h>
# include <openssl/e_os2.h>
#ifdef  __cplusplus
extern "C" {
#endif
# ifdef REF_PRINT
#  undef REF_PRINT
#  define REF_PRINT(a,b)  fprintf(stderr,"%08X:%4d:%s\n",(int)b,b->references,a)
# endif
# ifndef DEVRANDOM
#  define DEVRANDOM "/dev/urandom","/dev/random","/dev/srandom"
# endif
# ifndef DEVRANDOM_EGD
#  define DEVRANDOM_EGD "/var/run/egd-pool","/dev/egd-pool","/etc/egd-pool","/etc/entropy"
# endif
# if defined(OPENSSL_SYS_VXWORKS)
#  define NO_SYS_PARAM_H
#  define NO_CHMOD
#  define NO_SYSLOG
# endif
# if defined(OPENSSL_SYS_MACINTOSH_CLASSIC)
#  if macintosh==1
#   ifndef MAC_OS_GUSI_SOURCE
#    define MAC_OS_pre_X
#    define NO_SYS_TYPES_H
#   endif
#   define NO_SYS_PARAM_H
#   define NO_CHMOD
#   define NO_SYSLOG
#   undef  DEVRANDOM
#   define GETPID_IS_MEANINGLESS
#  endif
# endif
# if defined(OPENSSL_SYS_MSDOS) && !defined(OPENSSL_SYSNAME_WIN32)
#  define MS_STATIC     static
# else
#  define MS_STATIC
# endif
# if defined(OPENSSL_SYS_WIN32) && !defined(WIN32)
#  define WIN32
# endif
# if defined(OPENSSL_SYS_WINDOWS) && !defined(WINDOWS)
#  define WINDOWS
# endif
# if defined(OPENSSL_SYS_MSDOS) && !defined(MSDOS)
#  define MSDOS
# endif
# if defined(MSDOS) && !defined(GETPID_IS_MEANINGLESS)
#  define GETPID_IS_MEANINGLESS
# endif
# ifdef WIN32
#  define get_last_sys_error()    GetLastError()
#  define clear_sys_error()       SetLastError(0)
#  if !defined(WINNT)
#   define WIN_CONSOLE_BUG
#  endif
# else
#  define get_last_sys_error()    errno
#  define clear_sys_error()       errno=0
# endif
# if defined(WINDOWS)
#  define get_last_socket_error() WSAGetLastError()
#  define clear_socket_error()    WSASetLastError(0)
#  define readsocket(s,b,n)       recv((s),(b),(n),0)
#  define writesocket(s,b,n)      send((s),(b),(n),0)
# elif defined(__DJGPP__)
#  define WATT32
#  define get_last_socket_error() errno
#  define clear_socket_error()    errno=0
#  define closesocket(s)          close_s(s)
#  define readsocket(s,b,n)       read_s(s,b,n)
#  define writesocket(s,b,n)      send(s,b,n,0)
# elif defined(MAC_OS_pre_X)
#  define get_last_socket_error() errno
#  define clear_socket_error()    errno=0
#  define closesocket(s)          MacSocket_close(s)
#  define readsocket(s,b,n)       MacSocket_recv((s),(b),(n),true)
#  define writesocket(s,b,n)      MacSocket_send((s),(b),(n))
# elif defined(OPENSSL_SYS_VMS)
#  define get_last_socket_error() errno
#  define clear_socket_error()    errno=0
#  define ioctlsocket(a,b,c)      ioctl(a,b,c)
#  define closesocket(s)          close(s)
#  define readsocket(s,b,n)       recv((s),(b),(n),0)
#  define writesocket(s,b,n)      send((s),(b),(n),0)
# elif defined(OPENSSL_SYS_VXWORKS)
#  define get_last_socket_error() errno
#  define clear_socket_error()    errno=0
#  define ioctlsocket(a,b,c)          ioctl((a),(b),(int)(c))
#  define closesocket(s)              close(s)
#  define readsocket(s,b,n)           read((s),(b),(n))
#  define writesocket(s,b,n)          write((s),(char *)(b),(n))
# elif defined(OPENSSL_SYS_BEOS_R5)
#  define get_last_socket_error() errno
#  define clear_socket_error()    errno=0
#  define FIONBIO SO_NONBLOCK
#  define ioctlsocket(a,b,c)                setsockopt((a),SOL_SOCKET,(b),(c),sizeof(*(c)))
#  define readsocket(s,b,n)       recv((s),(b),(n),0)
#  define writesocket(s,b,n)      send((s),(b),(n),0)
# elif defined(OPENSSL_SYS_NETWARE)
#  if defined(NETWARE_BSDSOCK)
#   define get_last_socket_error() errno
#   define clear_socket_error()    errno=0
#   define closesocket(s)          close(s)
#   define ioctlsocket(a,b,c)      ioctl(a,b,c)
#   if defined(NETWARE_LIBC)
#    define readsocket(s,b,n)       recv((s),(b),(n),0)
#    define writesocket(s,b,n)      send((s),(b),(n),0)
#   else
#    define readsocket(s,b,n)       recv((s),(char*)(b),(n),0)
#    define writesocket(s,b,n)      send((s),(char*)(b),(n),0)
#   endif
#  else
#   define get_last_socket_error() WSAGetLastError()
#   define clear_socket_error()    WSASetLastError(0)
#   define readsocket(s,b,n)               recv((s),(b),(n),0)
#   define writesocket(s,b,n)              send((s),(b),(n),0)
#  endif
# else
#  define get_last_socket_error() errno
#  define clear_socket_error()    errno=0
#  define ioctlsocket(a,b,c)      ioctl(a,b,c)
#  define closesocket(s)          close(s)
#  define readsocket(s,b,n)       read((s),(b),(n))
#  define writesocket(s,b,n)      write((s),(b),(n))
# endif
# ifdef WIN16
#  define MS_CALLBACK   _far _loadds
#  define MS_FAR        _far
# else
#  define MS_CALLBACK
#  define MS_FAR
# endif
# ifdef OPENSSL_NO_STDIO
#  undef OPENSSL_NO_FP_API
#  define OPENSSL_NO_FP_API
# endif
# if (defined(WINDOWS) || defined(MSDOS))
#  ifdef __DJGPP__
#   include <unistd.h>
#   include <sys/stat.h>
#   include <sys/socket.h>
#   include <tcp.h>
#   include <netdb.h>
#   define _setmode setmode
#   define _O_TEXT O_TEXT
#   define _O_BINARY O_BINARY
#   undef DEVRANDOM
#   define DEVRANDOM "/dev/urandom\x24"
#  endif
#  ifndef S_IFDIR
#   define S_IFDIR     _S_IFDIR
#  endif
#  ifndef S_IFMT
#   define S_IFMT      _S_IFMT
#  endif
#  if !defined(WINNT) && !defined(__DJGPP__)
#   define NO_SYSLOG
#  endif
#  define NO_DIRENT
#  ifdef WINDOWS
#   if !defined(_WIN32_WCE) && !defined(_WIN32_WINNT)
#    define _WIN32_WINNT 0x0400
#   endif
#   if !defined(OPENSSL_NO_SOCK) && defined(_WIN32_WINNT)
#    include <winsock2.h>
#    include <ws2tcpip.h>
#   endif
#   include <windows.h>
#   include <stdio.h>
#   include <stddef.h>
#   include <errno.h>
#   include <string.h>
#   ifdef _WIN64
#    define strlen(s) _strlen31(s)
static __inline unsigned int _strlen31(const char *str)
{
unsigned int len = 0;
while (*str && len < 0x80000000U)
str++, len++;
return len & 0x7FFFFFFF;
}
#   endif
#   include <malloc.h>
#   if defined(_MSC_VER) && _MSC_VER<=1200 && defined(_MT) && defined(isspace)
#    undef isspace
#    undef isdigit
#    undef isalnum
#    undef isupper
#    undef isxdigit
#   endif
#   if defined(_MSC_VER) && !defined(_DLL) && defined(stdin)
#    if _MSC_VER>=1300 && _MSC_VER<1600
#     undef stdin
#     undef stdout
#     undef stderr
FILE *__iob_func();
#     define stdin  (&__iob_func()[0])
#     define stdout (&__iob_func()[1])
#     define stderr (&__iob_func()[2])
#    elif _MSC_VER<1300 && defined(I_CAN_LIVE_WITH_LNK4049)
#     undef stdin
#     undef stdout
#     undef stderr
extern FILE *_imp___iob;
#     define stdin  (&_imp___iob[0])
#     define stdout (&_imp___iob[1])
#     define stderr (&_imp___iob[2])
#    endif
#   endif
#  endif
#  include <io.h>
#  include <fcntl.h>
#  ifdef OPENSSL_SYS_WINCE
#   define OPENSSL_NO_POSIX_IO
#  endif
#  if defined (__BORLANDC__)
#   define _setmode setmode
#   define _O_TEXT O_TEXT
#   define _O_BINARY O_BINARY
#   define _int64 __int64
#   define _kbhit kbhit
#  endif
#  define EXIT(n) exit(n)
#  define LIST_SEPARATOR_CHAR ';'
#  ifndef X_OK
#   define X_OK        0
#  endif
#  ifndef W_OK
#   define W_OK        2
#  endif
#  ifndef R_OK
#   define R_OK        4
#  endif
#  define OPENSSL_CONF  "openssl.cnf"
#  define SSLEAY_CONF   OPENSSL_CONF
#  define NUL_DEV       "nul"
#  define RFILE         ".rnd"
#  ifdef OPENSSL_SYS_WINCE
#   define DEFAULT_HOME  ""
#  else
#   define DEFAULT_HOME  "C:"
#  endif
#  if defined(_MSC_VER) && _MSC_VER>=1800
#   define check_winnt() (1)
#   define check_win_minplat(x) (1)
#  else
#   define check_winnt() (GetVersion() < 0x80000000)
#   define check_win_minplat(x) (LOBYTE(LOWORD(GetVersion())) >= (x))
#  endif
# else
#  ifdef OPENSSL_SYS_VMS
#   define VMS 1
#   include <stdlib.h>
#   if defined(__DECC)
#    include <unistd.h>
#   else
#    include <unixlib.h>
#   endif
#   define OPENSSL_CONF        "openssl.cnf"
#   define SSLEAY_CONF         OPENSSL_CONF
#   define RFILE               ".rnd"
#   define LIST_SEPARATOR_CHAR ','
#   define NUL_DEV             "NLA0:"
#   undef DEVRANDOM
#   define EXIT(n)             do { int __VMS_EXIT = n; \
if (__VMS_EXIT == 0) \
__VMS_EXIT = 1; \
else \
__VMS_EXIT = (n << 3) | 2; \
__VMS_EXIT |= 0x10000000; \
exit(__VMS_EXIT); } while(0)
#   define NO_SYS_PARAM_H
#  elif defined(OPENSSL_SYS_NETWARE)
#   include <fcntl.h>
#   include <unistd.h>
#   define NO_SYS_TYPES_H
#   undef  DEVRANDOM
#   ifdef NETWARE_CLIB
#    define getpid GetThreadID
extern int GetThreadID(void);
extern int kbhit(void);
#   else
#    include <screen.h>
#   endif
#   define NO_SYSLOG
#   define _setmode setmode
#   define _kbhit kbhit
#   define _O_TEXT O_TEXT
#   define _O_BINARY O_BINARY
#   define OPENSSL_CONF   "openssl.cnf"
#   define SSLEAY_CONF    OPENSSL_CONF
#   define RFILE    ".rnd"
#   define LIST_SEPARATOR_CHAR ';'
#   define EXIT(n)  { if (n) printf("ERROR: %d\n", (int)n); exit(n); }
#  else
#   ifdef OPENSSL_SYS_MPE
#    define NO_SYS_PARAM_H
#   endif
#   ifdef OPENSSL_UNISTD
#    include OPENSSL_UNISTD
#   else
#    include <unistd.h>
#   endif
#   ifndef NO_SYS_TYPES_H
#    include <sys/types.h>
#   endif
#   if defined(NeXT) || defined(OPENSSL_SYS_NEWS4)
#    define pid_t int
#   endif
#   ifdef OPENSSL_SYS_NEWS4
#    define setvbuf(a, b, c, d) setbuffer((a), (b), (d))
typedef unsigned long clock_t;
#   endif
#   ifdef OPENSSL_SYS_WIN32_CYGWIN
#    include <io.h>
#    include <fcntl.h>
#   endif
#   define OPENSSL_CONF        "openssl.cnf"
#   define SSLEAY_CONF         OPENSSL_CONF
#   define RFILE               ".rnd"
#   define LIST_SEPARATOR_CHAR ':'
#   define NUL_DEV             "/dev/null"
#   define EXIT(n)             exit(n)
#  endif
#  define SSLeay_getpid()       getpid()
# endif
# ifdef USE_SOCKETS
#  if defined(WINDOWS) || defined(MSDOS)
#   ifdef OPENSSL_NO_SOCK
#    define SSLeay_Write(a,b,c)       (-1)
#    define SSLeay_Read(a,b,c)        (-1)
#    define SHUTDOWN(fd)              close(fd)
#    define SHUTDOWN2(fd)             close(fd)
#   elif !defined(__DJGPP__)
#    if defined(_WIN32_WCE) && _WIN32_WCE<410
#     define getservbyname _masked_declaration_getservbyname
#    endif
#    if !defined(IPPROTO_IP)
#     include <winsock.h>
#    endif
#    ifdef getservbyname
#     undef getservbyname
struct servent *PASCAL getservbyname(const char *, const char *);
#    endif
#    ifdef _WIN64
#     define socket(d,t,p)   ((int)socket(d,t,p))
#     define accept(s,f,l)   ((int)accept(s,f,l))
#    endif
#    define SSLeay_Write(a,b,c)       send((a),(b),(c),0)
#    define SSLeay_Read(a,b,c)        recv((a),(b),(c),0)
#    define SHUTDOWN(fd)              { shutdown((fd),0); closesocket(fd); }
#    define SHUTDOWN2(fd)             { shutdown((fd),2); closesocket(fd); }
#   else
#    define SSLeay_Write(a,b,c)       write_s(a,b,c,0)
#    define SSLeay_Read(a,b,c)        read_s(a,b,c)
#    define SHUTDOWN(fd)              close_s(fd)
#    define SHUTDOWN2(fd)             close_s(fd)
#   endif
#  elif defined(MAC_OS_pre_X)
#   include "MacSocket.h"
#   define SSLeay_Write(a,b,c)         MacSocket_send((a),(b),(c))
#   define SSLeay_Read(a,b,c)          MacSocket_recv((a),(b),(c),true)
#   define SHUTDOWN(fd)                MacSocket_close(fd)
#   define SHUTDOWN2(fd)               MacSocket_close(fd)
#  elif defined(OPENSSL_SYS_NETWARE)
#   if defined(NETWARE_BSDSOCK)
#    include <sys/socket.h>
#    include <netinet/in.h>
#    include <sys/time.h>
#    if defined(NETWARE_CLIB)
#     include <sys/bsdskt.h>
#    else
#     include <sys/select.h>
#    endif
#    define INVALID_SOCKET (int)(~0)
#   else
#    include <novsock2.h>
#   endif
#   define SSLeay_Write(a,b,c)   send((a),(b),(c),0)
#   define SSLeay_Read(a,b,c) recv((a),(b),(c),0)
#   define SHUTDOWN(fd)    { shutdown((fd),0); closesocket(fd); }
#   define SHUTDOWN2(fd)      { shutdown((fd),2); closesocket(fd); }
#  else
#   ifndef NO_SYS_PARAM_H
#    include <sys/param.h>
#   endif
#   ifdef OPENSSL_SYS_VXWORKS
#    include <time.h>
#   elif !defined(OPENSSL_SYS_MPE)
#    include <sys/time.h>
#   endif
#   include <netdb.h>
#   if defined(OPENSSL_SYS_VMS_NODECC)
#    include <socket.h>
#    include <in.h>
#    include <inet.h>
#   else
#    include <sys/socket.h>
#    ifdef FILIO_H
#     include <sys/filio.h>
#    endif
#    include <netinet/in.h>
#    if !defined(OPENSSL_SYS_BEOS_R5)
#     include <arpa/inet.h>
#    endif
#   endif
#   if defined(NeXT) || defined(_NEXT_SOURCE)
#    include <sys/fcntl.h>
#    include <sys/types.h>
#   endif
#   ifdef OPENSSL_SYS_AIX
#    include <sys/select.h>
#   endif
#   ifdef __QNX__
#    include <sys/select.h>
#   endif
#   if defined(__sun) || defined(sun)
#    include <sys/filio.h>
#   else
#    ifndef VMS
#     include <sys/ioctl.h>
#    else
#     if !defined(TCPIP_TYPE_SOCKETSHR) && defined(__VMS_VER) && (__VMS_VER > 70000000)
#      include <sys/ioctl.h>
#     endif
#    endif
#   endif
#   ifdef VMS
#    include <unixio.h>
#    if defined(TCPIP_TYPE_SOCKETSHR)
#     include <socketshr.h>
#    endif
#   endif
#   define SSLeay_Read(a,b,c)     read((a),(b),(c))
#   define SSLeay_Write(a,b,c)    write((a),(b),(c))
#   define SHUTDOWN(fd)    { shutdown((fd),0); closesocket((fd)); }
#   define SHUTDOWN2(fd)   { shutdown((fd),2); closesocket((fd)); }
#   ifndef INVALID_SOCKET
#    define INVALID_SOCKET      (-1)
#   endif
#  endif
#  if !defined(OPENSSL_USE_IPV6)
#   if defined(AF_INET6) && !defined(OPENSSL_SYS_BEOS_BONE) && !defined(NETWARE_CLIB)
#    define OPENSSL_USE_IPV6 1
#   else
#    define OPENSSL_USE_IPV6 0
#   endif
#  endif
# endif
# if (defined(__sun) || defined(sun)) && !defined(__svr4__) && !defined(__SVR4)
#  include <stdlib.h>
#  include <string.h>
#  define memmove(s1,s2,n) bcopy((s2),(s1),(n))
#  define strtoul(s,e,b) ((unsigned long int)strtol((s),(e),(b)))
extern char *sys_errlist[];
extern int sys_nerr;
#  define strerror(errnum) \
(((errnum)<0 || (errnum)>=sys_nerr) ? NULL : sys_errlist[errnum])
#  include "crypto/o_str.h"
#  define memcmp OPENSSL_memcmp
# endif
# ifndef OPENSSL_EXIT
#  if defined(MONOLITH) && !defined(OPENSSL_C)
#   define OPENSSL_EXIT(n) return(n)
#  else
#   define OPENSSL_EXIT(n) do { EXIT(n); return(n); } while(0)
#  endif
# endif
# define DG_GCC_BUG
# ifdef sgi
#  define IRIX_CC_BUG
# endif
# ifdef OPENSSL_SYS_SNI
#  define IRIX_CC_BUG
# endif
# if defined(OPENSSL_SYS_WINDOWS)
#  define strcasecmp _stricmp
#  define strncasecmp _strnicmp
# elif defined(OPENSSL_SYS_VMS)
#  include "o_str.h"
#  define strcasecmp OPENSSL_strcasecmp
#  define strncasecmp OPENSSL_strncasecmp
#  define OPENSSL_IMPLEMENTS_strncasecmp
# elif defined(OPENSSL_SYS_OS2) && defined(__EMX__)
#  define strcasecmp stricmp
#  define strncasecmp strnicmp
# elif defined(OPENSSL_SYS_NETWARE)
#  include <string.h>
#  if defined(NETWARE_CLIB)
#   define strcasecmp stricmp
#   define strncasecmp strnicmp
#  endif
# endif
# if defined(OPENSSL_SYS_OS2) && defined(__EMX__)
#  include <io.h>
#  include <fcntl.h>
#  define NO_SYSLOG
# endif
# if defined(OPENSSL_SYS_VXWORKS)
#  include <ioLib.h>
#  include <tickLib.h>
#  include <sysLib.h>
#  define TTY_STRUCT int
#  define sleep(a) taskDelay((a) * sysClkRateGet())
#  include <vxWorks.h>
#  include <sockLib.h>
#  include <taskLib.h>
#  define getpid taskIdSelf
struct hostent *gethostbyname(const char *name);
struct hostent *gethostbyaddr(const char *addr, int length, int type);
struct servent *getservbyname(const char *name, const char *proto);
# endif
# if defined(OPENSSL_SYS_BEOS_R5)
#  define SO_ERROR 0
#  define NO_SYS_UN
#  define IPPROTO_IP 0
#  include <OS.h>
# endif
# if !defined(inline) && !defined(__cplusplus)
#  if defined(__STDC_VERSION__) && __STDC_VERSION__>=199901L
#  elif defined(__GNUC__) && __GNUC__>=2
#   define inline __inline__
#  elif defined(_MSC_VER)
#   define inline __inline
#  else
#   define inline
#  endif
# endif
#ifdef  __cplusplus
}
#endif
#endif