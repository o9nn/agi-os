#ifndef _9FS_H_
#define _9FS_H_
#ifdef KERNEL
#include "opt_u9fs.h"
#endif
#define U9FS_FABLKSIZE   512
#define U9FS_PORT        17008
#define	U9FSMNT_SOFT		0x00000001
#define	U9FSMNT_MAXGRPS		0x00000020
#define	U9FSMNT_INT		0x00000040
#define	U9FSMNT_KERB		0x00000400
#define	U9FSMNT_READAHEAD	0x00002000
struct p9user {
uid_t p9_uid;
char p9_name[U9FS_NAMELEN];
};
#define U9FS_ARGSVERSION	1
struct u9fs_args {
int		version;
struct sockaddr	*addr;
int		addrlen;
int		sotype;
int		proto;
int		fhsize;
int		flags;
int		wsize;
int		rsize;
int		readdirsize;
char		*hostname;
struct sockaddr * authaddr;
int             authaddrlen;
int             authsotype;
int             authsoproto;
int             nusers;
char            uname[U9FS_NAMELEN];
char            key[U9AUTH_DESKEYLEN];
struct p9user * users;
};
struct u9fsnode {
LIST_ENTRY(u9fsnode)	n_hash;
u_quad_t		n_size;
u_quad_t		n_lrev;
struct vattr		n_vattr;
time_t			n_attrstamp;
u_int32_t		n_mode;
uid_t			n_modeuid;
time_t			n_modestamp;
time_t			n_mtime;
time_t			n_ctime;
u_short			*n_fid;
struct vnode		*n_vnode;
struct lockf		*n_lockf;
int			n_error;
#if 0
union {
struct timespec	nf_atim;
u9fsuint64	nd_cookieverf;
} n_un1;
union {
struct timespec	nf_mtim;
off_t		nd_direof;
} n_un2;
union {
struct sillyrename *nf_silly;
LIST_HEAD(, u9fsdmap) nd_cook;
} n_un3;
#endif
short			n_flag;
};
#define n_atim		n_un1.nf_atim
#define n_mtim		n_un2.nf_mtim
#define n_sillyrename	n_un3.nf_silly
#define n_cookieverf	n_un1.nd_cookieverf
#define n_direofoffset	n_un2.nd_direof
#define n_cookies	n_un3.nd_cook
#define	NFLUSHWANT	0x0001
#define	NFLUSHINPROG	0x0002
#define	NMODIFIED	0x0004
#define	NWRITEERR	0x0008
#define	NQU9FSNONCACHE	0x0020
#define	NQU9FSWRITE	0x0040
#define	NQU9FSEVICTED	0x0080
#define	NACC		0x0100
#define	NUPD		0x0200
#define	NCHG		0x0400
#define NLOCKED		0x0800
#define NWANTED		0x0100
#define VTOU9FS(vp)	((struct u9fsnode *)(vp)->v_data)
#define U9FSTOV(np)	((struct vnode *)(np)->n_vnode)
struct	u9fsmount {
int	nm_flag;
int	nm_state;
struct	mount *nm_mountp;
int	nm_numgrps;
u_short	nm_fid;
struct	socket *nm_so;
int	nm_sotype;
int	nm_soproto;
int	nm_soflags;
struct	sockaddr *nm_nam;
int	nm_sent;
int	nm_cwnd;
int	nm_rsize;
int	nm_wsize;
int	nm_readdirsize;
#if 0
struct vnode *nm_inprog;
uid_t	nm_authuid;
int	nm_authtype;
int	nm_authlen;
char	*nm_authstr;
char	*nm_verfstr;
int	nm_verflen;
u_char	nm_verf[U9FSX_V3WRITEVERF];
U9FSKERBKEY_T nm_key;
int	nm_numuids;
TAILQ_HEAD(, u9fsuid) nm_uidlruhead;
LIST_HEAD(, u9fsuid) nm_uidhashtbl[U9FS_MUIDHASHSIZ];
TAILQ_HEAD(, buf) nm_bufq;
short	nm_bufqlen;
short	nm_bufqwant;
int	nm_bufqiods;
#endif
u_int64_t nm_maxfilesize;
};
#ifdef KERNEL
#ifdef MALLOC_DECLARE
MALLOC_DECLARE(M_U9FSHASH);
#endif
#define VFSTOU9FS(mp)	((struct u9fsmount *)((mp)->mnt_data))
#endif
#endif