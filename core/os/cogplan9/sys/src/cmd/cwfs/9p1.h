#include <authsrv.h>
enum {
DIRREC	= 116,
ERRREC	= 64,
};
typedef	struct	Fcall	Fcall;
struct	Fcall
{
char	type;
ushort	fid;
short	err;
short	tag;
union
{
struct
{
short	uid;
short	oldtag;
Qid9p1	qid;
char	rauth[AUTHENTLEN];
};
struct
{
char	uname[NAMELEN];
char	aname[NAMELEN];
char	ticket[TICKETLEN];
char	auth[AUTHENTLEN];
};
struct
{
char	ename[ERRREC];
char	chal[CHALLEN];
char	authid[NAMELEN];
char	authdom[DOMLEN];
};
struct
{
char	name[NAMELEN];
long	perm;
ushort	newfid;
char	mode;
};
struct
{
Off	offset;
long	count;
char*	data;
};
struct
{
char	stat[DIRREC];
};
};
};
enum
{
Tnop =		50,
Rnop,
Tosession =	52,
Rosession,
Terror =	54,
Rerror,
Tflush =	56,
Rflush,
Toattach =	58,
Roattach,
Tclone =	60,
Rclone,
Twalk =		62,
Rwalk,
Topen =		64,
Ropen,
Tcreate =	66,
Rcreate,
Tread =		68,
Rread,
Twrite =	70,
Rwrite,
Tclunk =	72,
Rclunk,
Tremove =	74,
Rremove,
Tstat =		76,
Rstat,
Twstat =	78,
Rwstat,
Tclwalk =	80,
Rclwalk,
Tauth =		82,
Rauth,
Tsession =	84,
Rsession,
Tattach =	86,
Rattach,
MAXSYSCALL
};
int	convA2M9p1(Authenticator*, char*, char*);
void	convM2A9p1(char*, Authenticator*, char*);
void	convM2T9p1(char*, Ticket*, char*);
int	convD2M9p1(Dentry*, char*);
int	convM2D9p1(char*, Dentry*);
int	convM2S9p1(uchar*, Fcall*, int);
int	convS2M9p1(Fcall*, uchar*);
void	fcall9p1(Chan*, Fcall*, Fcall*);
void	(*call9p1[MAXSYSCALL])(Chan*, Fcall*, Fcall*);