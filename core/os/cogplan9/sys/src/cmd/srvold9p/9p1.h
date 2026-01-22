#include <authsrv.h>
#define	DIRREC		116
#define	ERRREC		64
#define	NAMEREC	28
typedef	struct	Fcall9p1	Fcall9p1;
typedef	struct	Qid9p1	Qid9p1;
struct	Qid9p1
{
long	path;
long	version;
};
struct	Fcall9p1
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
char	uname[NAMEREC];
char	aname[NAMEREC];
char	ticket[TICKETLEN];
char	auth[AUTHENTLEN];
};
struct
{
char	ename[ERRREC];
char	chal[CHALLEN];
char	authid[NAMEREC];
char	authdom[DOMLEN];
};
struct
{
char	name[NAMEREC];
long	perm;
ushort	newfid;
char	mode;
};
struct
{
long	offset;
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
Tnop9p1 =		50,
Rnop9p1,
Tosession9p1 =	52,
Rosession9p1,
Terror9p1 =	54,
Rerror9p1,
Tflush9p1 =	56,
Rflush9p1,
Toattach9p1 =	58,
Roattach9p1,
Tclone9p1 =	60,
Rclone9p1,
Twalk9p1 =		62,
Rwalk9p1,
Topen9p1 =		64,
Ropen9p1,
Tcreate9p1 =	66,
Rcreate9p1,
Tread9p1 =		68,
Rread9p1,
Twrite9p1 =	70,
Rwrite9p1,
Tclunk9p1 =	72,
Rclunk9p1,
Tremove9p1 =	74,
Rremove9p1,
Tstat9p1 =		76,
Rstat9p1,
Twstat9p1 =	78,
Rwstat9p1,
Tclwalk9p1 =	80,
Rclwalk9p1,
Tauth9p1 =		82,
Rauth9p1,
Tsession9p1 =	84,
Rsession9p1,
Tattach9p1 =	86,
Rattach9p1,
MAXSYSCALL
};
int	convA2M9p1(Authenticator*, char*, char*);
void	convM2A9p1(char*, Authenticator*, char*);
void	convM2T9p1(char*, Ticket*, char*);
int	convD2M9p1(Dir*, char*);
int	convM2D9p1(char*, Dir*);
int	convM2S9p1(char*, Fcall9p1*, int);
int	convS2M9p1(Fcall9p1*, char*);
int	fcallfmt9p1(Fmt*);
int	fcall(int);
#pragma	varargck	type	"F"	Fcall*
#pragma	varargck	type	"G"	Fcall9p1*
#pragma	varargck	type	"D"	Dir*
void	fatal(char*, ...);