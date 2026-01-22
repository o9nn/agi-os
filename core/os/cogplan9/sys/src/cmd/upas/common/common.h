#include "sys.h"
extern char *REMFROMRE;
extern int REMSENDERMATCH;
extern int REMDATEMATCH;
extern int REMSYSMATCH;
#define IS_HEADER(p) ((p)[0]=='F'&&(p)[1]=='r'&&(p)[2]=='o'&&(p)[3]=='m'&&(p)[4]==' ')
#define IS_TRAILER(p) ((p)[0]=='m'&&(p)[1]=='o'&&(p)[2]=='r'&&(p)[3]=='F'&&(p)[4]=='\n')
extern char *FROMRE;
extern int SENDERMATCH;
extern int DATEMATCH;
enum
{
Elemlen= 28,
Errlen=	ERRMAX,
Pathlen= 256,
};
enum { Atnoteunknown, Atnoterecog };
extern int	print_header(Biobuf*, char*, char*);
extern int	print_remote_header(Biobuf*, char*, char*, char*);
extern int	parse_header(char*, String*, String*);
extern String	*abspath(char*, char*, String*);
extern String	*mboxpath(char*, char*, String*, int);
extern char	*basename(char*);
extern int	delivery_status(String*);
extern void	append_match(Resub*, String*, int);
extern int	shellchars(char*);
extern String*	escapespecial(String*);
extern String*	unescapespecial(String*);
extern int	returnable(char*);
extern int	appendfiletombox(int, int);
extern int	appendfiletofile(int, int);
#define MF_NORMAL 0
#define MF_PIPE 1
#define MF_FORWARD 2
#define MF_NOMBOX 3
#define MF_NOTMBOX 4
typedef struct {
Biobuf	bb;
Biobuf	*fp;
int	fd;
} stream;
typedef struct process{
stream	*std[3];
int	pid;
int	status;
Waitmsg	*waitmsg;
} process;
extern stream	*instream(void);
extern stream	*outstream(void);
extern void	stream_free(stream*);
extern process	*noshell_proc_start(char**, stream*, stream*, stream*, int, char*);
extern process	*proc_start(char*, stream*, stream*, stream*, int, char*);
extern int	proc_wait(process*);
extern int	proc_free(process*);
extern int	proc_kill(process*);
#define USE(x)	if(x)