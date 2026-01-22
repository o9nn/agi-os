#define MAXSAME 32
#define MAXSAMECHAR 1024
typedef enum {
d_undefined,
d_pipe,
d_cat,
d_translate,
d_alias,
d_auth,
d_syntax,
d_unknown,
d_loop,
d_eloop,
d_noforward,
d_badmbox,
d_resource,
d_pipeto,
} d_status;
typedef struct dest dest;
struct dest {
dest	*next;
dest	*same;
dest	*parent;
String	*addr;
String	*repl1;
String	*repl2;
int	pstat;
d_status status;
int	authorized;
int	nsame;
int	nchar;
};
typedef struct message message;
struct message {
String	*sender;
String	*replyaddr;
String	*date;
String	*body;
String	*tmp;
String	*to;
int	size;
int	fd;
char	haveto;
String	*havefrom;
String	*havesender;
String	*havereplyto;
char	havedate;
char	havemime;
String	*havesubject;
char	bulk;
char	rfc822headers;
int	received;
char	*boundary;
};
extern int rmail;
extern int onatty;
extern char *thissys, *altthissys;
extern int xflg;
extern int nflg;
extern int tflg;
extern int debug;
extern int nosummary;
extern void	authorize(dest*);
extern int	cat_mail(dest*, message*);
extern dest	*up_bind(dest*, message*, int);
extern int	ok_to_forward(char*);
extern int	lookup(char*, char*, Biobuf**, char*, Biobuf**);
extern dest	*d_new(String*);
extern void	d_free(dest*);
extern dest	*d_rm(dest**);
extern void	d_insert(dest**, dest*);
extern dest	*d_rm_same(dest**);
extern void	d_same_insert(dest**, dest*);
extern String	*d_to(dest*);
extern dest	*s_to_dest(String*, dest*);
extern void	gateway(message*);
extern dest	*expand_local(dest*);
extern void	logdelivery(dest*, char*, message*);
extern void	loglist(dest*, message*, char*);
extern void	logrefusal(dest*, message*, char*);
extern int	default_from(message*);
extern message	*m_new(void);
extern void	m_free(message*);
extern message	*m_read(Biobuf*, int, int);
extern int	m_get(message*, long, char**);
extern int	m_print(message*, Biobuf*, char*, int);
extern int	m_bprint(message*, Biobuf*);
extern String	*rule_parse(String*, char*, int*);
extern int	getrules(void);
extern int	rewrite(dest*, message*);
extern void	dumprules(void);
extern void	regerror(char*);
extern dest	*translate(dest*);
extern char*	skipequiv(char*);
extern int	refuse(dest*, message*, char*, int, int);