enum {
STACKSIZE = 2048 * sizeof(void*),
};
typedef enum {
Category,
Cddata,
Cmd,
File,
Include,
Key,
Lyrics,
Part,
Path,
Performance,
Recording,
Root,
Search,
Soloists,
Time,
Track,
Work,
Ntoken,
Eof	=	-1,
Txt	=	-2,
BraceO	=	-3,
BraceC	=	-4,
Equals	=	-5,
Newcat	=	-6,
} Type;
typedef struct Object Object;
typedef struct Catset Catset;
typedef struct Token Token;
typedef struct Cmdlist Cmdlist;
typedef enum {
Obj,
Cat,
} Kind;
struct Catset {
uchar *bitpiece;
int nbitpiece;
};
struct Token {
char	*name;
Kind	kind;
long	value;
char	*kname;
Catset	categories;
};
typedef enum {
Hierarchy,
Typelist,
Nlisttype,
} Listtype;
struct Cmdlist {
int	flag;
char	*name;
};
#define KEYLEN 128
struct Object {
Type	type;
int	tabno;
Object	*parent;
Object	**children;
Object	**catparents;
Object	*orig;
int	nchildren;
int	ncatparents;
Catset	categories;
int	flags;
int	num;
char	*value;
char	key[KEYLEN];
char	*path;
};
#define Sort	0x01
#define Enum	0x02
#define Hier	0x04
#define Elab	0x10
extern	Token	*tokenlist;
extern	int	ncat;
extern	Object	**catobjects;
extern	Biobuf	*f;
extern	char	*file;
extern	Object	*root;
extern	int	ntoken;
extern	Object	**otab;
extern	int	notab;
extern	int	sotab;
extern	int	hotab;
extern	char	*user;
void	io(void *);
long	printchildren(char*, int, Object*);
long	printdigest(char*, int, Object*);
long	printfiles(char*, int, Object*);
long	printfulltext(char*, int, Object*);
long	printkey(char*, int, Object*);
long	printminiparentage(char*, int, Object*);
long	printparent(char*, int, Object*);
long	printparentage(char*, int, Object*);
long	printtext(char*, int, Object*);
long	printtype(char*, int, Object*);
void	reread(void);
void	listfiles(Object *o);