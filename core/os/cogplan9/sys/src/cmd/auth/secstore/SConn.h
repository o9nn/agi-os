enum {
Maxmsg	= 4096,
};
typedef struct SConn SConn;
struct SConn {
void 	*chan;
int 	secretlen;
int 	(*secret)(SConn*, uchar*, int);
int 	(*read)(SConn*, uchar*, int);
int	(*write)(SConn*, uchar*, int);
void	(*free)(SConn*);
};
SConn *newSConn(int);
void	writerr(SConn*, char*);
int	readstr(SConn*, char*);
void	*emalloc(ulong);
void	*erealloc(void*, ulong);
char	*estrdup(char*);