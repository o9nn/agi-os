#include "u.h"
#include "libc.h"
#define MAXLINE 200
#define WORD 256
#define EOF -1
#define ispunct(c)		(c=='|' || c=='&' || c==';' || c=='<' || \
c=='>' || c=='(' || c==')' || c=='\n')
#define isspace(c)		(c==' ' || c=='\t')
#define execute(np)		(ignored = (np? (*(np)->op)(np) : 0))
typedef struct Node	Node;
struct Node{
int (*op)(Node *);
Node *args[2];
char *argv[100];
char *io[3];
};
Node	nodes[25];
Node	*nfree;
char	strspace[10*MAXLINE];
char	*sfree;
int	t;
char 	*token;
int	putback = 0;
char	status[256];
int	cflag = 0;
int	tflag = 0;
int	interactive = 0;
char	*cflagp;
char	*path[] ={"/bin", 0};
int	ignored;
Node	*alloc(int (*op)(Node *));
int	builtin(Node *np);
Node	*command(void);
int	getch(void);
int	gettoken(void);
Node	*list(void);
void	error(char *s, char *t);
Node	*pipeline(void);
void	redirect(Node *np);
int	setio(Node *np);
Node	*simple(void);
int	xpipeline(Node *np);
int	xsimple(Node *np);
int	xsubshell(Node *np);
int	xnowait(Node *np);
int	xwait(Node *np);
void
main(int argc, char *argv[])
{
Node *np;
if(argc>1 && strcmp(argv[1], "-t")==0)
tflag++;
else if(argc>2 && strcmp(argv[1], "-c")==0){
cflag++;
cflagp = argv[2];
}else if(argc>1){
close(0);
if(open(argv[1], 0) != 0){
error(": can't open", argv[1]);
exits("argument");
}
}else
interactive = 1;
for(;;){
if(interactive)
fprint(2, "%d$ ", getpid());
nfree = nodes;
sfree = strspace;
if((t=gettoken()) == EOF)
break;
if(t != '\n')
if(np = list())
execute(np);
else
error("syntax error", "");
while(t!=EOF && t!='\n')
t = gettoken();
}
exits(status);
}
Node*
alloc(int (*op)(Node *))
{
if(nfree < nodes+sizeof(nodes)){
nfree->op = op;
nfree->args[0] = nfree->args[1] = 0;
nfree->argv[0] = nfree->argv[1] = 0;
nfree->io[0] = nfree->io[1] = nfree->io[2] = 0;
return nfree++;
}
error("node storage overflow", "");
exits("node storage overflow");
return nil;
}
int
builtin(Node *np)
{
int n = 0;
char name[MAXLINE];
Waitmsg *wmsg;
if(np->argv[1])
n = strtoul(np->argv[1], 0, 0);
if(strcmp(np->argv[0], "cd") == 0){
if(chdir(np->argv[1]? np->argv[1] : "/") == -1)
error(": bad directory", np->argv[0]);
return 1;
}else if(strcmp(np->argv[0], "exit") == 0)
exits(np->argv[1]? np->argv[1] : status);
else if(strcmp(np->argv[0], "bind") == 0){
if(np->argv[1]==0 || np->argv[2]==0)
error("usage: bind new old", "");
else if(bind(np->argv[1], np->argv[2], 0)==-1)
error("bind failed", "");
return 1;
#ifdef asdf
}else if(strcmp(np->argv[0], "unmount") == 0){
if(np->argv[1] == 0)
error("usage: unmount [new] old", "");
else if(np->argv[2] == 0){
if(unmount((char *)0, np->argv[1]) == -1)
error("unmount:", "");
}else if(unmount(np->argv[1], np->argv[2]) == -1)
error("unmount", "");
return 1;
#endif
}else if(strcmp(np->argv[0], "wait") == 0){
while((wmsg = wait()) != nil){
strncpy(status, wmsg->msg, sizeof(status)-1);
if(n && wmsg->pid==n){
n = 0;
free(wmsg);
break;
}
free(wmsg);
}
if(n)
error("wait error", "");
return 1;
}else if(strcmp(np->argv[0], "rfork") == 0){
char *p;
int mask;
p = np->argv[1];
if(p == 0 || *p == 0)
p = "ens";
mask = 0;
while(*p)
switch(*p++){
case 'n': mask |= RFNAMEG; break;
case 'N': mask |= RFCNAMEG; break;
case 'e': mask |= RFENVG; break;
case 'E': mask |= RFCENVG; break;
case 's': mask |= RFNOTEG; break;
case 'f': mask |= RFFDG; break;
case 'F': mask |= RFCFDG; break;
case 'm': mask |= RFNOMNT; break;
default: error(np->argv[1], "bad rfork flag");
}
rfork(mask);
return 1;
}else if(strcmp(np->argv[0], "exec") == 0){
redirect(np);
if(np->argv[1] == (char *) 0)
return 1;
exec(np->argv[1], &np->argv[1]);
n = np->argv[1][0];
if(n!='/' && n!='#' && (n!='.' || np->argv[1][1]!='/'))
for(n = 0; path[n]; n++){
sprint(name, "%s/%s", path[n], np->argv[1]);
exec(name, &np->argv[1]);
}
error(": not found", np->argv[1]);
return 1;
}
return 0;
}
Node*
command(void)
{
Node *np;
if(t != '(')
return simple();
np = alloc(xsubshell);
t = gettoken();
if((np->args[0]=list())==0 || t!=')')
return 0;
while((t=gettoken())=='<' || t=='>')
if(!setio(np))
return 0;
return np;
}
int
getch(void)
{
unsigned char c;
static done=0;
if(putback){
c = putback;
putback = 0;
}else if(tflag){
if(done || read(0, &c, 1)!=1){
done = 1;
return EOF;
}
if(c == '\n')
done = 1;
}else if(cflag){
if(done)
return EOF;
if((c=*cflagp++) == 0){
done = 1;
c = '\n';
}
}else if(read(0, &c, 1) != 1)
return EOF;
return c;
}
int
gettoken(void)
{
int c;
while((c = getch()) != EOF)
if(!isspace(c))
break;
if(c==EOF || ispunct(c))
return c;
token = sfree;
do{
if(sfree >= strspace+sizeof(strspace) - 1){
error("string storage overflow", "");
exits("string storage overflow");
}
*sfree++ = c;
}while((c=getch()) != EOF && !ispunct(c) && !isspace(c));
*sfree++ = 0;
putback = c;
return WORD;
}
Node*
list(void)
{
Node *np, *np1;
np = alloc(0);
if((np->args[1]=pipeline()) == 0)
return 0;
while(t==';' || t=='&'){
np->op = (t==';')? xwait : xnowait;
t = gettoken();
if(t==')' || t=='\n')
break;
np1 = alloc(0);
np1->args[0] = np;
if((np1->args[1]=pipeline()) == 0)
return 0;
np = np1;
}
if(np->op == 0)
np->op = xwait;
return np;
}
void
error(char *s, char *t)
{
char buf[256];
fprint(2, "%s%s", t, s);
errstr(buf, sizeof buf);
fprint(2, ": %s\n", buf);
}
Node*
pipeline(void)
{
Node *np, *np1;
if((np=command()) == 0)
return 0;
while(t == '|'){
np1 = alloc(xpipeline);
np1->args[0] = np;
t = gettoken();
if((np1->args[1]=command()) == 0)
return 0;
np = np1;
}
return np;
}
void
redirect(Node *np)
{
int fd;
if(np->io[0]){
if((fd = open(np->io[0], 0)) < 0){
error(": can't open", np->io[0]);
exits("open");
}
dup(fd, 0);
close(fd);
}
if(np->io[1]){
if((fd = create(np->io[1], 1, 0666L)) < 0){
error(": can't create", np->io[1]);
exits("create");
}
dup(fd, 1);
close(fd);
}
if(np->io[2]){
if((fd = open(np->io[2], 1)) < 0 && (fd = create(np->io[2], 1, 0666L)) < 0){
error(": can't write", np->io[2]);
exits("write");
}
dup(fd, 1);
close(fd);
seek(1, 0, 2);
}
}
int
setio(Node *np)
{
if(t == '<'){
t = gettoken();
np->io[0] = token;
}else if(t == '>'){
t = gettoken();
if(t == '>'){
t = gettoken();
np->io[2] = token;
}else
np->io[1] = token;
}else
return 0;
if(t != WORD)
return 0;
return 1;
}
Node*
simple(void)
{
Node *np;
int n = 1;
if(t != WORD)
return 0;
np = alloc(xsimple);
np->argv[0] = token;
while((t = gettoken())==WORD || t=='<' || t=='>')
if(t == WORD)
np->argv[n++] = token;
else if(!setio(np))
return 0;
np->argv[n] = 0;
return np;
}
int
xpipeline(Node *np)
{
int pid, fd[2];
if(pipe(fd) < 0){
error("can't create pipe", "");
return 0;
}
if((pid=fork()) == 0){
dup(fd[1], 1);
close(fd[0]);
close(fd[1]);
execute(np->args[0]);
exits(status);
}else if(pid == -1){
error("can't create process", "");
return 0;
}
if((pid=fork()) == 0){
dup(fd[0], 0);
close(fd[0]);
close(fd[1]);
pid = execute(np->args[1]);
if(pid > 0)
while(waitpid()!=pid)
;
exits(0);
}else if(pid == -1){
error("can't create process", "");
return 0;
}
close(fd[0]);
close(fd[1]);
return pid;
}
int
xsimple(Node *np)
{
char name[MAXLINE];
int pid, i;
if(builtin(np))
return 0;
if(pid = fork()){
if(pid == -1)
error(": can't create process", np->argv[0]);
return pid;
}
redirect(np);
exec(np->argv[0], &np->argv[0]);
i = np->argv[0][0];
if(i!='/' && i!='#' && (i!='.' || np->argv[0][1]!='/'))
for(i = 0; path[i]; i++){
sprint(name, "%s/%s", path[i], np->argv[0]);
exec(name, &np->argv[0]);
}
error(": not found", np->argv[0]);
exits("not found");
return -1;
}
int
xsubshell(Node *np)
{
int pid;
if(pid = fork()){
if(pid == -1)
error("can't create process", "");
return pid;
}
redirect(np);
execute(np->args[0]);
exits(status);
return -1;
}
int
xnowait(Node *np)
{
int pid;
execute(np->args[0]);
pid = execute(np->args[1]);
if(interactive)
fprint(2, "%d\n", pid);
return 0;
}
int xwait(Node *np)
{
int pid;
Waitmsg *wmsg;
execute(np->args[0]);
pid = execute(np->args[1]);
if(pid > 0){
while((wmsg = wait()) != nil){
if(wmsg->pid == pid)
break;
free(wmsg);
}
if(wmsg == nil)
error("wait error", "");
else {
strncpy(status, wmsg->msg, sizeof(status)-1);
free(wmsg);
}
}
return 0;
}