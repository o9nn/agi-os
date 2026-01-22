#define Unknown win_Unknown
#define UNICODE
#include	<windows.h>
#include <winbase.h>
#include	<winsock.h>
#undef Unknown
#include	"dat.h"
#include	"fns.h"
#include	"error.h"
extern int	nth2fd(HANDLE);
extern wchar_t	*widen(char*);
static char *
dblquote(char *cmd, char *s)
{
int nb;
char *p;
for(p=s; *p; p++)
if(*p == ' ' || *p == '\t' || *p == '\n' || *p == '\r' || *p == '"')
break;
if(*p == 0){
strcpy(cmd, s);
return cmd+(p-s);
}
*cmd++ = '"';
for(;;) {
for(nb=0; *s=='\\'; nb++)
*cmd++ = *s++;
if(*s == 0) {
while(nb-- > 0)
*cmd++ = '\\';
break;
}
if(*s == '"') {
while(nb-- > 0)
*cmd++ = '\\';
*cmd++ = '\\';
}
*cmd++ = *s++;
}
*cmd++ = '"';
*cmd = 0;
return cmd;
}
static char *
ntquotedcmd(char **argv)
{
int i, n;
char *cmd, *p;
for(i=0,n=0; argv[i]; i++)
n += 2*strlen(argv[i]);
n++;
cmd = malloc(n);
if(cmd == nil)
return nil;
for(i=0,p=cmd; argv[i]; i++) {
p = dblquote(p, argv[i]);
*p++ = ' ';
}
if(p != cmd)
p--;
*p = 0;
return cmd;
}
static HANDLE
exporthandle(HANDLE h, int close)
{
HANDLE cp, dh;
DWORD flags = DUPLICATE_SAME_ACCESS;
if (close)
flags |= DUPLICATE_CLOSE_SOURCE;
cp = GetCurrentProcess();
if (!DuplicateHandle(cp, h, cp, &dh, DUPLICATE_SAME_ACCESS, 1, flags))
return nil;
return dh;
}
void*
oscmd(char **args, int nice, char *dir, int *fd)
{
STARTUPINFO si;
SECURITY_ATTRIBUTES sec;
HANDLE rh, wh, eh, srh, swh, seh;
PROCESS_INFORMATION pinfo;
char *cmd;
wchar_t *wcmd, *wdir;
int prio;
wdir = nil;
if(dir != nil)
wdir = widen(dir);
cmd = ntquotedcmd(args);
if(cmd == nil)
error(Enomem);
wcmd = widen(cmd);
sec.nLength = sizeof(sec);
sec.lpSecurityDescriptor = 0;
sec.bInheritHandle = 0;
rh = wh = eh = srh = swh = seh = nil;
if(!CreatePipe(&rh, &swh, &sec, 0))
goto Error;
if(!CreatePipe(&srh, &wh, &sec, 0))
goto Error;
if(!CreatePipe(&seh, &eh, &sec, 0))
goto Error;
rh = exporthandle(rh, 1);
if(rh == nil)
goto Error;
wh = exporthandle(wh, 1);
if(wh == nil)
goto Error;
eh = exporthandle(eh, 1);
if(eh == nil)
goto Error;
memset(&si, 0, sizeof(si));
si.cb = sizeof(si);
si.dwFlags = STARTF_USESHOWWINDOW|STARTF_USESTDHANDLES;
si.wShowWindow = SW_SHOW;
si.hStdInput = rh;
si.hStdOutput = wh;
si.hStdError = eh;
prio = 0;
if(nice){
prio = IDLE_PRIORITY_CLASS;
if(nice > 1)
prio |= CREATE_SUSPENDED;
}
if(!CreateProcess(nil, wcmd, 0, 0, 1,
CREATE_NEW_PROCESS_GROUP|CREATE_DEFAULT_ERROR_MODE|prio,
0 , wdir, &si, &pinfo)){
goto Error;
}
fd[0] = nth2fd(swh);
fd[1] = nth2fd(srh);
fd[2] = nth2fd(seh);
if(fd[1] == 1 || fd[2] == 2)
panic("invalid mapping of handle to fd");
CloseHandle(si.hStdInput);
CloseHandle(si.hStdOutput);
CloseHandle(si.hStdError);
if(prio & CREATE_SUSPENDED){
if(nice > 1)
SetThreadPriority(pinfo.hThread,
nice>3? THREAD_PRIORITY_IDLE:
nice>2? THREAD_PRIORITY_LOWEST:
THREAD_PRIORITY_BELOW_NORMAL);
ResumeThread(pinfo.hThread);
}
CloseHandle(pinfo.hThread);
free(cmd);
free(wcmd);
free(wdir);
return pinfo.hProcess;
Error:
if(rh)
CloseHandle(rh);
if(wh)
CloseHandle(wh);
if(eh)
CloseHandle(eh);
if(srh)
CloseHandle(srh);
if(swh)
CloseHandle(swh);
if(seh)
CloseHandle(seh);
free(cmd);
free(wcmd);
free(wdir);
return nil;
}
int
oscmdwait(void *v, char *buf, int n)
{
int status;
HANDLE proc = (HANDLE)v;
if(WaitForSingleObject(proc, INFINITE) == WAIT_FAILED)
return -1;
if(!GetExitCodeProcess(proc, &status))
status = 1;
if(status)
n = snprint(buf, n, "0 0 0 0 'status %d'", status);
else
n = snprint(buf, n, "0 0 0 0 ''");
return n;
}
int
oscmdkill(void *v)
{
if(TerminateProcess((HANDLE)v, 666) == FALSE)
return -1;
return 0;
}
void
oscmdfree(void *v)
{
CloseHandle((HANDLE)v);
}