#include "stdio_.h"
#include "string_.h"
#include "memory_.h"
#include "pipe_.h"
#include <stdlib.h>
#include <stdarg.h>
#include "ctype_.h"
#include <io.h>
#include "malloc_.h"
#include <fcntl.h>
#include <signal.h>
#include "gx.h"
#include "gp.h"
#include "gpcheck.h"
#include "gpmisc.h"
#include "gserrors.h"
#include "gsexit.h"
#include "windows_.h"
#include <shellapi.h>
#include <winspool.h>
#include "gp_mswin.h"
extern char *getenv(const char *);
#define MAXSTR 255
char win_prntmp[MAXSTR];
HINSTANCE phInstance;
BOOL is_win32s = FALSE;
const LPSTR szAppName = "Ghostscript";
private int is_printer(const char *name);
void
gp_init(void)
{
}
void
gp_exit(int exit_status, int code)
{
}
void
gp_do_exit(int exit_status)
{
exit(exit_status);
}
int gp_cache_insert(int type, byte *key, int keylen, void *buffer, int buflen)
{
return 0;
}
int gp_cache_query(int type, byte* key, int keylen, void **buffer,
gp_cache_alloc alloc, void *userdata)
{
return -1;
}
private int gp_printfile(const char *, const char *);
FILE *
gp_open_printer(char fname[gp_file_name_sizeof], int binary_mode)
{
if (is_printer(fname)) {
FILE *pfile;
pfile = gp_open_scratch_file(gp_scratch_file_name_prefix,
win_prntmp, "wb");
return pfile;
} else if (fname[0] == '|')
return popen(fname + 1, (binary_mode ? "wb" : "w"));
else
return fopen(fname, (binary_mode ? "wb" : "w"));
}
void
gp_close_printer(FILE * pfile, const char *fname)
{
fclose(pfile);
if (!is_printer(fname))
return;
gp_printfile(win_prntmp, fname);
unlink(win_prntmp);
}
DLGRETURN CALLBACK
SpoolDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
LPSTR entry;
switch (message) {
case WM_INITDIALOG:
entry = (LPSTR) lParam;
while (*entry) {
SendDlgItemMessage(hDlg, SPOOL_PORT, LB_ADDSTRING, 0, (LPARAM) entry);
entry += lstrlen(entry) + 1;
}
SendDlgItemMessage(hDlg, SPOOL_PORT, LB_SETCURSEL, 0, (LPARAM) 0);
return TRUE;
case WM_COMMAND:
switch (LOWORD(wParam)) {
case SPOOL_PORT:
if (HIWORD(wParam) == LBN_DBLCLK)
PostMessage(hDlg, WM_COMMAND, IDOK, 0L);
return FALSE;
case IDOK:
EndDialog(hDlg, 1 + (int)SendDlgItemMessage(hDlg, SPOOL_PORT, LB_GETCURSEL, 0, 0L));
return TRUE;
case IDCANCEL:
EndDialog(hDlg, 0);
return TRUE;
}
}
return FALSE;
}
int
is_spool(const char *queue)
{
char *prefix = "\\\\spool";
int i;
for (i = 0; i < 7; i++) {
if (prefix[i] == '\\') {
if ((*queue != '\\') && (*queue != '/'))
return FALSE;
} else if (tolower(*queue) != prefix[i])
return FALSE;
queue++;
}
if (*queue && (*queue != '\\') && (*queue != '/'))
return FALSE;
return TRUE;
}
private int
is_printer(const char *name)
{
char buf[128];
if (strlen(name) == 0)
return TRUE;
GetProfileString("ports", name, "XYZ", buf, sizeof(buf));
if (strlen(name) == 0 || strcmp(buf, "XYZ"))
return TRUE;
if (is_spool(name))
return TRUE;
return FALSE;
}
private int gp_printfile_win32(const char *filename, char *port);
private int gp_printfile_gs16spl(const char *filename, const char *port);
private int
gp_printfile(const char *filename, const char *pmport)
{
if (!is_win32s) {
if (strlen(pmport) == 0) {
char buf[256];
char *p;
GetProfileString("windows", "device", "", buf, sizeof(buf));
if ((p = strchr(buf, ',')) != NULL)
*p = '\0';
return gp_printfile_win32(filename, buf);
} else if (is_spool(pmport)) {
if (strlen(pmport) >= 8)
return gp_printfile_win32(filename, (char *)pmport + 8);
else
return gp_printfile_win32(filename, (char *)NULL);
} else
return gp_printfile_gs16spl(filename, pmport);
} else {
if (is_spool(pmport)) {
if (strlen(pmport) >= 8) {
char driverbuf[256];
char *output;
GetProfileString("Devices", pmport + 8, "", driverbuf, sizeof(driverbuf));
strtok(driverbuf, ",");
output = strtok(NULL, ",");
return gp_printfile_gs16spl(filename, output);
} else
return gp_printfile_gs16spl(filename, (char *)NULL);
} else
return gp_printfile_gs16spl(filename, pmport);
}
}
#define PRINT_BUF_SIZE 16384u
#define PORT_BUF_SIZE 4096
char *
get_queues(void)
{
int i;
DWORD count, needed;
PRINTER_INFO_1 *prinfo;
char *enumbuffer;
char *buffer;
char *p;
EnumPrinters(PRINTER_ENUM_CONNECTIONS | PRINTER_ENUM_LOCAL, NULL, 1, NULL, 0, &needed, &count);
if (needed == 0) {
enumbuffer = malloc(4);
if (enumbuffer == (char *)NULL)
return NULL;
memset(enumbuffer, 0, 4);
return enumbuffer;
}
enumbuffer = malloc(needed);
if (enumbuffer == (char *)NULL)
return NULL;
if (!EnumPrinters(PRINTER_ENUM_CONNECTIONS | PRINTER_ENUM_LOCAL, NULL, 1, (LPBYTE) enumbuffer, needed, &needed, &count)) {
char buf[256];
free(enumbuffer);
sprintf(buf, "EnumPrinters() failed, error code = %d", GetLastError());
MessageBox((HWND) NULL, buf, szAppName, MB_OK | MB_ICONSTOP);
return NULL;
}
prinfo = (PRINTER_INFO_1 *) enumbuffer;
if ((buffer = malloc(PORT_BUF_SIZE)) == (char *)NULL) {
free(enumbuffer);
return NULL;
}
p = buffer;
for (i = 0; i < count; i++) {
if (strlen(prinfo[i].pName) + 1 < (PORT_BUF_SIZE - (p - buffer))) {
strcpy(p, prinfo[i].pName);
p += strlen(p) + 1;
}
}
*p = '\0';
free(enumbuffer);
return buffer;
}
char *
get_ports(void)
{
char *buffer;
if (!is_win32s)
return get_queues();
if ((buffer = malloc(PORT_BUF_SIZE)) == (char *)NULL)
return NULL;
GetProfileString("ports", NULL, "", buffer, PORT_BUF_SIZE);
return buffer;
}
BOOL
get_queuename(char *portname, const char *queue)
{
char *buffer;
char *p;
int i, iport;
buffer = get_queues();
if (buffer == NULL)
return FALSE;
if ((queue == (char *)NULL) || (strlen(queue) == 0)) {
iport = DialogBoxParam(phInstance, "QueueDlgBox", (HWND) NULL, SpoolDlgProc, (LPARAM) buffer);
if (!iport) {
free(buffer);
return FALSE;
}
p = buffer;
for (i = 1; i < iport && strlen(p) != 0; i++)
p += lstrlen(p) + 1;
strcpy(portname, "\\\\spool\\");
strcat(portname, p);
} else {
strcpy(portname, "\\\\spool\\");
strcat(portname, queue);
}
free(buffer);
return TRUE;
}
BOOL
get_portname(char *portname, const char *port)
{
char *buffer;
char *p;
int i, iport;
char filename[MAXSTR];
buffer = get_ports();
if (buffer == NULL)
return FALSE;
if ((port == (char *)NULL) || (strlen(port) == 0)) {
if (buffer == (char *)NULL)
return FALSE;
iport = DialogBoxParam(phInstance, "SpoolDlgBox", (HWND) NULL, SpoolDlgProc, (LPARAM) buffer);
if (!iport) {
free(buffer);
return FALSE;
}
p = buffer;
for (i = 1; i < iport && strlen(p) != 0; i++)
p += lstrlen(p) + 1;
strcpy(portname, p);
} else
strcpy(portname, port);
if (strlen(portname) == 0)
return FALSE;
if (strcmp(portname, "FILE:") == 0) {
OPENFILENAME ofn;
filename[0] = '\0';
memset(&ofn, 0, sizeof(OPENFILENAME));
ofn.lStructSize = sizeof(OPENFILENAME);
ofn.hwndOwner = (HWND) NULL;
ofn.lpstrFile = filename;
ofn.nMaxFile = sizeof(filename);
ofn.Flags = OFN_PATHMUSTEXIST;
if (!GetSaveFileName(&ofn)) {
free(buffer);
return FALSE;
}
strcpy(portname, filename);
}
free(buffer);
return TRUE;
}
private int
gp_printfile_win32(const char *filename, char *port)
{
DWORD count;
char *buffer;
char portname[MAXSTR];
FILE *f;
HANDLE printer;
DOC_INFO_1 di;
DWORD written;
if (!get_queuename(portname, port))
return FALSE;
port = portname + 8;
if ((buffer = malloc(PRINT_BUF_SIZE)) == (char *)NULL)
return FALSE;
if ((f = fopen(filename, "rb")) == (FILE *) NULL) {
free(buffer);
return FALSE;
}
if (!OpenPrinter(port, &printer, NULL)) {
char buf[256];
sprintf(buf, "OpenPrinter() failed for \042%s\042, error code = %d", port, GetLastError());
MessageBox((HWND) NULL, buf, szAppName, MB_OK | MB_ICONSTOP);
free(buffer);
return FALSE;
}
di.pDocName = szAppName;
di.pOutputFile = NULL;
di.pDatatype = "RAW";
if (!StartDocPrinter(printer, 1, (LPBYTE) & di)) {
char buf[256];
sprintf(buf, "StartDocPrinter() failed, error code = %d", GetLastError());
MessageBox((HWND) NULL, buf, szAppName, MB_OK | MB_ICONSTOP);
AbortPrinter(printer);
free(buffer);
return FALSE;
}
while ((count = fread(buffer, 1, PRINT_BUF_SIZE, f)) != 0) {
if (!WritePrinter(printer, (LPVOID) buffer, count, &written)) {
free(buffer);
fclose(f);
AbortPrinter(printer);
return FALSE;
}
}
fclose(f);
free(buffer);
if (!EndDocPrinter(printer)) {
char buf[256];
sprintf(buf, "EndDocPrinter() failed, error code = %d", GetLastError());
MessageBox((HWND) NULL, buf, szAppName, MB_OK | MB_ICONSTOP);
AbortPrinter(printer);
return FALSE;
}
if (!ClosePrinter(printer)) {
char buf[256];
sprintf(buf, "ClosePrinter() failed, error code = %d", GetLastError());
MessageBox((HWND) NULL, buf, szAppName, MB_OK | MB_ICONSTOP);
return FALSE;
}
return TRUE;
}
int
gp_printfile_gs16spl(const char *filename, const char *port)
{
char portname[MAXSTR];
HINSTANCE hinst;
char command[MAXSTR];
char *p;
HWND hwndspl;
if (!get_portname(portname, port))
return FALSE;
GetModuleFileName(phInstance, command, sizeof(command));
if ((p = strrchr(command, '\\')) != (char *)NULL)
p++;
else
p = command;
*p = '\0';
sprintf(command + strlen(command), "gs16spl.exe %s %s",
portname, filename);
hinst = (HINSTANCE) WinExec(command, SW_SHOWNORMAL);
if (hinst < (HINSTANCE) HINSTANCE_ERROR) {
char buf[MAXSTR];
sprintf(buf, "Can't run: %s", command);
MessageBox((HWND) NULL, buf, szAppName, MB_OK | MB_ICONSTOP);
return FALSE;
}
hwndspl = FindWindow(NULL, "GS Win32s/Win16 spooler");
while (IsWindow(hwndspl)) {
gp_check_interrupts(NULL);
}
return 0;
}
FILE *mswin_popen(const char *cmd, const char *mode)
{
SECURITY_ATTRIBUTES saAttr;
STARTUPINFO siStartInfo;
PROCESS_INFORMATION piProcInfo;
HANDLE hPipeTemp = INVALID_HANDLE_VALUE;
HANDLE hChildStdinRd = INVALID_HANDLE_VALUE;
HANDLE hChildStdinWr = INVALID_HANDLE_VALUE;
HANDLE hChildStdoutWr = INVALID_HANDLE_VALUE;
HANDLE hChildStderrWr = INVALID_HANDLE_VALUE;
HANDLE hProcess = GetCurrentProcess();
int handle = 0;
char *command = NULL;
FILE *pipe = NULL;
if (strcmp(mode, "wb") != 0)
return NULL;
saAttr.nLength = sizeof(SECURITY_ATTRIBUTES);
saAttr.bInheritHandle = TRUE;
saAttr.lpSecurityDescriptor = NULL;
if (handle == 0)
if (!CreatePipe(&hChildStdinRd, &hPipeTemp, &saAttr, 0))
handle = -1;
if (handle == 0) {
if (!DuplicateHandle(hProcess, hPipeTemp,
hProcess, &hChildStdinWr, 0, FALSE ,
DUPLICATE_SAME_ACCESS))
handle = -1;
CloseHandle(hPipeTemp);
}
if (handle == 0)
if (!DuplicateHandle(hProcess, GetStdHandle(STD_OUTPUT_HANDLE),
hProcess, &hChildStdoutWr, 0, TRUE ,
DUPLICATE_SAME_ACCESS))
handle = -1;
if (handle == 0)
if (!DuplicateHandle(hProcess, GetStdHandle(STD_ERROR_HANDLE),
hProcess, &hChildStderrWr, 0, TRUE ,
DUPLICATE_SAME_ACCESS))
handle = -1;
memset(&siStartInfo, 0, sizeof(STARTUPINFO));
siStartInfo.cb = sizeof(STARTUPINFO);
siStartInfo.dwFlags = STARTF_USESTDHANDLES;
siStartInfo.hStdInput = hChildStdinRd;
siStartInfo.hStdOutput = hChildStdoutWr;
siStartInfo.hStdError = hChildStderrWr;
if (handle == 0) {
command = (char *)malloc(strlen(cmd)+1);
if (command)
strcpy(command, cmd);
else
handle = -1;
}
if (handle == 0)
if (!CreateProcess(NULL,
command,
NULL,
NULL,
TRUE,
0,
NULL,
NULL,
&siStartInfo,
&piProcInfo))
{
handle = -1;
}
else {
CloseHandle(piProcInfo.hProcess);
CloseHandle(piProcInfo.hThread);
handle = _open_osfhandle((long)hChildStdinWr, 0);
}
if (hChildStdinRd != INVALID_HANDLE_VALUE)
CloseHandle(hChildStdinRd);
if (hChildStdoutWr != INVALID_HANDLE_VALUE)
CloseHandle(hChildStdoutWr);
if (hChildStderrWr != INVALID_HANDLE_VALUE)
CloseHandle(hChildStderrWr);
if (command)
free(command);
if (handle < 0) {
if (hChildStdinWr != INVALID_HANDLE_VALUE)
CloseHandle(hChildStdinWr);
}
else {
pipe = _fdopen(handle, "wb");
if (pipe == NULL)
_close(handle);
}
return pipe;
}
FILE *
gp_open_scratch_file(const char *prefix, char *fname, const char *mode)
{
UINT n;
DWORD l;
HANDLE hfile = INVALID_HANDLE_VALUE;
int fd = -1;
FILE *f = NULL;
char sTempDir[_MAX_PATH];
char sTempFileName[_MAX_PATH];
memset(fname, 0, gp_file_name_sizeof);
if (!gp_file_name_is_absolute(prefix, strlen(prefix))) {
int plen = sizeof(sTempDir);
if (gp_gettmpdir(sTempDir, &plen) != 0)
l = GetTempPath(sizeof(sTempDir), sTempDir);
else
l = strlen(sTempDir);
} else {
strncpy(sTempDir, prefix, sizeof(sTempDir));
prefix = "";
l = strlen(sTempDir);
}
if (sTempDir[l-1] == '/')
sTempDir[l-1] = '\\';
if (l <= sizeof(sTempDir)) {
n = GetTempFileName(sTempDir, prefix, 0, sTempFileName);
if (n == 0) {
int l = strlen(sTempDir), i;
for (i = l - 1; i > 0; i--) {
uint slen = gs_file_name_check_separator(sTempDir + i, l, sTempDir + l);
if (slen > 0) {
sTempDir[i] = 0;
i += slen;
break;
}
}
if (i > 0)
n = GetTempFileName(sTempDir, sTempDir + i, 0, sTempFileName);
}
if (n != 0) {
hfile = CreateFile(sTempFileName,
GENERIC_READ | GENERIC_WRITE | DELETE,
FILE_SHARE_READ | FILE_SHARE_WRITE, NULL, CREATE_ALWAYS,
FILE_ATTRIBUTE_NORMAL ,
NULL);
}
}
if (hfile != INVALID_HANDLE_VALUE) {
fd = _open_osfhandle((long)hfile, 0);
if (fd == -1)
CloseHandle(hfile);
else {
f = fdopen(fd, mode);
if (f == NULL)
_close(fd);
}
}
if (f != NULL) {
if ((strlen(sTempFileName) < gp_file_name_sizeof))
strncpy(fname, sTempFileName, gp_file_name_sizeof - 1);
else {
fclose(f);
f = NULL;
}
}
if (f == NULL)
eprintf1("**** Could not open temporary file '%s'\n", fname);
return f;
}
FILE *
gp_fopen(const char *fname, const char *mode)
{
return fopen(fname, mode);
}
void *gp_enumerate_fonts_init(gs_memory_t *mem)
{
return NULL;
}
int gp_enumerate_fonts_next(void *enum_state, char **fontname, char **path)
{
return 0;
}
void gp_enumerate_fonts_free(void *enum_state)
{
}