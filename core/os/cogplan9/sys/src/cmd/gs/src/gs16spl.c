#define STRICT
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define ID_TEXT 100
DECLARE_HANDLE(HPJOB);
HPJOB WINAPI OpenJob(LPSTR, LPSTR, HPJOB);
int WINAPI StartSpoolPage(HPJOB);
int WINAPI EndSpoolPage(HPJOB);
int WINAPI WriteSpool(HPJOB, LPSTR, int);
int WINAPI CloseJob(HPJOB);
int WINAPI DeleteJob(HPJOB, int);
int WINAPI WriteDialog(HPJOB, LPSTR, int);
int WINAPI DeleteSpoolPage(HPJOB);
#define MAXSTR 256
#define PRINT_BUF_SIZE 16384
HPJOB hJob;
HWND hwndspl;
DLGPROC lpfnSpoolProc;
HINSTANCE phInstance;
char port[MAXSTR];
char filename[MAXSTR];
char error_message[MAXSTR];
int error;
char szAppName[] = "GS Win32s/Win16 spooler";
int
spoolfile(char *portname, char *filename)
{
FILE *f;
char *buffer;
char pcdone[64];
long ldone;
long lsize;
int count;
MSG msg;
if ((*portname == '\0') || (*filename == '\0')) {
strcpy(error_message, "Usage: gs16spl port filename");
return FALSE;
}
if ((buffer = malloc(PRINT_BUF_SIZE)) == (char *)NULL)
return FALSE;
if ((f = fopen(filename, "rb")) == (FILE *) NULL) {
sprintf(error_message, "Can't open %s", filename);
free(buffer);
return FALSE;
}
fseek(f, 0L, SEEK_END);
lsize = ftell(f);
if (lsize <= 0)
lsize = 1;
fseek(f, 0L, SEEK_SET);
ldone = 0;
hJob = OpenJob(portname, filename, (HDC) NULL);
switch ((int)hJob) {
case SP_APPABORT:
case SP_ERROR:
case SP_OUTOFDISK:
case SP_OUTOFMEMORY:
case SP_USERABORT:
fclose(f);
free(buffer);
return FALSE;
}
if (StartSpoolPage(hJob) < 0)
error = TRUE;
while (!error
&& (count = fread(buffer, 1, PRINT_BUF_SIZE, f)) != 0) {
if (WriteSpool(hJob, buffer, count) < 0)
error = TRUE;
ldone += count;
sprintf(pcdone, "%d%% written to %s", (int)(ldone * 100 / lsize), portname);
SetWindowText(GetDlgItem(hwndspl, ID_TEXT), pcdone);
while (PeekMessage(&msg, 0, 0, 0, PM_REMOVE)) {
TranslateMessage(&msg);
DispatchMessage(&msg);
}
}
free(buffer);
fclose(f);
EndSpoolPage(hJob);
if (error)
DeleteJob(hJob, 0);
else
CloseJob(hJob);
return !error;
}
BOOL CALLBACK _export
SpoolDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
switch (message) {
case WM_INITDIALOG:
SetWindowText(hDlg, szAppName);
return TRUE;
case WM_COMMAND:
switch (LOWORD(wParam)) {
case IDCANCEL:
error = TRUE;
DestroyWindow(hDlg);
EndDialog(hDlg, 0);
PostQuitMessage(0);
return TRUE;
}
}
return FALSE;
}
void
init_window(LPSTR cmdline)
{
LPSTR s;
char *d;
s = cmdline;
while (*s && *s == ' ')
s++;
d = port;
while (*s && *s != ' ')
*d++ = *s++;
*d = '\0';
while (*s && *s == ' ')
s++;
d = filename;
while (*s && *s != ' ')
*d++ = *s++;
*d = '\0';
lpfnSpoolProc = (DLGPROC) MakeProcInstance((FARPROC) SpoolDlgProc, phInstance);
hwndspl = CreateDialog(phInstance, "SpoolDlgBox", HWND_DESKTOP, lpfnSpoolProc);
return;
}
int PASCAL
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpszCmdLine, int cmdShow)
{
MSG msg;
phInstance = hInstance;
init_window(lpszCmdLine);
ShowWindow(hwndspl, cmdShow);
if (!spoolfile(port, filename)) {
SetWindowText(GetDlgItem(hwndspl, ID_TEXT), error_message);
while (GetMessage(&msg, (HWND) NULL, 0, 0)) {
TranslateMessage(&msg);
DispatchMessage(&msg);
}
}
DestroyWindow(hwndspl);
FreeProcInstance((FARPROC) lpfnSpoolProc);
return 0;
}