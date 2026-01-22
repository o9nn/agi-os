#include "windows_.h"
#include "errno_.h"
#include "stdio_.h"
#include "string_.h"
#include "ctype_.h"
#include "fcntl_.h"
#include <io.h>
#include "gp.h"
#include "gscdefs.h"
#include "gserrors.h"
#include "gserror.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gxiodev.h"
private iodev_proc_init(mswin_printer_init);
private iodev_proc_fopen(mswin_printer_fopen);
private iodev_proc_fclose(mswin_printer_fclose);
const gx_io_device gs_iodev_printer = {
"%printer%", "FileSystem",
{mswin_printer_init, iodev_no_open_device,
NULL  , mswin_printer_fopen, mswin_printer_fclose,
iodev_no_delete_file, iodev_no_rename_file, iodev_no_file_status,
iodev_no_enumerate_files, NULL, NULL,
iodev_no_get_params, iodev_no_put_params
}
};
typedef struct tid_s {
unsigned long tid;
} tid_t;
void mswin_printer_thread(void *arg)
{
int fd = (int)arg;
char pname[gp_file_name_sizeof];
char data[4096];
HANDLE hprinter = INVALID_HANDLE_VALUE;
int count;
DWORD written;
DOC_INFO_1 di;
if (read(fd, pname, sizeof(pname)) != sizeof(pname)) {
close(fd);
return;
}
while ( (count = read(fd, data, sizeof(data))) > 0 ) {
if (hprinter == INVALID_HANDLE_VALUE) {
if (!OpenPrinter(pname, &hprinter, NULL)) {
close(fd);
return;
}
di.pDocName = (LPTSTR)gs_product;
di.pOutputFile = NULL;
di.pDatatype = "RAW";
if (!StartDocPrinter(hprinter, 1, (LPBYTE) & di)) {
AbortPrinter(hprinter);
close(fd);
return;
}
}
if (!WritePrinter(hprinter, (LPVOID) data, count, &written)) {
AbortPrinter(hprinter);
close(fd);
return;
}
}
if (hprinter != INVALID_HANDLE_VALUE) {
if (count == 0) {
EndDocPrinter(hprinter);
ClosePrinter(hprinter);
}
else {
AbortPrinter(hprinter);
}
}
close(fd);
}
private int
mswin_printer_init(gx_io_device * iodev, gs_memory_t * mem)
{
iodev->state = gs_alloc_bytes(mem, sizeof(tid_t), "mswin_printer_init");
if (iodev->state == NULL)
return_error(gs_error_VMerror);
((tid_t *)iodev->state)->tid = -1;
return 0;
}
private int
mswin_printer_fopen(gx_io_device * iodev, const char *fname, const char *access,
FILE ** pfile, char *rfname, uint rnamelen)
{
DWORD version = GetVersion();
HANDLE hprinter;
int pipeh[2];
unsigned long tid;
HANDLE hthread;
char pname[gp_file_name_sizeof];
unsigned long *ptid = &((tid_t *)(iodev->state))->tid;
if (((HIWORD(version) & 0x8000) != 0) &&
((HIWORD(version) & 0x4000) == 0))
return_error(gs_error_invalidfileaccess);
if (!OpenPrinter((LPTSTR)fname, &hprinter, NULL))
return_error(gs_error_invalidfileaccess);
ClosePrinter(hprinter);
if (_pipe(pipeh, 4096, _O_BINARY) != 0)
return_error(gs_fopen_errno_to_code(errno));
*pfile = fdopen(pipeh[1], (char *)access);
if (*pfile == NULL) {
close(pipeh[0]);
close(pipeh[1]);
return_error(gs_fopen_errno_to_code(errno));
}
tid = _beginthread(&mswin_printer_thread, 32768, pipeh[0]);
if (tid == -1) {
fclose(*pfile);
close(pipeh[0]);
return_error(gs_error_invalidfileaccess);
}
if (!DuplicateHandle(GetCurrentProcess(), (HANDLE)tid,
GetCurrentProcess(), &hthread,
0, FALSE, DUPLICATE_SAME_ACCESS)) {
fclose(*pfile);
return_error(gs_error_invalidfileaccess);
}
*ptid = (unsigned long)hthread;
strncpy(pname, fname, sizeof(pname));
fwrite(pname, 1, sizeof(pname), *pfile);
return 0;
}
private int
mswin_printer_fclose(gx_io_device * iodev, FILE * file)
{
unsigned long *ptid = &((tid_t *)(iodev->state))->tid;
HANDLE hthread;
fclose(file);
if (*ptid != -1) {
hthread = (HANDLE)*ptid;
WaitForSingleObject(hthread, 60000);
CloseHandle(hthread);
*ptid = -1;
}
return 0;
}