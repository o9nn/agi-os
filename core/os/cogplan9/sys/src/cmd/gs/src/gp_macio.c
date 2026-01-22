#ifndef __CARBON__
#include <Palettes.h>
#include <Aliases.h>
#include <Quickdraw.h>
#include <QDOffscreen.h>
#include <AppleEvents.h>
#include <Fonts.h>
#include <Controls.h>
#include <Script.h>
#include <Timer.h>
#include <Folders.h>
#include <Resources.h>
#include <Sound.h>
#include <ToolUtils.h>
#include <Menus.h>
#include <LowMem.h>
#include <Devices.h>
#include <Scrap.h>
#include <StringCompare.h>
#include <Gestalt.h>
#include <Folders.h>
#include <Files.h>
#include <Fonts.h>
#include <FixMath.h>
#include <Resources.h>
#else
#include <Carbon.h>
#include <CoreServices.h>
#endif
#include "stdio_.h"
#include "math_.h"
#include "string_.h"
#include <stdlib.h>
#include <stdarg.h>
#include <console.h>
#include "gx.h"
#include "gp.h"
#include "gpmisc.h"
#include "gxdevice.h"
#include "gp_mac.h"
#include "stream.h"
#include "gxiodev.h"
#include "gsdll.h"
extern void
convertSpecToPath(FSSpec * s, char * p, int pLen)
{
OSStatus	err = noErr;
CInfoPBRec	params;
Str255		dirName;
int		totLen = 0, dirLen = 0;
memcpy(p, s->name + 1, s->name[0]);
totLen += s->name[0];
params.dirInfo.ioNamePtr = dirName;
params.dirInfo.ioVRefNum = s->vRefNum;
params.dirInfo.ioDrParID = s->parID;
params.dirInfo.ioFDirIndex = -1;
do {
params.dirInfo.ioDrDirID = params.dirInfo.ioDrParID;
err = PBGetCatInfoSync(&params);
if ((err != noErr) || (totLen + dirName[0] + 2 > pLen)) {
p[0] = 0;
return;
}
dirName[++dirName[0]] = ':';
memmove(p + dirName[0], p, totLen);
memcpy(p, dirName + 1, dirName[0]);
totLen += dirName[0];
} while (params.dirInfo.ioDrParID != fsRtParID);
p[totLen] = 0;
return;
}
OSErr
convertPathToSpec(const char *path, const int pathlength, FSSpec * spec)
{
Str255 filename;
if (pathlength > 254) return bdNamErr;
*filename = pathlength;
memcpy(filename + 1, path, pathlength);
return FSMakeFSSpec(0, 0, filename, spec);
}
const char gp_file_name_list_separator = ',';
const char gp_scratch_file_name_prefix[] = "tempgs_";
const char gp_null_file_name[] = "????";
extern const char gp_current_directory_name[] = ":";
int fake_stdin = 0;
void
setenv(const char * env, char *p) {
}
char *
getenv(const char * env) {
char 			*p;
FSSpec			pFile;
OSErr			err = 0;
char			fpath[256]="";
if ( strcmp(env,"GS_LIB") == 0) {
pFile.name[0] = 0;
err = FindFolder(kOnSystemDisk, kApplicationSupportFolderType, kDontCreateFolder,
&pFile.vRefNum, &pFile.parID);
if (err != noErr) goto failed;
convertSpecToPath(&pFile, fpath, 256);
p = (char*)malloc((size_t) ( 4*strlen(fpath) + 40));
sprintf(p,"%s,%sGhostscript:lib,%sGhostscript:fonts",
(char *)&fpath[0],(char *)&fpath[0],
(char *)&fpath[0] );
return p;
failed:
return NULL;
} else
return NULL;
}
private void mac_std_init(void);
private stream_proc_process(mac_stdin_read_process);
private stream_proc_process(mac_stdout_write_process);
private stream_proc_process(mac_stderr_write_process);
private stream_proc_available(mac_std_available);
private iodev_proc_init(mac_stdio_init);
const gx_io_device gs_iodev_macstdio =
{
"macstdio", "Special",
{mac_stdio_init, iodev_no_open_device,
iodev_no_open_file, iodev_no_fopen, iodev_no_fclose,
iodev_no_delete_file, iodev_no_rename_file,
iodev_no_file_status, iodev_no_enumerate_files
}
};
private int
mac_stdio_init(gx_io_device * iodev, gs_memory_t * mem)
{
mac_std_init();
return 0;
}
extern const gx_io_device gs_iodev_stdin;
private int
mac_stdin_open(gx_io_device * iodev, const char *access, stream ** ps,
gs_memory_t * mem)
{
int code = gs_iodev_stdin.procs.open_device(iodev, access, ps, mem);
stream *s = *ps;
if (code != 1)
return code;
s->procs.process = mac_stdin_read_process;
s->procs.available = mac_std_available;
s->file = NULL;
return 0;
}
extern const gx_io_device gs_iodev_stdout;
private int
mac_stdout_open(gx_io_device * iodev, const char *access, stream ** ps,
gs_memory_t * mem)
{
int code = gs_iodev_stdout.procs.open_device(iodev, access, ps, mem);
stream *s = *ps;
if (code != 1)
return code;
s->procs.process = mac_stdout_write_process;
s->procs.available = mac_std_available;
s->file = NULL;
return 0;
}
extern const gx_io_device gs_iodev_stderr;
private int
mac_stderr_open(gx_io_device * iodev, const char *access, stream ** ps,
gs_memory_t * mem)
{
int code = gs_iodev_stderr.procs.open_device(iodev, access, ps, mem);
stream *s = *ps;
if (code != 1)
return code;
s->procs.process = mac_stderr_write_process;
s->procs.available = mac_std_available;
s->file = NULL;
return 0;
}
private void
mac_std_init(void)
{
gs_findiodevice((const byte *)"%stdin", 6)->procs.open_device =
mac_stdin_open;
gs_findiodevice((const byte *)"%stdout", 7)->procs.open_device =
mac_stdout_open;
gs_findiodevice((const byte *)"%stderr", 7)->procs.open_device =
mac_stderr_open;
}
private int
mac_stdin_read_process(stream_state *st, stream_cursor_read *ignore_pr,
stream_cursor_write *pw, bool last)
{
uint count = pw->limit - pw->ptr;
if (pgsdll_callback == NULL) return EOFC;
count = (*pgsdll_callback) (GSDLL_STDIN, (char*)pw->ptr + 1, count);
pw->ptr += count;
return 1;
}
private int
mac_stdout_write_process(stream_state *st, stream_cursor_read *pr,
stream_cursor_write *ignore_pw, bool last)
{	uint count = pr->limit - pr->ptr;
if (pgsdll_callback == NULL) return EOFC;
(*pgsdll_callback) (GSDLL_STDOUT, (char *)(pr->ptr + 1), count);
pr->ptr = pr->limit;
return 0;
}
private int
mac_stderr_write_process(stream_state *st, stream_cursor_read *pr,
stream_cursor_write *ignore_pw, bool last)
{	uint count = pr->limit - pr->ptr;
if (pgsdll_callback == NULL) return EOFC;
(*pgsdll_callback) (GSDLL_STDOUT, (char *)(pr->ptr + 1), count);
pr->ptr = pr->limit;
return 0;
}
private int
mac_std_available(register stream * s, long *pl)
{
*pl = -1;
return 0;
}
FILE *
gp_open_printer (char *fname, int binary_mode)
{
if (strlen(fname) == 0)
return gp_open_scratch_file(gp_scratch_file_name_prefix, fname, binary_mode ? "wb" : "w");
else
return gp_fopen(fname, binary_mode ? "wb" : "b");
}
void
gp_close_printer (FILE *pfile, const char *fname)
{
fclose(pfile);
}
const char gp_fmode_binary_suffix[] = "b";
const char gp_fmode_rb[] = "rb";
const char gp_fmode_wb[] = "wb";
int
gp_setmode_binary(FILE *pfile, bool binary)
{	return 0;
}
FILE *
gp_open_scratch_file (const char *prefix, char fname[gp_file_name_sizeof], const char *mode)
{
char thefname[256];
Str255 thepfname;
OSErr myErr;
short foundVRefNum;
long foundDirID;
FSSpec fSpec;
FILE *f;
int prefix_length = strlen(prefix);
if (prefix_length > gp_file_name_sizeof) return NULL;
strcpy (fname, (char *) prefix);
{
char newName[50];
tmpnam (newName);
if ( prefix_length + strlen(newName) > gp_file_name_sizeof ) return NULL;
strcat (fname, newName);
}
if ( strlen(fname) > 255 ) return NULL;
if ( strrchr(fname,':') == NULL ) {
memmove((char*)&thepfname[1],(char *)&fname[0],strlen(fname));
thepfname[0]=strlen(fname);
myErr = FindFolder(kOnSystemDisk,kTemporaryFolderType,kCreateFolder,
&foundVRefNum, &foundDirID);
if ( myErr != noErr ) {
eprintf("Can't find temp folder.\n");
return (NULL);
}
FSMakeFSSpec(foundVRefNum, foundDirID,thepfname, &fSpec);
convertSpecToPath(&fSpec, thefname, sizeof(thefname) - 1);
sprintf(fname,"%s",thefname);
} else {
sprintf((char*)&thefname[0],"%s\0",fname);
memmove((char*)&thepfname[1],(char *)&thefname[0],strlen(thefname));
thepfname[0]=strlen(thefname);
}
f = gp_fopen (thefname, mode);
if (f == NULL)
eprintf1("**** Could not open temporary file %s\n", fname);
return f;
}
int
gp_read_macresource(byte *buf, const char *fname, const uint type, const ushort id)
{
Handle resource = NULL;
SInt32 size = 0;
FSSpec spec;
SInt16 fileref;
OSErr result;
result = convertPathToSpec(fname, strlen(fname), &spec);
if (result != noErr) goto fin;
fileref = FSpOpenResFile(&spec, fsRdPerm);
if (fileref == -1) goto fin;
if_debug1('s', "[s] loading resource from fileref %d\n", fileref);
resource = Get1Resource((ResType)type, (SInt16)id);
if (resource == NULL) goto fin;
size = GetMaxResourceSize(resource);
if_debug1('s', "[s] resource size on disk is %d bytes\n", size);
if (buf == NULL) goto fin;
HLock(resource);
memcpy(buf, *resource, size);
HUnlock(resource);
fin:
ReleaseResource(resource);
CloseResFile(fileref);
return (size);
}
int gp_native_fontmap(char *names[], char *paths[], int *count)
{
return 0;
}
struct file_enum_s {
char *pattern;
int first_time;
gs_memory_t *memory;
};
file_enum *
gp_enumerate_files_init (const char *pat, uint patlen, gs_memory_t *memory)
{	file_enum *pfen =
(file_enum *)gs_alloc_bytes(memory, sizeof(file_enum), "gp_enumerate_files");
char *pattern;
if ( pfen == 0 ) return 0;
pattern =
(char *)gs_alloc_bytes(memory, patlen + 1, "gp_enumerate_files(pattern)");
if ( pattern == 0 ) return 0;
memcpy(pattern, pat, patlen);
pattern[patlen] = 0;
pfen->pattern = pattern;
pfen->memory = memory;
pfen->first_time = 1;
return pfen;
}
uint
gp_enumerate_files_next (file_enum *pfen, char *ptr, uint maxlen)
{	if ( pfen->first_time )
{	pfen->first_time = 0;
}
return -1;
}
void
gp_enumerate_files_close (file_enum *pfen)
{
gs_free_object(pfen->memory, pfen->pattern, "gp_enumerate_files_close(pattern)");
gs_free_object(pfen->memory, (char *)pfen, "gp_enumerate_files_close");
}
FILE *
gp_fopen (const char * fname, const char * mode) {
char thefname[256];
FILE *fid;
if ( strrchr(fname,':') == NULL )
sprintf((char *)&thefname[0],"%s%s\0","",fname);
else
sprintf((char*)&thefname[0],"%s\0",fname);
fid = fopen(thefname,mode);
return fid;
}
FILE *
popen (const char * fname, const char * mode ) {
return gp_fopen (fname,  mode);
}
int
pclose (FILE * pipe ) {
return fclose (pipe);
}
#ifdef __CARBON__
static int compare_UniStr(HFSUniStr255 u, const char *c, uint len)
{
int i,searchlen,unichar;
searchlen = min(len,u.length);
for (i = 0; i < searchlen; i++) {
unichar = u.unicode[i];
if (unichar & !0xFF) return -1;
if (unichar != c[i]) break;
}
return (i == u.length) ? i : 0;
}
uint gp_file_name_root(const char *fname, uint len)
{
OSErr err = noErr;
HFSUniStr255 volumeName;
FSRef rootDirectory;
int index, match;
if (len > 0 && fname[0] == ':')
return 0;
index = 1;
while (err == noErr) {
err = FSGetVolumeInfo (kFSInvalidVolumeRefNum, index,
NULL, kFSVolInfoNone, NULL,
&volumeName, &rootDirectory);
if (err == nsvErr) return 0;
if (err == noErr) {
match = compare_UniStr(volumeName, fname, len);
if (match > 0) {
if (fname[match] == ':') return match + 1;
return match;
}
}
index++;
}
return 0;
}
#else
uint gp_file_name_root(const char *fname, uint len)
{
return 0;
}
#endif
uint gs_file_name_check_separator(const char *fname, int len, const char *item)
{   if (len > 0) {
if (fname[0] == ':') {
if (fname == item + 1 && item[0] == ':')
return 1;
if (len > 1 && fname[1] == ':')
return 0;
return 1;
}
} else if (len < 0) {
if (fname[-1] == ':')
return 1;
}
return 0;
}
bool gp_file_name_is_parent(const char *fname, uint len)
{   return len == 1 && fname[0] == ':';
}
bool gp_file_name_is_current(const char *fname, uint len)
{   return (len == 0) || (len == 1 && fname[0] == ':');
}
const char *gp_file_name_separator(void)
{   return ":";
}
const char *gp_file_name_directory_separator(void)
{   return ":";
}
const char *gp_file_name_parent(void)
{   return "::";
}
const char *gp_file_name_current(void)
{   return ":";
}
bool gp_file_name_is_partent_allowed(void)
{   return true;
}
bool gp_file_name_is_empty_item_meanful(void)
{   return true;
}
gp_file_name_combine_result
gp_file_name_combine(const char *prefix, uint plen, const char *fname, uint flen,
bool no_sibling, char *buffer, uint *blen)
{
return gp_file_name_combine_generic(prefix, plen,
fname, flen, no_sibling, buffer, blen);
}
static char *MacStr2c(char *pstring)
{
char *cstring;
int len = (pstring[0] < 256) ? pstring[0] : 255;
if (len == 0) return NULL;
cstring = malloc(len + 1);
if (cstring != NULL) {
memcpy(cstring, &(pstring[1]), len);
cstring[len] = '\0';
}
return(cstring);
}
typedef struct {
int size, style, id;
} fond_entry;
typedef struct {
int entries;
fond_entry *refs;
} fond_table;
static fond_table *fond_table_new(int entries)
{
fond_table *table = malloc(sizeof(fond_table));
if (table != NULL) {
table->entries = entries;
table->refs = malloc(entries * sizeof(fond_entry));
if (table->refs == NULL) { free(table); table = NULL; }
}
return table;
}
static void fond_table_free(fond_table *table)
{
if (table != NULL) {
if (table->refs) free(table->refs);
free(table);
}
}
static fond_table *fond_table_grow(fond_table *table, int entries)
{
if (table == NULL) {
table = fond_table_new(entries);
} else {
table->entries += entries;
table->refs = realloc(table->refs, table->entries * sizeof(fond_entry));
}
return table;
}
static int get_int16(unsigned char *p) {
return (p[0]&0xFF)<<8 | (p[1]&0xFF);
}
static int get_int32(unsigned char *p) {
return (p[0]&0xFF)<<24 | (p[1]&0xFF)<<16 | (p[2]&0xFF)<<8 | (p[3]&0xFF);
}
static fond_table * parse_fond(FSSpec *spec)
{
OSErr result = noErr;
FSRef specref;
SInt16 ref;
Handle fond = NULL;
unsigned char *res;
fond_table *table = NULL;
int i,j, count, n, start;
result = FSpMakeFSRef(spec,&specref);
#ifdef __CARBON__
if (result == noErr)
result = FSOpenResourceFile(&specref, 0, NULL, fsRdPerm, &ref);
#else
result = bdNamErr;
#endif
if (result != noErr) {
ref = FSpOpenResFile(spec, fsRdPerm);
result = ResError();
}
if (result != noErr || ref <= 0) {
char path[256];
convertSpecToPath(spec, path, 256);
dlprintf2("unable to open resource file '%s' for font enumeration (error %d)\n",
path, result);
goto fin;
}
start = 0;
UseResFile(ref);
count = Count1Resources('FOND');
for (i = 0; i < count; i++) {
fond = Get1IndResource('FOND', i+1);
if (fond == NULL) {
result = ResError();
goto fin;
}
HLock(fond);
res = *fond + 52;
n = get_int16(res) + 1;	res += 2;
table = fond_table_grow(table, n);
for (j = start; j < start + n; j++ ) {
table->refs[j].size = get_int16(res); res += 2;
table->refs[j].style = get_int16(res); res += 2;
table->refs[j].id = get_int16(res); res += 2;
}
start += n;
HUnlock(fond);
}
fin:
CloseResFile(ref);
return table;
}
static int is_ttf_file(const char *path)
{
int len = strlen(path);
return !memcmp(path+len-4,".ttf",4);
}
static int is_otf_file(const char *path)
{
int len = strlen(path);
return !memcmp(path+len-4,".otf",4);
}
static void strip_char(char *string, int len, const int c)
{
char *bit;
len += 1;
while(bit = strchr(string,' ')) {
memmove(bit, bit + 1, string + len - bit - 1);
}
}
static char *makePSFontName(FMFontFamily Family, FMFontStyle Style)
{
Str255 Name;
OSStatus result;
int length;
char *stylename, *fontname;
char *psname;
result = FMGetFontFamilyName(Family, Name);
if (result != noErr) return NULL;
fontname = MacStr2c(Name);
if (fontname == NULL) return NULL;
strip_char(fontname, strlen(fontname), ' ');
switch (Style) {
case 0: stylename=""; break;;
case 1: stylename="Bold"; break;;
case 2: stylename="Italic"; break;;
case 3: stylename="BoldItalic"; break;;
default: stylename="Unknown"; break;;
}
length = strlen(fontname) + strlen(stylename) + 2;
psname = malloc(length);
if (Style != 0)
snprintf(psname, length, "%s-%s", fontname, stylename);
else
snprintf(psname, length, "%s", fontname);
free(fontname);
return psname;
}
typedef struct {
int count;
FMFontIterator Iterator;
char *name;
char *path;
FSSpec last_container;
char *last_container_path;
fond_table *last_table;
} fontenum_t;
void *gp_enumerate_fonts_init(gs_memory_t *mem)
{
fontenum_t *state = gs_alloc_bytes(mem, sizeof(fontenum_t),
"macos font enumerator state");
FMFontIterator *Iterator = &state->Iterator;
OSStatus result;
if (state != NULL) {
state->count = 0;
state->name = NULL;
state->path = NULL;
result = FMCreateFontIterator(NULL, NULL,
kFMLocalIterationScope, Iterator);
if (result != noErr) return NULL;
memset(&state->last_container, 0, sizeof(FSSpec));
state->last_container_path = NULL;
state->last_table = NULL;
}
return (void *)state;
}
void gp_enumerate_fonts_free(void *enum_state)
{
fontenum_t *state = (fontenum_t *)enum_state;
FMFontIterator *Iterator = &state->Iterator;
FMDisposeFontIterator(Iterator);
if (state->name) free(state->name);
if (state->path) free(state->path);
if (state->last_container_path) free(state->last_container_path);
if (state->last_table) fond_table_free(state->last_table);
}
int gp_enumerate_fonts_next(void *enum_state, char **fontname, char **path)
{
fontenum_t *state = (fontenum_t *)enum_state;
FMFontIterator *Iterator = &state->Iterator;
FMFont Font;
FourCharCode Format;
FMFontFamily FontFamily;
FMFontStyle Style;
FSSpec FontContainer;
char type[5];
char fontpath[256];
char *psname;
fond_table *table = NULL;
OSStatus result;
result = FMGetNextFont(Iterator, &Font);
if (result != noErr) return 0;
result = FMGetFontFormat(Font, &Format);
type[0] = ((char*)&Format)[0];
type[1] = ((char*)&Format)[1];
type[2] = ((char*)&Format)[2];
type[3] = ((char*)&Format)[3];
type[4] = '\0';
FMGetFontFamilyInstanceFromFont(Font, &FontFamily, &Style);
if (state->name) free (state->name);
psname = makePSFontName(FontFamily, Style);
if (psname == NULL) {
state->name = strdup("GSPlaceHolder");
} else {
state->name = psname;
}
result = FMGetFontContainer(Font, &FontContainer);
if (!memcmp(&FontContainer, &state->last_container, sizeof(FSSpec))) {
strncpy(fontpath, state->last_container_path, 256);
table = state->last_table;
} else {
convertSpecToPath(&FontContainer, fontpath, 256);
if (!is_ttf_file(fontpath) && !is_otf_file(fontpath))
table = parse_fond(&FontContainer);
memcpy(&state->last_container, &FontContainer, sizeof(FSSpec));
if (state->last_container_path) free (state->last_container_path);
state->last_container_path = strdup(fontpath);
if (state->last_table) fond_table_free(state->last_table);
state->last_table = table;
}
if (state->path) {
free(state->path);
state->path = NULL;
}
if (table != NULL) {
int i;
for (i = 0; i < table->entries; i++) {
if (table->refs[i].size == 0) {
if (table->refs[i].style == Style) {
int len = strlen(fontpath) + strlen("%macresource%#sfnt+") + 6;
state->path = malloc(len);
snprintf(state->path, len, "%%macresource%%%s#sfnt+%d",
fontpath, table->refs[i].id);
break;
}
}
}
} else {
state->path = strdup(fontpath);
}
if (state->path == NULL) {
int len = strlen(fontpath) + strlen("%macresource%#POST") + 1;
state->path = malloc(len);
snprintf(state->path, len, "%%macresource%%%s#POST", fontpath);
}
#ifdef DEBUG
dlprintf2("fontenum: returning '%s' in '%s'\n", state->name, state->path);
#endif
*fontname = state->name;
*path = state->path;
state->count += 1;
return 1;
}