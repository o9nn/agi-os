#include "errno_.h"
#include "string_.h"
#include "unistd_.h"
#include "gx.h"
#include "gserrors.h"
#include "gp.h"
#include "gscdefs.h"
#include "gsparam.h"
#include "gsstruct.h"
#include "gxiodev.h"
#include "gsutil.h"
private iodev_proc_init(iodev_diskn_init);
private iodev_proc_fopen(iodev_diskn_fopen);
private iodev_proc_delete_file(diskn_delete);
private iodev_proc_rename_file(diskn_rename);
private iodev_proc_file_status(diskn_status);
private iodev_proc_enumerate_files(diskn_enumerate_files);
private iodev_proc_enumerate_next(diskn_enumerate_next);
private iodev_proc_enumerate_close(diskn_enumerate_close);
private iodev_proc_get_params(diskn_get_params);
private iodev_proc_put_params(diskn_put_params);
iodev_proc_put_params(diskn_os_put_params);
#define diskn(varname,diskname) \
const gx_io_device varname = \
{ \
diskname, "FileSystem", \
{iodev_diskn_init, iodev_no_open_device, \
NULL  , iodev_diskn_fopen, iodev_os_fclose, \
diskn_delete, diskn_rename, diskn_status, \
iodev_no_enumerate_files,  \
diskn_enumerate_next, diskn_enumerate_close, \
diskn_get_params, diskn_put_params \
} \
}
diskn(gs_iodev_disk0,"%disk0%");
diskn(gs_iodev_disk1,"%disk1%");
diskn(gs_iodev_disk2,"%disk2%");
diskn(gs_iodev_disk3,"%disk3%");
diskn(gs_iodev_disk4,"%disk4%");
diskn(gs_iodev_disk5,"%disk5%");
diskn(gs_iodev_disk6,"%disk6%");
#undef diskn
typedef struct diskn_state_s {
int root_size;
char * root;
gs_memory_t * memory;
} diskn_state;
gs_private_st_ptrs1(st_diskn_state, struct diskn_state_s, "diskn_state",
diskn_state_enum_ptrs, diskn_state_reloc_ptrs, root);
#define MAP_FILE_NAME "map.txt"
#define TEMP_FILE_NAME "Tmp.txt"
#define MAP_FILE_VERSION 1
#define InitialNumber 0
#define BUFFER_LENGTH gp_file_name_sizeof
typedef struct map_file_enum_s {
FILE *  stream;
char *  pattern;
char *  root;
gs_memory_t * memory;
} map_file_enum;
gs_private_st_ptrs2(st_map_file_enum, struct map_file_enum_s, "map_file_enum",
map_file_enum_enum_ptrs, map_file_enum_reloc_ptrs, pattern, root);
private void * map_file_enum_init(gs_memory_t *, const char *, const char *);
private bool map_file_enum_next(void *, char *);
private void map_file_enum_close(void *);
private bool map_file_name_get(const char *, const char *, char *);
private void map_file_name_add(const char *, const char *);
private void map_file_name_ren(const char*, const char *, const char *);
private void map_file_name_del(const char *, const char *);
private int
iodev_diskn_init(gx_io_device * iodev, gs_memory_t * mem)
{
diskn_state * pstate = gs_alloc_struct(mem, diskn_state, &st_diskn_state,
"iodev_diskn_init(state)");
if (!pstate)
return gs_error_VMerror;
pstate->root_size = 0;
pstate->root = NULL;
pstate->memory = mem;
iodev->state = pstate;
return 0;
}
private int
iodev_diskn_fopen(gx_io_device * iodev, const char *fname, const char *access,
FILE ** pfile, char *rfname, uint rnamelen)
{
char realname[gp_file_name_sizeof];
diskn_state * pstate = (diskn_state *)iodev->state;
if (!pstate->root)
return_error(gs_error_undefinedfilename);
if (!map_file_name_get((char *)pstate->root, fname, realname)) {
if (strchr(access, 'w')) {
map_file_name_add(pstate->root, fname);
map_file_name_get(pstate->root, fname, realname);
}
else
return_error(gs_error_undefinedfilename);
}
return iodev_os_fopen(iodev_default, realname, access, pfile, rfname, rnamelen);
}
private int
diskn_delete(gx_io_device * iodev, const char *fname)
{
char realname[gp_file_name_sizeof];
diskn_state * pstate = (diskn_state *)iodev->state;
if (!pstate->root)
return_error(gs_error_undefinedfilename);
if (!map_file_name_get((char *)pstate->root, fname, realname))
return_error(gs_error_undefinedfilename);
map_file_name_del((char *)pstate->root, fname);
return (unlink(realname) == 0 ? 0 : gs_error_ioerror);
}
private int
diskn_rename(gx_io_device * iodev, const char *from, const char *to)
{
char toreal[gp_file_name_sizeof];
int code = 0;
diskn_state * pstate = (diskn_state *)iodev->state;
if (!pstate->root)
return_error(gs_error_undefinedfilename);
if (strcmp(to, from) == 0)
return 0;
if (map_file_name_get((char *)pstate->root, to, toreal)) {
map_file_name_del((char *)pstate->root, to);
code = unlink(toreal) == 0 ? 0 : gs_error_ioerror;
}
map_file_name_ren((char *)pstate->root, from, to);
return code;
}
private int
diskn_status(gx_io_device * iodev, const char *fname, struct stat *pstat)
{
char realname[gp_file_name_sizeof];
diskn_state * pstate = (diskn_state *)iodev->state;
if (!pstate->root)
return_error(gs_error_undefinedfilename);
if (!map_file_name_get((char *)pstate->root, fname, realname))
return_error(gs_error_undefinedfilename);
return (stat((char *)realname, pstat) < 0 ? gs_error_undefinedfilename : 0);
}
private file_enum *
diskn_enumerate_files_init(gx_io_device * iodev, const char *pat, uint patlen,
gs_memory_t * mem)
{
char patstr[gp_file_name_sizeof];
diskn_state * pstate = (diskn_state *)iodev->state;
memcpy(patstr, pat, patlen);
patstr[patlen]=0;
return (file_enum *)map_file_enum_init(mem, (char *)pstate->root, patstr);
}
private void
diskn_enumerate_close(file_enum *pfen)
{
map_file_enum_close((void *)pfen);
}
private uint
diskn_enumerate_next(file_enum *pfen, char *ptr, uint maxlen)
{
if (map_file_enum_next((void *)pfen, ptr))
return strlen(ptr);
diskn_enumerate_close(pfen);
return ~(uint) 0;
}
private int
diskn_get_params(gx_io_device * iodev, gs_param_list * plist)
{
int code;
int i0 = 0, so = 1;
bool btrue = true, bfalse = false;
diskn_state * pstate = (diskn_state *)iodev->state;
bool bsearch = pstate->root != 0;
int BlockSize;
long Free, LogicalSize;
gs_param_string rootstring;
BlockSize = 1024;
LogicalSize = bsearch ? 2000000000 / BlockSize : 0;
Free = LogicalSize * 3 / 4;
if (
(code = param_write_bool(plist, "HasNames", &btrue)) < 0 ||
(code = param_write_int(plist, "BlockSize", &BlockSize)) < 0 ||
(code = param_write_long(plist, "Free", &Free)) < 0 ||
(code = param_write_int(plist, "InitializeAction", &i0)) < 0 ||
(code = param_write_bool(plist, "Mounted", &bsearch)) < 0 ||
(code = param_write_bool(plist, "Removable", &bfalse)) < 0 ||
(code = param_write_bool(plist, "Searchable", &bsearch)) < 0 ||
(code = param_write_int(plist, "SearchOrder", &so)) < 0 ||
(code = param_write_bool(plist, "Writeable", &bsearch)) < 0 ||
(code = param_write_long(plist, "LogicalSize", &LogicalSize)) < 0
)
return code;
if (pstate->root) {
rootstring.data = (const byte *)pstate->root;
rootstring.size = strlen(pstate->root);
rootstring.persistent = false;
return param_write_string(plist, "Root", &rootstring);
}
else {
return param_write_null(plist, "Root");
}
}
private int
diskn_put_params(gx_io_device *iodev, gs_param_list *plist)
{
gs_param_string rootstr;
int code;
diskn_state * pstate = (diskn_state *)iodev->state;
switch (code = param_read_string(plist, "Root", &rootstr)) {
case 0:
break;
default:
param_signal_error(plist, "Root", code);
case 1:
rootstr.data = 0;
break;
}
code = iodev_no_put_params(iodev, plist);
if (code < 0)
return code;
if (rootstr.data) {
if (!pstate->root || pstate->root_size <= rootstr.size) {
if (pstate->root)
gs_free_object(pstate->memory, pstate->root, "diskn(rootdir)");
pstate->root = (char *)gs_alloc_byte_array(pstate->memory,
gp_file_name_sizeof, sizeof(char), "diskn(rootdir)");
if (!pstate->root)
return gs_error_VMerror;
pstate->root_size = rootstr.size + 1;
iodev->procs.enumerate_files = diskn_enumerate_files_init;
}
memcpy(pstate->root, rootstr.data, rootstr.size);
pstate->root[rootstr.size] = 0;
}
return 0;
}
private FILE *
MapFileOpen(const char * rootpath, const char * filename, const char * attributes)
{
char fullname[BUFFER_LENGTH];
if (strlen(rootpath) + strlen(filename) >= BUFFER_LENGTH)
return NULL;
sprintf(fullname, "%s%s", rootpath, filename);
return gp_fopen(fullname, attributes);
}
private int
MapFileReadVersion(FILE * mapfile, int * value)
{
int code = fscanf(mapfile, "FileVersion\t%d\t", value) == 1 ? 1 : 0;
int c;
do {
c = fgetc(mapfile);
} while (c != EOF && c != '\n' && c != '\r');
while (c != EOF && (c == '\n' || c == '\r')) {
c = fgetc(mapfile);
}
return code;
}
private void
MapFileWriteVersion(FILE * mapfile, int value)
{
fprintf(mapfile,
"FileVersion\t%d\tThis file is machine generated.  Do not edit.\n",
value);
}
private int
MapFileRead(FILE * mapfile, char * namebuf, int * value)
{
int count = 0;
int c;
if (fscanf(mapfile, "%d\t", value) != 1)
return 0;
do {
namebuf[count++] = c = fgetc(mapfile);
} while (count < BUFFER_LENGTH && c != EOF && c != '\n' && c != '\r');
namebuf[--count] = 0;
while (c != EOF && (c == '\n' || c == '\r')) {
c = fgetc(mapfile);
}
return count != 0 ? 1: 0;
}
private void
MapFileWrite(FILE * mapfile, const char * namebuf, int value)
{
fprintf(mapfile, " %d\t%s\n", value, namebuf);
}
private void
MapFileUnlink(const char * rootpath, const char * filename)
{
char fullname[BUFFER_LENGTH];
if (strlen(rootpath) + strlen(filename) >= BUFFER_LENGTH)
return;
sprintf(fullname, "%s%s", rootpath, filename);
unlink(fullname);
}
private void
MapFileRename(const char * rootpath, const char * newfilename, const char * oldfilename)
{
char oldfullname[BUFFER_LENGTH];
char newfullname[BUFFER_LENGTH];
if (strlen(rootpath) + strlen(oldfilename) >= BUFFER_LENGTH)
return;
if (strlen(rootpath) + strlen(newfilename) >= BUFFER_LENGTH)
return;
sprintf(oldfullname, "%s%s", rootpath, oldfilename);
sprintf(newfullname, "%s%s", rootpath, newfilename);
rename(oldfullname, newfullname);
}
private int
MapToFile(const char* rootpath, const char* name)
{
FILE * mapfile;
int d = -1;
char filename[BUFFER_LENGTH];
int file_version;
mapfile = MapFileOpen(rootpath, MAP_FILE_NAME, "r");
if (mapfile == NULL)
return -1;
if (MapFileReadVersion(mapfile, &file_version)
&& file_version == MAP_FILE_VERSION) {
while (MapFileRead(mapfile, filename, &d)) {
if (strcmp(filename, name) == 0)
break;
d = -1;
}
}
fclose(mapfile);
return d;
}
private void *
map_file_enum_init(gs_memory_t * mem, const char * root_name, const char * search_pattern)
{
int file_version;
map_file_enum * mapfileenum = gs_alloc_struct(mem, map_file_enum, &st_map_file_enum,
"diskn:enum_init(file_enum)");
if (mapfileenum == NULL)
return NULL;
memset(mapfileenum, 0, sizeof(map_file_enum));
mapfileenum->memory = mem;
if (search_pattern) {
mapfileenum->pattern = (char *)gs_alloc_bytes(mem, strlen(search_pattern) + 1,
"diskn:enum_init(pattern)");
if (mapfileenum->pattern == NULL) {
map_file_enum_close((file_enum *) mapfileenum);
return NULL;
}
strcpy(mapfileenum->pattern, search_pattern);
}
mapfileenum->root = (char *)gs_alloc_bytes(mem, strlen(root_name) + 1,
"diskn:enum_init(root)");
if (mapfileenum->root == NULL) {
map_file_enum_close((file_enum *) mapfileenum);
return NULL;
}
if (strlen(root_name) >= BUFFER_LENGTH)
return NULL;
strcpy(mapfileenum->root, root_name);
mapfileenum->stream = MapFileOpen(root_name, MAP_FILE_NAME, "r");
if (mapfileenum->stream != NULL
&& (!MapFileReadVersion(mapfileenum->stream, &file_version)
|| file_version != MAP_FILE_VERSION)) {
fclose(mapfileenum->stream);
mapfileenum->stream = NULL;
}
return mapfileenum;
}
private bool
map_file_enum_next(void * enum_mem, char* target)
{
int d = -1;
map_file_enum * mapfileenum;
if (enum_mem == NULL)
return false;
mapfileenum = (map_file_enum*)enum_mem;
if (mapfileenum->stream == NULL)
return false;
if (mapfileenum->pattern) {
while (MapFileRead(mapfileenum->stream, target, &d)) {
if (string_match((byte *)target, strlen(target),
(byte *)mapfileenum->pattern,
strlen(mapfileenum->pattern), 0))
return true;
}
}
else {
if (MapFileRead(mapfileenum->stream, target, &d))
return true;
}
return false;
}
private void
map_file_enum_close(void * enum_mem)
{
map_file_enum * mapfileenum = (map_file_enum *) enum_mem;
gs_memory_t * mem = mapfileenum->memory;
if (mapfileenum->stream)
fclose(mapfileenum->stream);
if (mapfileenum->root)
gs_free_object(mem, mapfileenum->root, "diskn_enum_init(root)");
if (mapfileenum->pattern)
gs_free_object(mem, mapfileenum->pattern, "diskn_enum_init(pattern)");
gs_free_object(mem, mapfileenum, "diskn_enum_init(mapfileenum)");
}
private bool
map_file_name_get(const char * root_name, const char * Fname, char * osname)
{
int d = MapToFile(root_name, Fname);
if (d != -1) {
if ((strlen(root_name) + 20) < BUFFER_LENGTH) {
sprintf(osname, "%s%d", root_name, d);
return true;
}
}
*osname = 0;
return false;
}
private void
map_file_name_del(const char * root_name, const char * Fname)
{
int d = MapToFile(root_name, Fname);
int file_version;
if (d != -1) {
char    name[BUFFER_LENGTH];
FILE*   newMap;
FILE*   oldMap;
MapFileUnlink(root_name, TEMP_FILE_NAME );
newMap = MapFileOpen(root_name, TEMP_FILE_NAME, "w");
if (newMap == NULL)
return;
oldMap = MapFileOpen(root_name, MAP_FILE_NAME, "r");
if (oldMap != NULL && (!MapFileReadVersion(oldMap, &file_version)
|| file_version != MAP_FILE_VERSION)) {
fclose(oldMap);
oldMap= NULL;
}
if (oldMap == NULL) {
fclose(newMap);
MapFileUnlink(root_name, TEMP_FILE_NAME);
return;
}
MapFileWriteVersion(newMap, MAP_FILE_VERSION);
while (MapFileRead(oldMap, name, &d))
if (strcmp(name, Fname))
MapFileWrite(newMap, name, d);
fclose(newMap);
fclose(oldMap);
MapFileUnlink(root_name, MAP_FILE_NAME);
MapFileRename(root_name, MAP_FILE_NAME, TEMP_FILE_NAME);
}
}
private void
map_file_name_add(const char * root_name, const char * Fname)
{
char    name[BUFFER_LENGTH];
int d;
int dmax = -1;
int file_version;
FILE*   newMap;
FILE*   oldMap;
oldMap = MapFileOpen(root_name, MAP_FILE_NAME, "r");
if (oldMap != NULL && (!MapFileReadVersion(oldMap, &file_version)
|| file_version != MAP_FILE_VERSION)) {
fclose(oldMap);
oldMap = NULL;
}
if (oldMap == NULL) {
oldMap = MapFileOpen(root_name, MAP_FILE_NAME, "w");
if (!oldMap)
return;
MapFileWriteVersion(oldMap, MAP_FILE_VERSION);
MapFileWrite(oldMap, Fname, InitialNumber);
fclose(oldMap);
}
else {
MapFileUnlink(root_name, TEMP_FILE_NAME);
newMap = MapFileOpen(root_name, TEMP_FILE_NAME, "w");
if (newMap != NULL) {
MapFileWriteVersion(newMap, MAP_FILE_VERSION);
while (MapFileRead(oldMap, name, &d)) {
MapFileWrite(newMap, name, d);
if (dmax < d)
dmax = d;
}
dmax += 1;
MapFileWrite(newMap, Fname, dmax);
fclose(newMap);
fclose(oldMap);
MapFileUnlink(root_name, MAP_FILE_NAME);
MapFileRename(root_name, MAP_FILE_NAME, TEMP_FILE_NAME);
}
}
}
private void
map_file_name_ren(const char* root_name, const char * oldname, const char * newname)
{
int d = MapToFile(root_name, oldname);
int file_version;
if (d != -1) {
char    name[BUFFER_LENGTH];
FILE*   newMap;
FILE*   oldMap;
MapFileUnlink(root_name, TEMP_FILE_NAME );
newMap = MapFileOpen(root_name, TEMP_FILE_NAME, "w");
if (newMap == NULL)
return;
oldMap = MapFileOpen(root_name, MAP_FILE_NAME, "r");
if (oldMap != NULL && (!MapFileReadVersion(oldMap, &file_version)
|| file_version != MAP_FILE_VERSION)) {
fclose(oldMap);
oldMap= NULL;
}
if (oldMap == NULL) {
fclose(newMap);
MapFileUnlink(root_name, TEMP_FILE_NAME);
return;
}
MapFileWriteVersion(newMap, MAP_FILE_VERSION);
while (MapFileRead(oldMap, name, &d))
if (strcmp(name, oldname))
MapFileWrite(newMap, name, d);
else
MapFileWrite(newMap, newname, d);
fclose(newMap);
fclose(oldMap);
MapFileUnlink(root_name, MAP_FILE_NAME);
MapFileRename(root_name, MAP_FILE_NAME, TEMP_FILE_NAME);
}
}