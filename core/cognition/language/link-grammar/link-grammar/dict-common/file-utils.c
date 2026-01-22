#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifndef _MSC_VER
#include <unistd.h>
#else
#include <windows.h>
#include <Shlwapi.h>
#include <direct.h>
#endif
#include <stdlib.h>
#include <string.h>
#include "file-utils.h"
#include "error.h"
#include "link-includes.h"
#include "utilities.h"
#ifdef _WIN32
#define DIR_SEPARATOR "\\"
#else
#define DIR_SEPARATOR "/"
#endif
#ifndef DICTIONARY_DIR
#define DICTIONARY_DIR NULL
#endif
#define MAX_PATH_NAME 200
char *find_last_dir_separator(char *path)
{
char *dirsep = NULL;
size_t pathlen = strlen(path);
for (size_t p = pathlen; p > 0; p--)
if (('/' == path[p]) || ('\\' == path[p])) return &path[p];
return dirsep;
}
char * join_path(const char * prefix, const char * suffix)
{
char * path;
size_t path_len, prel;
path_len = strlen(prefix) + 1  + strlen(suffix);
path = (char *) malloc(path_len + 1);
strcpy(path, prefix);
prel = strlen(path);
if (0 < prel && (path[prel-1] != '/') && (path[prel-1] != '\\'))
{
path[prel] = '/';
path[prel+1] = '\0';
}
strcat(path, suffix);
return path;
}
static char * custom_data_dir = NULL;
static void free_custom_data_dir(void) {
free(custom_data_dir);
}
void dictionary_set_data_dir(const char * path)
{
if (custom_data_dir)
free(custom_data_dir);
else
atexit(free_custom_data_dir);
custom_data_dir = safe_strdup(path);
}
char * dictionary_get_data_dir(void)
{
char * data_dir = NULL;
if (custom_data_dir != NULL) {
data_dir = safe_strdup(custom_data_dir);
return data_dir;
}
return NULL;
}
#ifdef _MSC_VER
static const char *get_dictionary_dir(bool);
static void free_dictionary_dir(void)
{
get_dictionary_dir(false);
}
#endif
static const char *get_dictionary_dir(bool find)
{
#ifndef _MSC_VER
return DICTIONARY_DIR;
#else
static const char *dictionary_dir;
if (!find) {
free((void *)dictionary_dir);
return NULL;
}
if (NULL != dictionary_dir) return dictionary_dir;
dictionary_dir = DICTIONARY_DIR;
if ((NULL == dictionary_dir) || ('\0' == dictionary_dir[1]))
dictionary_dir = ".";
if (0 == strncmp(dictionary_dir, "\\\\", 2)) return DICTIONARY_DIR;
if (0 == strncmp(dictionary_dir, "
if ((strlen(dictionary_dir) > 2) && (':' == dictionary_dir[1]))
return DICTIONARY_DIR;
char dll_path[MAX_PATH_NAME] = "";
HMODULE dll_hm = NULL;
if (!GetModuleHandleEx(GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS |
GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT,
(LPCSTR) &get_dictionary_dir, &dll_hm))
{
prt_error("Warning: GetModuleHandleEx error %d\n", (int)GetLastError());
return dictionary_dir;
}
if (!GetModuleFileNameA(dll_hm, dll_path, sizeof(dll_path)))
{
prt_error("Warning: GetModuleFileNameA error %d\n", (int)GetLastError());
return dictionary_dir;
}
if ('\0' == dll_path[0])
{
prt_error("Warning: GetModuleFileNameA didn't return a path!\n");
return dictionary_dir;
}
if (!PathRemoveFileSpecA(dll_path))
{
prt_error("Warning: Cannot get directory from LG DLL path '%s'!\n",
dll_path);
return dictionary_dir;
}
if (NULL != strchr(dll_path, '?'))
{
prt_error("Warning: Directory of LG DLL (%s) "
"contains unsupported characters\n", dll_path);
return dictionary_dir;
}
if (strlen(dll_path) < 3)
{
prt_error("Warning: DLL directory name '%s' too short!\n", dll_path);
return dictionary_dir;
}
lgdebug(D_USER_FILES, "Debug: Directory of LG DLL: %s\n", dll_path);
char *combined_dictionary_dir;
if (('\\' == dictionary_dir[0]) || ('/' == dictionary_dir[0]))
{
size_t prefix_len = 0;
if (dll_path[1] == ':')
{
prefix_len = 2;
}
else
{
const char *hostend = strchr(dll_path+3, '\\');
if (NULL == hostend)
hostend = strchr(dll_path+3, '/');
if (NULL != hostend)
prefix_len = (size_t)(hostend - dll_path);
}
size_t len = prefix_len + strlen(dictionary_dir) + 1;
combined_dictionary_dir = malloc(len);
strncpy(combined_dictionary_dir, dll_path, prefix_len);
strcpy(combined_dictionary_dir + prefix_len, dictionary_dir);
}
else
{
size_t len = strlen(dll_path)+1+strlen(dictionary_dir)+1;
combined_dictionary_dir = malloc(len);
strcpy(combined_dictionary_dir, dll_path);
strcat(combined_dictionary_dir, "\\");
strcat(combined_dictionary_dir, dictionary_dir);
}
dictionary_dir = combined_dictionary_dir;
atexit(free_dictionary_dir);
lgdebug(D_USER_FILES, "Debug: Using dictionary directory '%s'\n",
dictionary_dir);
return dictionary_dir;
#endif
}
static void *dict_file_open(const char *fullname, const void *how)
{
return fopen(fullname, how);
}
#define NOTFOUND(fp) ((NULL == (fp)) ? " (Not found)" : "")
void * object_open(const char *filename,
void * (*opencb)(const char *, const void *),
const void * user_data)
{
static TLS char *path_found;
char *completename = NULL;
void *fp = NULL;
char *data_dir = NULL;
const char *dictionary_dir = NULL;
const char **path = NULL;
if (NULL == filename)
{
char *pf = path_found;
path_found = NULL;
free(pf);
return NULL;
}
if (NULL == path_found)
{
dictionary_dir = get_dictionary_dir(true);
data_dir = dictionary_get_data_dir();
if (verbosity_level(D_USER_FILES))
{
char cwd[MAX_PATH_NAME];
char *cwdp = getcwd(cwd, sizeof(cwd));
prt_error("Debug: Current directory: %s\n", NULL == cwdp ? "NULL": cwdp);
prt_error("Debug: Data directory: %s\n",
data_dir ? data_dir : "NULL");
prt_error("Debug: System data directory: %s\n",
dictionary_dir ? dictionary_dir : "NULL");
}
}
if ((filename[0] == '/')
#ifdef _WIN32
|| ((filename[1] == ':')
&& ((filename[2] == '\\') || (filename[2] == '/')))
|| (filename[0] == '\\')
#endif
)
{
fp = opencb(filename, user_data);
lgdebug(D_USER_FILES, "Debug: Opening file %s%s\n", filename, NOTFOUND(fp));
}
else
{
const char *dictpath[] =
{
path_found,
".",
"./data",
"..",
"../data",
data_dir,
dictionary_dir,
};
size_t i = sizeof(dictpath)/sizeof(dictpath[0]);
for (path = dictpath; i-- > 0; path++)
{
if (NULL == *path) continue;
free(completename);
completename = join_path(*path, filename);
fp = opencb(completename, user_data);
lgdebug(D_USER_FILES, "Debug: Opening file %s%s\n", completename, NOTFOUND(fp));
if ((NULL != fp) || (NULL != path_found)) break;
}
}
if (NULL == fp)
{
fp = opencb(filename, user_data);
lgdebug(D_USER_FILES, "Debug: Opening file %s%s\n", filename, NOTFOUND(fp));
}
else if (NULL == path_found)
{
char *pfnd = strdup((NULL != completename) ? completename : filename);
if ((0 < verbosity) && (dict_file_open == opencb))
prt_error("Info: Dictionary found at %s\n", pfnd);
for (size_t i = 0; i < 2; i++)
{
char *root = find_last_dir_separator(pfnd);
if (NULL != root) *root = '\0';
}
path_found = pfnd;
lgdebug(D_USER_FILES, "Debug: Using dictionary path \"%s\"\n", path_found);
}
free(data_dir);
free(completename);
return fp;
}
#undef NOTFOUND
FILE *dictopen(const char *filename, const char *how)
{
return object_open(filename, dict_file_open, how);
}
static void *data_file_open(const char *fullname, const void *how)
{
return fopen(fullname, how);
}
FILE *linkgrammar_open_data_file(const char *filename)
{
object_open(NULL, NULL, NULL);
return object_open(filename, data_file_open, "r");
}
bool check_db(const char *lang)
{
char *dbname = join_path (lang, "dict.db");
bool retval = file_exists(dbname);
#if !HAVE_SQLITE3
if (retval)
prt_error("Error: Could not open dictionary \"%s\" "
"(not configured with SQLite support)\n", dbname);
#endif
free(dbname);
return retval;
}
#define ATOMESE_DICT "storage.dict"
bool check_atomspace(const char *lang)
{
char *cfgfile = join_path (lang, ATOMESE_DICT);
bool retval = file_exists(cfgfile);
#if !HAVE_ATOMESE
if (retval)
prt_error("Error: Could not open dictionary \"%s\" "
"(not configured with AtomSpace support)\n", cfgfile);
#endif
free(cfgfile);
return retval;
}
bool file_exists(const char * dict_name)
{
bool retval = false;
int fd;
struct stat buf;
FILE *fp = dictopen(dict_name, "rb");
if (fp == NULL)
return false;
fd = fileno(fp);
fstat(fd, &buf);
if (0 < buf.st_size) retval = true;
fclose(fp);
return retval;
}
char *get_file_contents(const char * dict_name)
{
int fd;
size_t tot_size;
size_t tot_read = 0;
struct stat buf;
char * contents;
FILE *fp = dictopen(dict_name, "rb");
if (fp == NULL)
return NULL;
fd = fileno(fp);
fstat(fd, &buf);
tot_size = buf.st_size;
contents = (char *) malloc(sizeof(char) * (tot_size+7));
while (1)
{
size_t read_size = fread(contents, 1, tot_size+7, fp);
if (0 == read_size)
{
bool err = (0 != ferror(fp));
if (err)
{
prt_error("Error: %s: Read error (%s)\n", dict_name,
syserror_msg(errno));
fclose(fp);
free(contents);
return NULL;
}
fclose(fp);
break;
}
tot_read += read_size;
}
if (tot_read > tot_size+6)
{
prt_error("Error: %s: File size is insane (%zu)!\n", dict_name, tot_size);
free(contents);
return NULL;
}
contents[tot_read] = '\0';
return contents;
}
void free_file_contents(char * contents)
{
free(contents);
}