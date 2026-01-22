#include "stdio_.h"
#include "string_.h"
#include "time_.h"
#include <stdlib.h>
#include "gconfigd.h"
#include "gp.h"
#include "md5.h"
#define GP_CACHE_VERSION 0
typedef struct gp_cache_entry_s {
int type;
int keylen;
byte *key;
md5_byte_t hash[16];
char *filename;
int len;
void *buffer;
int dirty;
time_t last_used;
} gp_cache_entry;
private void gp_cache_clear_entry(gp_cache_entry *item)
{
item->type = -1;
item->key = NULL;
item->keylen = 0;
item->filename = NULL;
item->buffer = NULL;
item->len = 0;
item->dirty = 0;
item->last_used = 0;
}
private char *gp_cache_prefix(void)
{
char *prefix = NULL;
int plen = 0;
if (gp_getenv("GS_CACHE_DIR", (char *)NULL, &plen) < 0) {
prefix = malloc(plen);
gp_getenv("GS_CACHE_DIR", prefix, &plen);
plen--;
} else {
#ifdef GS_CACHE_DIR
prefix = strdup(GS_CACHE_DIR);
#else
prefix = strdup(".cache");
#endif
plen = strlen(prefix);
}
if (plen > 1 && prefix[0] == '~') {
char *home, *path;
int hlen = 0;
unsigned int pathlen = 0;
gp_file_name_combine_result result;
if (gp_getenv("HOME", (char *)NULL, &hlen) < 0) {
home = malloc(hlen);
if (home == NULL) return prefix;
gp_getenv("HOME", home, &hlen);
hlen--;
if (plen == 1) {
free(prefix);
return home;
}
pathlen = hlen + plen + 1;
path = malloc(pathlen);
if (path == NULL) { free(home); return prefix; }
result = gp_file_name_combine(home, hlen, prefix+2, plen-2, false, path, &pathlen);
if (result == gp_combine_success) {
free(prefix);
prefix = path;
} else {
dlprintf1("file_name_combine failed with code %d\n", result);
}
free(home);
}
}
#ifdef DEBUG_CACHE
dlprintf1("cache dir read as '%s'\n", prefix);
#endif
return prefix;
}
private char *
gp_cache_indexfilename(const char *prefix)
{
const char *fn = "gs_cache";
char *path;
unsigned int len;
gp_file_name_combine_result result;
len = strlen(prefix) + strlen(fn) + 2;
path = malloc(len);
result = gp_file_name_combine(prefix, strlen(prefix), fn, strlen(fn), true, path, &len);
if (result == gp_combine_small_buffer) {
free(path);
path = malloc(++len);
result = gp_file_name_combine(prefix, strlen(prefix), fn, strlen(fn), true, path, &len);
}
if (result != gp_combine_success) {
dlprintf1("pcache: file_name_combine for indexfilename failed with code %d\n", result);
free(path);
return NULL;
}
return path;
}
private void gp_cache_hash(gp_cache_entry *entry)
{
md5_state_t md5;
md5_init(&md5);
md5_append(&md5, entry->key, entry->keylen);
md5_finish(&md5, entry->hash);
}
private void gp_cache_filename(const char *prefix, gp_cache_entry *item)
{
const char hexmap[16] = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
char *fn = malloc(gp_file_name_sizeof), *fni;
int i;
fni = fn;
*fni++ = hexmap[item->type>>4 & 0x0F];
*fni++ = hexmap[item->type & 0x0F];
*fni++ = '.';
for (i = 0; i < 16; i++) {
*fni++ = hexmap[(item->hash[i]>>4 & 0x0F)];
*fni++ = hexmap[(item->hash[i] & 0x0F)];
}
*fni = '\0';
if (item->filename) free(item->filename);
item->filename = fn;
}
private char *gp_cache_itempath(const char *prefix, gp_cache_entry *item)
{
const char *fn = item->filename;
gp_file_name_combine_result result;
char *path;
unsigned int len;
len = strlen(prefix) + strlen(fn) + 2;
path = malloc(len);
result = gp_file_name_combine(prefix, strlen(prefix),
fn, strlen(fn), false, path, &len);
if (result != gp_combine_success) {
dlprintf1("pcache: file_name_combine failed on cache item filename with code %d\n", result);
}
return path;
}
private int gp_cache_saveitem(FILE *file, gp_cache_entry* item)
{
unsigned char version = 0;
int ret;
#ifdef DEBUG_CACHE
dlprintf2("pcache: saving key with version %d, data length %d\n", version, item->len);
#endif
ret = fwrite(&version, 1, 1, file);
ret = fwrite(&(item->keylen), 1, sizeof(item->keylen), file);
ret = fwrite(item->key, 1, item->keylen, file);
ret = fwrite(&(item->len), 1, sizeof(item->len), file);
ret = fwrite(item->buffer, 1, item->len, file);
item->dirty = 0;
return ret;
}
private int gp_cache_loaditem(FILE *file, gp_cache_entry *item, gp_cache_alloc alloc, void *userdata)
{
unsigned char version;
unsigned char *filekey = NULL;
int len, keylen;
fread(&version, 1, 1, file);
if (version != GP_CACHE_VERSION) {
#ifdef DEBUG_CACHE
dlprintf2("pcache file version mismatch (%d vs expected %d)\n", version, GP_CACHE_VERSION);
#endif
return -1;
}
fread(&keylen, 1, sizeof(keylen), file);
if (keylen != item->keylen) {
#ifdef DEBUG_CACHE
dlprintf2("pcache file has correct hash but wrong key length (%d vs %d)\n",
keylen, item->keylen);
#endif
return -1;
}
filekey = malloc(keylen);
if (filekey != NULL)
fread(filekey, 1, keylen, file);
if (memcmp(filekey, item->key, keylen)) {
#ifdef DEBUG_CACHE
dlprintf("pcache file has correct hash but doesn't match the full key\n");
#endif
free(filekey);
item->buffer = NULL;
item->len = 0;
return -1;
}
free(filekey);
fread(&len, 1, sizeof(len), file);
#ifdef DEBUG_CACHE
dlprintf2("key matches file with version %d, data length %d\n", version, len);
#endif
item->buffer = alloc(userdata, len);
if (item->buffer == NULL) {
dlprintf("pcache: unable to allocate buffer for file data!\n");
return -1;
}
item->len = fread(item->buffer, 1, len, file);
item->dirty = 1;
item->last_used = time(NULL);
return 0;
}
private int readhexbyte(const char *s)
{
const char hexmap[16] = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
int i,r;
for (i = 0; i < 16; i++)
if (hexmap[i] == *s) break;
if (i == 16) return -1;
r = i;
s++;
for (i = 0; i < 16; i++)
if (hexmap[i] == *s) break;
if (i == 16) return -1;
r = r<<4 | i;
return r;
}
private int
gp_cache_read_entry(FILE *file, gp_cache_entry *item)
{
char line[256];
char fn[32];
int i;
if (!fgets(line, 256, file)) return -1;
if (line[0] == '#') return 1;
sscanf(line, "%s %ld\n", fn, &item->last_used);
item->type = readhexbyte(fn);
for (i = 0; i < 16; i++)
item->hash[i] = readhexbyte(fn + 3 + 2*i);
if (item->filename) free(item->filename);
item->filename = malloc(strlen(fn) + 1);
memcpy(item->filename, fn, strlen(fn));
item->key = NULL;
item->keylen = 0;
item->len = 0;
item->buffer = NULL;
return 0;
}
private int
gp_cache_write_entry(FILE *file, gp_cache_entry *item)
{
fprintf(file, "%s %ld\n", item->filename, item->last_used);
return 0;
}
int gp_cache_insert(int type, byte *key, int keylen, void *buffer, int buflen)
{
char *prefix, *path;
char *infn,*outfn;
FILE *file, *in, *out;
gp_cache_entry item, item2;
int code, hit = 0;
prefix = gp_cache_prefix();
infn = gp_cache_indexfilename(prefix);
{
int len = strlen(infn) + 2;
outfn = malloc(len);
memcpy(outfn, infn, len - 2);
outfn[len-2] = '+';
outfn[len-1] = '\0';
}
in = fopen(infn, "r");
if (in == NULL) {
dlprintf1("pcache: unable to open '%s'\n", infn);
return -1;
}
out = fopen(outfn, "w");
if (out == NULL) {
dlprintf1("pcache: unable to open '%s'\n", outfn);
return -1;
}
fprintf(out, "# Ghostscript persistent cache index table\n");
gp_cache_clear_entry(&item);
item.type = type;
item.key = key;
item.keylen = keylen;
item.buffer = buffer;
item.len = buflen;
item.dirty = 1;
item.last_used = time(NULL);
gp_cache_hash(&item);
gp_cache_filename(prefix, &item);
path = gp_cache_itempath(prefix, &item);
file = fopen(path, "wb");
if (file != NULL) {
gp_cache_saveitem(file, &item);
fclose(file);
}
gp_cache_clear_entry(&item2);
while ((code = gp_cache_read_entry(in, &item2)) >= 0) {
if (code == 1) continue;
if (!memcmp(item.hash, item2.hash, 16)) {
gp_cache_write_entry(out, &item);
hit = 1;
} else {
gp_cache_write_entry(out, &item2);
}
}
if (!hit) {
gp_cache_write_entry(out, &item);
}
free(item.filename);
fclose(out);
fclose(in);
unlink(infn);
rename(outfn,infn);
free(prefix);
free(infn);
free(outfn);
return 0;
}
int gp_cache_query(int type, byte* key, int keylen, void **buffer,
gp_cache_alloc alloc, void *userdata)
{
char *prefix, *path;
char *infn,*outfn;
FILE *file, *in, *out;
gp_cache_entry item, item2;
int code, hit = 0;
prefix = gp_cache_prefix();
infn = gp_cache_indexfilename(prefix);
{
int len = strlen(infn) + 2;
outfn = malloc(len);
memcpy(outfn, infn, len - 2);
outfn[len-2] = '+';
outfn[len-1] = '\0';
}
in = fopen(infn, "r");
if (in == NULL) {
dlprintf1("pcache: unable to open '%s'\n", infn);
return -1;
}
out = fopen(outfn, "w");
if (out == NULL) {
dlprintf1("pcache: unable to open '%s'\n", outfn);
return -1;
}
fprintf(out, "# Ghostscript persistent cache index table\n");
gp_cache_clear_entry(&item);
item.type = type;
item.key = key;
item.keylen = keylen;
item.last_used = time(NULL);
gp_cache_hash(&item);
gp_cache_filename(prefix, &item);
path = gp_cache_itempath(prefix, &item);
file = fopen(path, "rb");
if (file != NULL) {
hit = gp_cache_loaditem(file, &item, alloc, userdata);
fclose(file);
} else {
hit = -1;
}
gp_cache_clear_entry(&item2);
while ((code = gp_cache_read_entry(in, &item2)) >= 0) {
if (code == 1) continue;
if (!hit && !memcmp(item.hash, item2.hash, 16)) {
gp_cache_write_entry(out, &item);
item.dirty = 0;
} else {
gp_cache_write_entry(out, &item2);
}
}
if (item.dirty) {
gp_cache_write_entry(out, &item);
}
free(item.filename);
fclose(out);
fclose(in);
unlink(infn);
rename(outfn,infn);
free(prefix);
free(infn);
free(outfn);
if (!hit) {
*buffer = item.buffer;
return item.len;
} else {
*buffer = NULL;
return -1;
}
}