#include "stdpre.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>
#define ROMFS_BLOCKSIZE 4096
#define ROMFS_CBUFSIZE ((int)((ROMFS_BLOCKSIZE) * 1.001) + 12)
typedef struct romfs_inode_s {
char *name;
struct romfs_inode_s *next, *child;
unsigned int blocks;
unsigned long length;
unsigned long offset;
unsigned long data_size;
unsigned char **data;
unsigned int *data_lengths;
} romfs_inode;
static int put_int32(unsigned char *p, const unsigned int q)
{
*p++ = (q >> 24) & 0xFF;
*p++ = (q >> 16) & 0xFF;
*p++ = (q >> 8) & 0xFF;
*p++ = (q >> 0) & 0xFF;
return 4;
}
void inode_clear(romfs_inode* node)
{
int i;
if (node) {
if (node->data) {
for (i = 0; i < node->blocks; i++) {
if (node->data[i]) free(node->data[i]);
}
free(node->data);
}
if (node->data_lengths) free(node->data_lengths);
if (node->name) free(node->name);
}
}
int
inode_write(FILE *out, romfs_inode *node)
{
int i, offset = 0;
unsigned char buf[64];
unsigned char *p = buf;
p += put_int32(p, node->offset);
p += put_int32(p, node->length);
p += put_int32(p, strlen(node->name));
printf("writing node '%s'...\n", node->name);
printf(" offset %ld\n", node->offset);
printf(" length %ld\n", node->length);
printf(" path length %ld\n", strlen(node->name));
printf(" %d compressed blocks comprising %ld bytes\n", node->blocks, node->data_size);
offset += fwrite(buf, 3, 4, out);
offset += fwrite(node->name, 1, strlen(node->name), out);
offset += fwrite(node->data_lengths, node->blocks, sizeof(*node->data_lengths), out);
for (i = 0; i < node->blocks; i++)
offset += fwrite(node->data[i], 1, node->data_lengths[i], out);
printf(" wrote %d bytes in all\n", offset);
return offset;
}
int
main(int argc, char *argv[])
{
int i, ret, block;
romfs_inode *node;
unsigned char *ubuf, *cbuf;
unsigned long ulen, clen;
unsigned long offset = 0;
FILE *in, *out;
ubuf = malloc(ROMFS_BLOCKSIZE);
cbuf = malloc(ROMFS_CBUFSIZE);
printf("compressing with %d byte blocksize (zlib output buffer %d bytes)\n",
ROMFS_BLOCKSIZE, ROMFS_CBUFSIZE);
out = fopen("gsromfs", "wb");
for (i = 1; i < argc; i++) {
node = calloc(1, sizeof(romfs_inode));
node->name = strdup(argv[i]);
in = fopen(node->name, "rb");
fseek(in, 0, SEEK_END);
node->length = ftell(in);
node->blocks = (node->length - 1) / ROMFS_BLOCKSIZE + 1;
node->data_lengths = calloc(node->blocks, sizeof(unsigned int));
node->data = calloc(node->blocks, sizeof(unsigned char *));
fclose(in);
in = fopen(node->name, "rb");
block = 0;
while (!feof(in)) {
ulen = fread(ubuf, 1, ROMFS_BLOCKSIZE, in);
if (!ulen) break;
clen = ROMFS_CBUFSIZE;
ret = compress(cbuf, &clen, ubuf, ulen);
if (ret != Z_OK) {
printf("error compressing data block!\n");
}
node->data_lengths[block] = clen;
node->data[block] = malloc(clen);
memcpy(node->data[block], cbuf, clen);
block++;
node->data_size += clen;
}
fclose(in);
node->offset = 12 + 4 * node->blocks + node->data_size + strlen(node->name);
printf("inode %d (%ld/%ld bytes) '%s'\t%ld%%\n",
i, node->data_size, node->length, node->name, 100*node->data_size/node->length);
inode_write(out, node);
inode_clear(node);
free(node);
}
free(ubuf);
fclose(out);
return 0;
}