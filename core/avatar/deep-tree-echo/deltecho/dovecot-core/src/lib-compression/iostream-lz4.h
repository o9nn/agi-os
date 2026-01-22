#ifndef IOSTREAM_LZ4_H
#define IOSTREAM_LZ4_H
#define IOSTREAM_LZ4_MAGIC "Dovecot-LZ4\x0d\x2a\x9b\xc5"
#define IOSTREAM_LZ4_MAGIC_LEN (sizeof(IOSTREAM_LZ4_MAGIC)-1)
struct iostream_lz4_header {
unsigned char magic[IOSTREAM_LZ4_MAGIC_LEN];
unsigned char max_uncompressed_chunk_size[4];
};
#define OSTREAM_LZ4_CHUNK_SIZE (1024*64)
#define ISTREAM_LZ4_CHUNK_SIZE (1024*1024)
#define IOSTREAM_LZ4_CHUNK_PREFIX_LEN 4
#endif