#ifndef ISTREAM_FS_FILE_H
#define ISTREAM_FS_FILE_H
struct fs_file;
struct istream *
i_stream_create_fs_file(struct fs_file **file, size_t max_buffer_size);
#endif