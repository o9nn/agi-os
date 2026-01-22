#ifndef MAILLOCK_H
#define MAILLOCK_H
#ifdef __cplusplus
extern "C" {
#endif
int maillock_read_lock(const char * filename, int fd);
int maillock_read_unlock(const char * filename, int fd);
int maillock_write_lock(const char * filename, int fd);
int maillock_write_unlock(const char * filename, int fd);
#ifdef __cplusplus
}
#endif
#endif