#ifndef	_STDIO_H_
#define	_STDIO_H_
#pragma lib "/$M/lib/ape/libap.a"
#include <stdarg.h>
#include <stddef.h>
#include <sys/types.h>
typedef struct{
int fd;
char flags;
char state;
char *buf;
char *rp;
char *wp;
char *lp;
size_t bufl;
char unbuf[1];
}FILE;
typedef long long fpos_t;
#ifndef NULL
#ifdef __cplusplus
#define NULL 0
#else
#define NULL ((void*)0)
#endif
#endif
#define	_IOFBF	1
#define	_IOLBF	2
#define	_IONBF	3
#define	BUFSIZ	4096
#define	EOF	(-1)
#define	FOPEN_MAX	90
#define	FILENAME_MAX	BUFSIZ
#define	L_tmpnam	20
#define	L_cuserid	32
#define	L_ctermid	32
#define	SEEK_CUR	1
#define	SEEK_END	2
#define	SEEK_SET	0
#define	TMP_MAX		64
#define	stderr	(&_IO_stream[2])
#define	stdin	(&_IO_stream[0])
#define	stdout	(&_IO_stream[1])
#define	_IO_CHMASK	0377
#ifdef __cplusplus
extern "C" {
#endif
extern int remove(const char *);
extern int rename(const char *, const char *);
extern FILE *tmpfile(void);
extern char *tmpnam(char *);
extern int fclose(FILE *);
extern int fflush(FILE *);
extern FILE *fopen(const char *, const char *);
extern FILE *freopen(const char *, const char *, FILE *);
extern void setbuf(FILE *, char *);
extern int setvbuf(FILE *, char *, int, size_t);
extern int fprintf(FILE *, const char *, ...);
extern int fscanf(FILE *, const char *, ...);
extern int printf(const char *, ...);
extern int scanf(const char *, ...);
extern int sprintf(char *, const char *, ...);
extern int snprintf(char *, size_t, const char *, ...);
extern int vsnprintf(char *, size_t, const char *, va_list);
extern int sscanf(const char *, const char *, ...);
extern int vfprintf(FILE *, const char *, va_list);
extern int vprintf(const char *, va_list);
extern int vsprintf(char *, const char *, va_list);
extern int vfscanf(FILE *, const char *, va_list);
extern int fgetc(FILE *);
extern char *fgets(char *, int, FILE *);
extern int fputc(int, FILE *);
extern int fputs(const char *, FILE *);
extern int getc(FILE *);
#define	getc(f)	((f)->rp>=(f)->wp?_IO_getc(f):*(f)->rp++&_IO_CHMASK)
extern int _IO_getc(FILE *f);
extern int getchar(void);
#define	getchar()	getc(stdin)
extern char *gets(char *);
extern int putc(int, FILE *);
#define	putc(c, f) ((f)->wp>=(f)->rp?_IO_putc(c, f):(*(f)->wp++=c)&_IO_CHMASK)
extern int _IO_putc(int, FILE *);
extern int putchar(int);
#define	putchar(c)	putc(c, stdout)
extern int puts(const char *);
extern int ungetc(int, FILE *);
extern size_t fread(void *, size_t, size_t, FILE *);
extern size_t fwrite(const void *, size_t, size_t, FILE *);
extern int fgetpos(FILE *, fpos_t *);
extern int fseek(FILE *, long, int);
extern int fseeko(FILE *, off_t, int);
extern int fsetpos(FILE *, const fpos_t *);
extern long ftell(FILE *);
extern off_t ftello(FILE *);
extern void rewind(FILE *);
extern void clearerr(FILE *);
extern int feof(FILE *);
extern int ferror(FILE *);
extern void perror(const char *);
extern FILE _IO_stream[FOPEN_MAX];
#ifdef _POSIX_SOURCE
extern int fileno(FILE *);
extern FILE* fdopen(int, const char*);
extern char *ctermid(char *);
#endif
#ifdef _REENTRANT_SOURCE
extern char *tmpnam_r(char *);
extern char *ctermid_r(char *);
#endif
#ifdef _BSD_EXTENSION
#pragma lib "/$M/lib/ape/libbsd.a"
extern FILE *popen(char *, char *);
extern int	pclose(FILE *);
#endif
#ifdef __cplusplus
}
#endif
#endif