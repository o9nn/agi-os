#pragma	src	"/sys/src/libstdio"
#pragma	lib	"libstdio.a"
typedef struct{
int fd;
char flags;
char state;
char *buf;
char *rp;
char *wp;
char *lp;
long bufl;
char unbuf[1];
}FILE;
typedef long fpos_t;
#ifndef NULL
#define	NULL	((void*)0)
#endif
#define	_IOFBF	1
#define	_IOLBF	2
#define	_IONBF	3
#define	BUFSIZ	4096
#define	EOF	(-1)
#define	FOPEN_MAX	100
#define	FILENAME_MAX	BUFSIZ
#define	L_tmpnam	20
#ifndef SEEK_SET
#define	SEEK_CUR	1
#define	SEEK_END	2
#define	SEEK_SET	0
#endif
#define	TMP_MAX		64
#define	stderr	(&_IO_stream[2])
#define	stdin	(&_IO_stream[0])
#define	stdout	(&_IO_stream[1])
#define	_IO_CHMASK	0377
FILE *tmpfile(void);
char *tmpnam(char *);
int fclose(FILE *);
int fflush(FILE *);
FILE *fopen(const char *, const char *);
FILE *fdopen(const int, const char *);
FILE *freopen(const char *, const char *, FILE *);
void setbuf(FILE *, char *);
int setvbuf(FILE *, char *, int, long);
int fprintf(FILE *, const char *, ...);
int fscanf(FILE *, const char *, ...);
int printf(const char *, ...);
int scanf(const char *, ...);
int sprintf(char *, const char *, ...);
int snprintf(char *, int, const char *, ...);
int sscanf(const char *, const char *, ...);
int vfprintf(FILE *, const char *, va_list);
int vprintf(const char *, va_list);
int vsprintf(char *, const char *, va_list);
int vsnprintf(char *, int, const char *, va_list);
int vfscanf(FILE *, const char *, va_list);
int fgetc(FILE *);
char *fgets(char *, int, FILE *);
int fputc(int, FILE *);
int fputs(const char *, FILE *);
int getc(FILE *);
#define	getc(f)	((f)->rp>=(f)->wp?_IO_getc(f):*(f)->rp++&_IO_CHMASK)
int _IO_getc(FILE *f);
int getchar(void);
#define	getchar()	getc(stdin)
char *gets(char *);
int putc(int, FILE *);
#define	putc(c, f) ((f)->wp>=(f)->rp?_IO_putc(c, f):(*(f)->wp++=c)&_IO_CHMASK)
int _IO_putc(int, FILE *);
int putchar(int);
#define	putchar(c)	putc(c, stdout)
int puts(const char *);
int ungetc(int, FILE *);
long fread(void *, long, long, FILE *);
long fwrite(const void *, long, long, FILE *);
int fgetpos(FILE *, fpos_t *);
int fseek(FILE *, long, int);
int fseeko(FILE *, long long, int);
int fsetpos(FILE *, const fpos_t *);
long ftell(FILE *);
long long ftello(FILE *);
void rewind(FILE *);
void clearerr(FILE *);
int feof(FILE *);
int ferror(FILE *);
void perror(const char *);
extern FILE _IO_stream[FOPEN_MAX];
FILE *sopenr(const char *);
FILE *sopenw(void);
char *sclose(FILE *);
int fileno(FILE *);