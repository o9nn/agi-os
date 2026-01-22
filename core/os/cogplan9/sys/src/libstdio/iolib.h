#include <u.h>
#include <libc.h>
#undef END
#include "Stdio.h"
#define	BALLOC	1
#define	LINEBUF	2
#define	STRING	4
#define APPEND	8
#define	CLOSED	0
#define	OPEN	1
#define	RDWR	2
#define	RD	3
#define	WR	4
#define	ERR	5
#define	END	6
int _IO_setvbuf(FILE *);
extern QLock _stdiolk;