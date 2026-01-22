#include <stdio.h>
#include "gen.h"
#include "postio.h"
extern char	*block;
extern int	blocksize;
extern int	head;
extern int	tail;
extern char	*line;
extern char	mesg[];
extern int	ttyo;
slowsend(fd_in)
int		fd_in;
{
while ( readblock(fd_in) )
switch ( getstatus(0) )  {
case WAITING:
writeblock(blocksize);
break;
case BUSY:
case IDLE:
case PRINTING:
writeblock(30);
break;
case NOSTATUS:
case UNKNOWN:
break;
case PRINTERERROR:
sleep(30);
break;
case ERROR:
fprintf(stderr, "%s", mesg);
error(FATAL, "PostScript Error");
break;
case FLUSHING:
error(FATAL, "Flushing Job");
break;
case DISCONNECT:
error(FATAL, "Disconnected - printer may be offline");
break;
default:
sleep(2);
break;
}
}
static writeblock(num)
int		num;
{
int		count;
if ( num > tail - head )
num = tail - head;
if ( (count = write(ttyo, &block[head], num)) == -1 )
error(FATAL, "error writing to %s", line);
else if ( count == 0 )
error(FATAL, "printer appears to be offline");
head += count;
return(count);
}