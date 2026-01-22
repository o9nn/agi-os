#ifndef lint
static char *ident = "$Id: append.c,v 1.2 89/02/12 10:03:58 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
void append_archive(void)
#else
void append_archive()
#endif
{
Stat            sb;
char            name[PATH_MAX + 1];
name[0] = '\0';
while (get_header(name, &sb) == 0) {
if (((ar_format == TAR)
? buf_skip(ROUNDUP((OFFSET) sb.sb_size, BLOCKSIZE))
: buf_skip((OFFSET) sb.sb_size)) < 0) {
warn(name, "File data is corrupt");
}
}
bufend = bufidx = bufstart;
create_archive();
}