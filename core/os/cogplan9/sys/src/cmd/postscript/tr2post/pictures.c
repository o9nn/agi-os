#include <u.h>
#include <libc.h>
#include <bio.h>
#include <stdio.h>
#include "ext.h"
#include "common.h"
#include "tr2post.h"
#include "comments.h"
#include "path.h"
Biobuf	*bfp_pic = NULL;
Biobufhdr	*Bfp_pic;
Biobufhdr	*picopen(char *);
#define MAXGETFIELDS	16
char *fields[MAXGETFIELDS];
int nfields;
extern int	devres, hpos, vpos;
extern int	picflag;
void
picture(Biobufhdr *inp, char *buf) {
int	i;
int	indent;
int	length;
int	outline = 0;
int	page = 1;
int	poffset;
int	scaleboth = 0;
int	totrap;
int	whiteout = 0;
char	flags[20];
char	hwo[40], *p;
char	name[100];
char	units;
double	adjx = 0.5;
double	adjy = 0.5;
double	frame[4];
double	rot = 0;
Biobufhdr *fp_in;
USED(inp);
if (!picflag)
return;
endstring();
flags[0] = '\0';
nfields = getfields(buf, fields, MAXGETFIELDS, 0, ":\n");
if (nfields < 6) {
error(WARNING, "too few arguments to specify picture");
return;
}
poffset = atoi(fields[1]);
indent = atoi(fields[2]);
length = atoi(fields[3]);
totrap = atoi(fields[4]);
strncpy(name, fields[5], sizeof(name));
strncpy(hwo, fields[6], sizeof(hwo));
if (nfields >= 6)
strncpy(flags, fields[7], sizeof(flags));
nfields = getfields(buf, fields, MAXGETFIELDS, 0, "()");
if (nfields == 2) {
strncpy(name, fields[0], sizeof(name));
page = atoi(fields[1]);
}
if ((fp_in = picopen(name)) == NULL) {
error(WARNING, "can't open picture file %s\n", name);
return;
}
frame[0] = frame[1] = -1;
frame[2] = frame[3] = 0;
for (i = 0, p = hwo-1; i < 4 && p != NULL; i++, p = strchr(p, ','))
if (sscanf(++p, "%lf%c", &frame[i], &units) == 2)
if (units == 'i' || units == ',' || units == '\0')
frame[i] *= devres;
if (frame[0] <= 0)
frame[0] = totrap;
if (frame[1] <= 0)
frame[1] = length - indent;
frame[3] += poffset + indent;
for (i = 0; flags[i]; i++)
switch (flags[i]) {
case 'c': adjx = adjy = 0.5; break;
case 'l': adjx = 0; break;
case 'r': adjx = 1; break;
case 't': adjy = 1; break;
case 'b': adjy = 0; break;
case 'o': outline = 1; break;
case 'w': whiteout = 1; break;
case 's': scaleboth = 1; break;
case 'a': if ( sscanf(&flags[i+1], "%lf", &rot) != 1 )
rot += 90;
}
endstring();
Bprint(Bstdout, "cleartomark\n");
Bprint(Bstdout, "saveobj restore\n");
ps_include(fp_in, Bstdout, page, whiteout, outline, scaleboth,
frame[3]+frame[1]/2, -vpos-frame[2]-frame[0]/2, frame[1], frame[0], adjx, adjy, -rot);
Bprint(Bstdout, "/saveobj save def\n");
Bprint(Bstdout, "mark\n");
Bterm(fp_in);
}
Biobufhdr *
picopen(char *path) {
Biobuf *bfp;
if ((bfp = Bopen(path, OREAD)) == 0)
error(FATAL, "can't open %s\n", path);
return bfp;
#ifdef UNDEF
if (Bfp_pic != NULL) {
Bseek(Bfp_pic, 0L, 0);
while (Bgetfield(Bfp_pic, 's', name, 99)>0
&& Bgetfield(Bfp_pic, 'd', &total, 0)>0) {
pos = Bseek(Bfp_pic, 0L, 1);
if (strcmp(path, name) == 0) {
if (tmpnam(pictmpname) == NULL)
error(FATAL, "can't generate temp file name");
if ( (bfp = Bopen(pictmpname, ORDWR)) == NULL )
error(FATAL, "can't open %s", pictmpname);
Bfp = &(bfp->Biobufhdr);
piccopy(Bfp_pic, Bfp, total);
Bseek(Bfp, 0L, 0);
return(Bfp);
}
Bseek(Bfp_pic, total+pos, 0);
}
}
return Bopen(path, OREAD);
#endif
}
#ifdef UNDEF
void
inlinepic(Biobufhdr *Bfp, char *buf) {
char	name[100];
long	total;
if (Bfp_pic == NULL ) {
tmpnam(pictmpname);
if ((bfp_pic = Bopen(pictmpname, ORDWR)) == 0)
error(FATAL, "can't open in-line picture file %s", ipictmpname);
unlink(pictmpname);
}
if ( sscanf(buf, "%s %ld", name, &total) != 2 )
error(FATAL, "in-line picture error");
fseek(Bfp_pic, 0L, 2);
fprintf(Bfp_pic, "%s %ld\n", name, total);
getc(fp);
fflush(fp_pic);
piccopy(fp, fp_pic, total);
ungetc('\n', fp);
}
#endif
void
piccopy(Biobufhdr *Bfp_in, Biobufhdr *Bfp_out, long total) {
long i;
for (i = 0; i < total; i++)
if (Bputc(Bfp_out, Bgetc(Bfp_in)) < 0)
error(FATAL, "error copying in-line picture file");
Bflush(Bfp_out);
}