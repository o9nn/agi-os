#include "tdef.h"
#include "fns.h"
#include "ext.h"
#define MAXCH NCHARS
char *chnames[MAXCH];
int nchnames;
#define MAXPS 100
int pstab[MAXPS];
int nsizes;
Font fonts[MAXFONTS+1];
#define skipline(f) while (getc(f) != '\n')
#define eq(s1, s2) (strcmp(s1, s2) == 0)
getdesc(char *name)
{
FILE *fin;
char cmd[100], s[100];
int i, v;
if ((fin = fopen(name, "r")) == NULL)
return -1;
while (fscanf(fin, "%s", cmd) != EOF) {
if (strcmp(cmd, "res") == 0) {
fscanf(fin, "%d", &Inch);
} else if (strcmp(cmd, "hor") == 0) {
fscanf(fin, "%d", &Hor);
} else if (strcmp(cmd, "vert") == 0) {
fscanf(fin, "%d", &Vert);
} else if (strcmp(cmd, "unitwidth") == 0) {
fscanf(fin, "%d", &Unitwidth);
} else if (strcmp(cmd, "sizes") == 0) {
nsizes = 0;
while (fscanf(fin, "%d", &v) != EOF && v != 0 && nsizes < MAXPS)
pstab[nsizes++] = v;
} else if (strcmp(cmd, "fonts") == 0) {
fscanf(fin, "%d", &nfonts);
for (i = 1; i <= nfonts; i++) {
fscanf(fin, "%s", s);
fontlab[i] = PAIR(s[0], s[1]);
}
} else if (strcmp(cmd, "charset") == 0) {
while (fscanf(fin, "%s", s) != EOF)
chadd(s, Troffchar, Install);
break;
}
skipline(fin);
}
fclose(fin);
return 1;
}
static int checkfont(char *name)
{
FILE *fp;
char buf[300], buf2[300];
int i, status = -1;
if ((fp = fopen(name, "r")) == NULL)
return -1;
for (i = 1; i <= 10; i++) {
if (fgets(buf, sizeof buf, fp) == NULL)
break;
sscanf(buf, "%s", buf2);
if (buf2[0] == '#') {
i--;
continue;
}
if (eq(buf2, "name") || eq(buf2, "fontname") ||
eq(buf2, "special") || eq(buf2, "charset")) {
status = 1;
break;
}
}
fclose(fp);
return status;
}
getfont(char *name, int pos)
{
FILE *fin;
Font *ftemp = &fonts[pos];
Chwid chtemp[MAXCH];
static Chwid chinit;
int i, nw, n, wid, kern, code, type;
char buf[100], ch[100], s1[100], s2[100], s3[100], cmd[300];
if (checkfont(name) == -1)
return -1;
if ((fin = fopen(name, "r")) == NULL)
return -1;
for (i = 0; i < ALPHABET; i++)
chtemp[i] = chinit;
ftemp->specfont = ftemp->ligfont = 0;
ftemp->defaultwidth = ftemp->spacewidth = Inch * Unitwidth / 72 / 3;
nw = code = 0;
while (fscanf(fin, "%s", cmd) != EOF) {
if (strcmp(cmd, "name") == 0)
fscanf(fin, "%s", ftemp->longname);
else if (strcmp(cmd, "special") == 0)
ftemp->specfont = 1;
else if (strcmp(cmd, "ligatures") == 0) {
ftemp->ligfont = getlig(fin);
} else if (strcmp(cmd, "spacewidth") == 0) {
fscanf(fin, "%d", &ftemp->spacewidth);
} else if (strcmp(cmd, "defaultwidth") == 0) {
fscanf(fin, "%d", &ftemp->defaultwidth);
} else if (strcmp(cmd, "charset") == 0) {
wchar_t wc;
skipline(fin);
nw = ALPHABET;
while (fgets(buf, sizeof buf, fin) != NULL) {
sscanf(buf, "%s %s %s %s", ch, s1, s2, s3);
if (s1[0] != '"') {
sscanf(s1, "%d", &wid);
sscanf(s2, "%d", &kern);
code = strtol(s3, 0, 0);
}
if (strlen(ch) == 1) {
n = ch[0];
chtemp[n].num = ch[0];
} else if (ch[0] == '\\' && ch[1] == '0') {
n = strtol(ch+1, 0, 0);
chtemp[n].num = n;
#ifdef UNICODE
} else if (mbtowc(&wc, ch, strlen(ch)) > 1) {
chtemp[nw].num = chadd(ch, MBchar, Install);
n = nw;
nw++;
#endif
} else {
if (strcmp(ch, "---") == 0) {
sprintf(ch, "%d", code);
type = Number;
} else
type = Troffchar;
chtemp[nw].num = chadd(ch, type, Install);
n = nw;
nw++;
}
chtemp[n].wid = wid;
chtemp[n].kern = kern;
chtemp[n].code = code;
}
break;
}
skipline(fin);
}
fclose(fin);
chtemp[' '].wid = ftemp->spacewidth;
ftemp->nchars = nw;
if (ftemp->wp)
free(ftemp->wp);
ftemp->wp = (Chwid *) malloc(nw * sizeof(Chwid));
if (ftemp->wp == NULL)
return -1;
for (i = 0; i < nw; i++)
ftemp->wp[i] = chtemp[i];
return 1;
}
chadd(char *s, int type, int install)
{
char *p;
int i;
for (i = 0; i < nchnames; i++)
if (type == chnames[i][0] && eq(s, chnames[i]+1))
break;
if (i < nchnames)
return ALPHABET + i;
else if (install == Lookup)
return -1;
chnames[nchnames] = p = (char *) malloc(strlen(s)+1+1);
if (p == NULL) {
ERROR "out of space adding character %s", s WARN;
return LEFTHAND;
}
if (nchnames >= NCHARS - ALPHABET) {
ERROR "out of table space adding character %s", s WARN;
return LEFTHAND;
}
strcpy(chnames[nchnames]+1, s);
chnames[nchnames][0] = type;
return nchnames++ + ALPHABET;
}
char *chname(int n)
{
if (n >= ALPHABET && n < nchnames + ALPHABET)
return chnames[n-ALPHABET];
else
return "";
}
getlig(FILE *fin)
{
int lig;
char temp[200];
lig = 0;
while (fscanf(fin, "%s", temp) != EOF && strcmp(temp, "0") != 0) {
if (strcmp(temp, "fi") == 0)
lig |= LFI;
else if (strcmp(temp, "fl") == 0)
lig |= LFL;
else if (strcmp(temp, "ff") == 0)
lig |= LFF;
else if (strcmp(temp, "ffi") == 0)
lig |= LFFI;
else if (strcmp(temp, "ffl") == 0)
lig |= LFFL;
else
fprintf(stderr, "illegal ligature %s ignored\n", temp);
}
return lig;
}