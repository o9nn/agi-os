#define _PLAN9_SOURCE
#define _BSD_EXTENSION
#define _POSIX_SOURCE
#include <u.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>
#include <utime.h>
#include <sys/types.h>
#include <sys/stat.h>
#define	min(a,b)	((a>b) ? b : a)
#define BITS	16
#define HSIZE	69001
typedef long	code_int;
typedef long	count_int;
static char rcs_ident[] = "$Header: compress.c,v 4.0 85/07/30 12:50:00 joe Release $";
uchar magic_header[] = { 0x1F, 0x9D };
#define BIT_MASK	0x1f
#define BLOCK_MASK	0x80
#define INIT_BITS 9
#define ARGVAL() (*++(*argv) || (--argc && *++argv))
int n_bits;
int maxbits = BITS;
code_int maxcode;
code_int maxmaxcode = 1 << BITS;
#define MAXCODE(n_bits)	((1 << (n_bits)) - 1)
count_int htab[HSIZE];
ushort codetab[HSIZE];
#define htabof(i)	htab[i]
#define codetabof(i)	codetab[i]
code_int hsize = HSIZE;
count_int fsize;
#define tab_prefixof(i)	codetabof(i)
#define tab_suffixof(i)	((uchar *)(htab))[i]
#define de_stack		((uchar *)&tab_suffixof(1<<BITS))
code_int free_ent = 0;
int exit_stat = 0;
void	cl_block(void);
void	cl_hash(count_int);
void	compress(void);
void	copystat(char *, char *);
void	decompress(void);
int	foreground(void);
code_int getcode(void);
void	onintr(int);
void	oops(int);
void	output(code_int);
void	prratio(FILE *, long, long);
void	version(void);
void	writeerr(void);
void
Usage(void)
{
#ifdef DEBUG
fprintf(stderr,"Usage: compress [-cdfDV] [-b maxbits] [file ...]\n");
#else
fprintf(stderr,"Usage: compress [-cdfvV] [-b maxbits] [file ...]\n");
#endif
}
int debug = 0;
int nomagic = 0;
int zcat_flg = 0;
int quiet = 1;
int block_compress = BLOCK_MASK;
int clear_flg = 0;
long ratio = 0;
#define CHECK_GAP 10000
count_int checkpoint = CHECK_GAP;
#define FIRST	257
#define	CLEAR	256
int force = 0;
char ofname [100];
#ifdef DEBUG
int verbose = 0;
#endif
void (*bgnd_flag)(int);
int do_decomp = 0;
main(argc, argv)
int argc;
char **argv;
{
int overwrite = 0;
char tempname[512];
char **filelist, **fileptr;
char *cp;
struct stat statbuf;
if ( (bgnd_flag = signal ( SIGINT, SIG_IGN )) != SIG_IGN ) {
signal(SIGINT, onintr);
signal(SIGSEGV, oops);
}
filelist = fileptr = (char **)(malloc(argc * sizeof(*argv)));
*filelist = NULL;
if((cp = strrchr(argv[0], '/')) != 0)
cp++;
else
cp = argv[0];
if(strcmp(cp, "uncompress") == 0)
do_decomp = 1;
else if(strcmp(cp, "zcat") == 0) {
do_decomp = 1;
zcat_flg = 1;
}
for (argc--, argv++; argc > 0; argc--, argv++) {
if (**argv == '-') {
while (*++(*argv)) {
switch (**argv) {
case 'C':
block_compress = 0;
break;
#ifdef DEBUG
case 'D':
debug = 1;
break;
case 'V':
verbose = 1;
version();
break;
#else
case 'V':
version();
break;
#endif
case 'b':
if (!ARGVAL()) {
fprintf(stderr, "Missing maxbits\n");
Usage();
exit(1);
}
maxbits = atoi(*argv);
goto nextarg;
case 'c':
zcat_flg = 1;
break;
case 'd':
do_decomp = 1;
break;
case 'f':
case 'F':
overwrite = 1;
force = 1;
break;
case 'n':
nomagic = 1;
break;
case 'q':
quiet = 1;
break;
case 'v':
quiet = 0;
break;
default:
fprintf(stderr, "Unknown flag: '%c'; ", **argv);
Usage();
exit(1);
}
}
} else {
*fileptr++ = *argv;
*fileptr = NULL;
}
nextarg:
continue;
}
if(maxbits < INIT_BITS) maxbits = INIT_BITS;
if (maxbits > BITS) maxbits = BITS;
maxmaxcode = 1 << maxbits;
if (*filelist != NULL) {
for (fileptr = filelist; *fileptr; fileptr++) {
exit_stat = 0;
if (do_decomp != 0) {
if (strcmp(*fileptr + strlen(*fileptr) - 2, ".Z") != 0) {
strcpy(tempname, *fileptr);
strcat(tempname, ".Z");
*fileptr = tempname;
}
if ((freopen(*fileptr, "r", stdin)) == NULL) {
perror(*fileptr);
continue;
}
if (nomagic == 0) {
if ((getchar() != (magic_header[0] & 0xFF))
|| (getchar() != (magic_header[1] & 0xFF))) {
fprintf(stderr, "%s: not in compressed format\n",
*fileptr);
continue;
}
maxbits = getchar();
block_compress = maxbits & BLOCK_MASK;
maxbits &= BIT_MASK;
maxmaxcode = 1 << maxbits;
if(maxbits > BITS) {
fprintf(stderr,
"%s: compressed with %d bits, can only handle %d bits\n",
*fileptr, maxbits, BITS);
continue;
}
}
strcpy(ofname, *fileptr);
ofname[strlen(*fileptr) - 2] = '\0';
} else {
if (strcmp(*fileptr + strlen(*fileptr) - 2, ".Z") == 0) {
fprintf(stderr,
"%s: already has .Z suffix -- no change\n",
*fileptr);
continue;
}
if ((freopen(*fileptr, "r", stdin)) == NULL) {
perror(*fileptr);
continue;
}
(void) stat(*fileptr, &statbuf);
fsize = (long) statbuf.st_size;
hsize = HSIZE;
if (fsize < (1 << 12))
hsize = min(5003, HSIZE);
else if (fsize < (1 << 13))
hsize = min(9001, HSIZE);
else if (fsize < (1 << 14))
hsize = min (18013, HSIZE);
else if (fsize < (1 << 15))
hsize = min (35023, HSIZE);
else if (fsize < 47000)
hsize = min (50021, HSIZE);
strcpy(ofname, *fileptr);
#ifndef BSD4_2
if ((cp=strrchr(ofname,'/')) != NULL)
cp++;
else
cp = ofname;
if (strlen(cp) > 25) {
fprintf(stderr, "%s: filename too long to tack on .Z\n",
cp);
continue;
}
#endif
strcat(ofname, ".Z");
}
if (overwrite == 0 && zcat_flg == 0 &&
stat(ofname, &statbuf) == 0) {
char response[2];
response[0] = 'n';
fprintf(stderr, "%s already exists;", ofname);
if (foreground()) {
fprintf(stderr,
" do you wish to overwrite %s (y or n)? ",
ofname);
fflush(stderr);
(void) read(2, response, 2);
while (response[1] != '\n')
if (read(2, response+1, 1) < 0) {
perror("stderr");
break;
}
}
if (response[0] != 'y') {
fprintf(stderr, "\tnot overwritten\n");
continue;
}
}
if(zcat_flg == 0) {
if (freopen(ofname, "w", stdout) == NULL) {
perror(ofname);
continue;
}
if(!quiet)
fprintf(stderr, "%s: ", *fileptr);
}
if (do_decomp == 0)
compress();
#ifndef DEBUG
else
decompress();
#else
else if (debug == 0)
decompress();
else
printcodes();
if (verbose)
dump_tab();
#endif
if(zcat_flg == 0) {
copystat(*fileptr, ofname);
if (exit_stat == 1 || !quiet)
putc('\n', stderr);
}
}
} else {
if (do_decomp == 0) {
compress();
#ifdef DEBUG
if(verbose)
dump_tab();
#endif
if(!quiet)
putc('\n', stderr);
} else {
if (nomagic == 0) {
if ((getchar()!=(magic_header[0] & 0xFF))
|| (getchar()!=(magic_header[1] & 0xFF))) {
fprintf(stderr, "stdin: not in compressed format\n");
exit(1);
}
maxbits = getchar();
block_compress = maxbits & BLOCK_MASK;
maxbits &= BIT_MASK;
maxmaxcode = 1 << maxbits;
fsize = 100000;
if(maxbits > BITS) {
fprintf(stderr,
"stdin: compressed with %d bits, can only handle %d bits\n",
maxbits, BITS);
exit(1);
}
}
#ifndef DEBUG
decompress();
#else
if (debug == 0)
decompress();
else
printcodes();
if (verbose)
dump_tab();
#endif
}
}
exit(exit_stat);
return 0;
}
static int offset;
long in_count = 1;
long bytes_out;
long out_count = 0;
void
compress(void)
{
code_int ent, hsize_reg;
code_int i;
int c, disp, hshift;
long fcode;
if (nomagic == 0) {
putchar(magic_header[0]);
putchar(magic_header[1]);
putchar((char)(maxbits | block_compress));
if(ferror(stdout))
writeerr();
}
offset = 0;
bytes_out = 3;
out_count = 0;
clear_flg = 0;
ratio = 0;
in_count = 1;
checkpoint = CHECK_GAP;
maxcode = MAXCODE(n_bits = INIT_BITS);
free_ent = (block_compress? FIRST: 256);
ent = getchar ();
hshift = 0;
for (fcode = (long)hsize;  fcode < 65536L; fcode *= 2)
hshift++;
hshift = 8 - hshift;
hsize_reg = hsize;
cl_hash( (count_int) hsize_reg);
while ((c = getchar()) != EOF) {
in_count++;
fcode = (long) (((long) c << maxbits) + ent);
i = ((c << hshift) ^ ent);
if (htabof (i) == fcode) {
ent = codetabof(i);
continue;
} else if ((long)htabof(i) < 0 )
goto nomatch;
disp = hsize_reg - i;
if (i == 0)
disp = 1;
probe:
if ((i -= disp) < 0)
i += hsize_reg;
if (htabof (i) == fcode) {
ent = codetabof(i);
continue;
}
if ((long)htabof(i) > 0)
goto probe;
nomatch:
output((code_int)ent);
out_count++;
ent = c;
if (free_ent < maxmaxcode) {
codetabof(i) = free_ent++;
htabof(i) = fcode;
} else if ((count_int)in_count >= checkpoint && block_compress)
cl_block ();
}
output( (code_int)ent );
out_count++;
output( (code_int)-1 );
if(zcat_flg == 0 && !quiet) {
#ifdef DEBUG
fprintf( stderr,
"%ld chars in, %ld codes (%ld bytes) out, compression factor: ",
in_count, out_count, bytes_out );
prratio( stderr, in_count, bytes_out );
fprintf( stderr, "\n");
fprintf( stderr, "\tCompression as in compact: " );
prratio( stderr, in_count-bytes_out, in_count );
fprintf( stderr, "\n");
fprintf( stderr, "\tLargest code (of last block) was %d (%d bits)\n",
free_ent - 1, n_bits );
#else
fprintf( stderr, "Compression: " );
prratio( stderr, in_count-bytes_out, in_count );
#endif
}
if(bytes_out > in_count)
exit_stat = 2;
}
static char buf[BITS];
uchar lmask[9] = {0xff, 0xfe, 0xfc, 0xf8, 0xf0, 0xe0, 0xc0, 0x80, 0x00};
uchar rmask[9] = {0x00, 0x01, 0x03, 0x07, 0x0f, 0x1f, 0x3f, 0x7f, 0xff};
void
output( code )
code_int  code;
{
#ifdef DEBUG
static int col = 0;
#endif
int r_off = offset, bits= n_bits;
char *bp = buf;
#ifdef DEBUG
if (verbose)
fprintf(stderr, "%5d%c", code,
(col+=6) >= 74? (col = 0, '\n'): ' ');
#endif
if (code >= 0) {
bp += (r_off >> 3);
r_off &= 7;
*bp = (*bp & rmask[r_off]) | (code << r_off) & lmask[r_off];
bp++;
bits -=  8 - r_off;
code >>= 8 - r_off;
if ( bits >= 8 ) {
*bp++ = code;
code >>= 8;
bits -= 8;
}
if(bits)
*bp = code;
offset += n_bits;
if ( offset == (n_bits << 3) ) {
bp = buf;
bits = n_bits;
bytes_out += bits;
do {
putchar(*bp++);
} while(--bits);
offset = 0;
}
if ( free_ent > maxcode || (clear_flg > 0)) {
if ( offset > 0 ) {
if( fwrite( buf, 1, n_bits, stdout ) != n_bits)
writeerr();
bytes_out += n_bits;
}
offset = 0;
if ( clear_flg ) {
maxcode = MAXCODE (n_bits = INIT_BITS);
clear_flg = 0;
} else {
n_bits++;
if ( n_bits == maxbits )
maxcode = maxmaxcode;
else
maxcode = MAXCODE(n_bits);
}
#ifdef DEBUG
if ( debug ) {
fprintf(stderr,
"\nChange to %d bits\n", n_bits);
col = 0;
}
#endif
}
} else {
if ( offset > 0 )
fwrite( buf, 1, (offset + 7) / 8, stdout );
bytes_out += (offset + 7) / 8;
offset = 0;
fflush( stdout );
#ifdef DEBUG
if ( verbose )
fprintf( stderr, "\n" );
#endif
if( ferror( stdout ) )
writeerr();
}
}
void
decompress(void)
{
int finchar;
code_int code, oldcode, incode;
uchar *stackp;
maxcode = MAXCODE(n_bits = INIT_BITS);
for (code = 255; code >= 0; code--) {
tab_prefixof(code) = 0;
tab_suffixof(code) = (uchar)code;
}
free_ent = (block_compress? FIRST: 256);
finchar = oldcode = getcode();
if(oldcode == -1)
return;
putchar((char)finchar);
if(ferror(stdout))
writeerr();
stackp = de_stack;
while ((code = getcode()) > -1) {
if ((code == CLEAR) && block_compress) {
for (code = 255; code >= 0; code--)
tab_prefixof(code) = 0;
clear_flg = 1;
free_ent = FIRST - 1;
if ((code = getcode()) == -1)
break;
}
incode = code;
if (code >= free_ent) {
*stackp++ = finchar;
code = oldcode;
}
while (code >= 256) {
*stackp++ = tab_suffixof(code);
code = tab_prefixof(code);
}
*stackp++ = finchar = tab_suffixof(code);
do {
putchar(*--stackp);
} while (stackp > de_stack);
if ( (code=free_ent) < maxmaxcode ) {
tab_prefixof(code) = (ushort)oldcode;
tab_suffixof(code) = finchar;
free_ent = code+1;
}
oldcode = incode;
}
fflush(stdout);
if(ferror(stdout))
writeerr();
}
code_int
getcode()
{
int r_off, bits;
code_int code;
static int offset = 0, size = 0;
static uchar buf[BITS];
uchar *bp = buf;
if ( clear_flg > 0 || offset >= size || free_ent > maxcode ) {
if ( free_ent > maxcode ) {
n_bits++;
if ( n_bits == maxbits )
maxcode = maxmaxcode;
else
maxcode = MAXCODE(n_bits);
}
if ( clear_flg > 0) {
maxcode = MAXCODE(n_bits = INIT_BITS);
clear_flg = 0;
}
size = fread(buf, 1, n_bits, stdin);
if (size <= 0)
return -1;
offset = 0;
size = (size << 3) - (n_bits - 1);
}
r_off = offset;
bits = n_bits;
bp += (r_off >> 3);
r_off &= 7;
code = (*bp++ >> r_off);
bits -= (8 - r_off);
r_off = 8 - r_off;
if (bits >= 8) {
code |= *bp++ << r_off;
r_off += 8;
bits -= 8;
}
code |= (*bp & rmask[bits]) << r_off;
offset += n_bits;
return code;
}
#ifdef DEBUG
printcodes()
{
code_int code;
int col = 0, bits;
bits = n_bits = INIT_BITS;
maxcode = MAXCODE(n_bits);
free_ent = ((block_compress) ? FIRST : 256 );
while ( ( code = getcode() ) >= 0 ) {
if ( (code == CLEAR) && block_compress ) {
free_ent = FIRST - 1;
clear_flg = 1;
}
else if ( free_ent < maxmaxcode )
free_ent++;
if ( bits != n_bits ) {
fprintf(stderr, "\nChange to %d bits\n", n_bits );
bits = n_bits;
col = 0;
}
fprintf(stderr, "%5d%c", code, (col+=6) >= 74 ? (col = 0, '\n') : ' ' );
}
putc( '\n', stderr );
exit( 0 );
}
code_int sorttab[1<<BITS];
#define STACK_SIZE	15000
dump_tab()
{
int i, first, c, ent;
int stack_top = STACK_SIZE;
if(do_decomp == 0) {
int flag = 1;
for(i=0; i<hsize; i++) {
if((long)htabof(i) >= 0) {
sorttab[codetabof(i)] = i;
}
}
first = block_compress ? FIRST : 256;
for(i = first; i < free_ent; i++) {
fprintf(stderr, "%5d: \"", i);
de_stack[--stack_top] = '\n';
de_stack[--stack_top] = '"';
stack_top = in_stack((htabof(sorttab[i])>>maxbits)&0xff,
stack_top);
for(ent=htabof(sorttab[i]) & ((1<<maxbits)-1);
ent > 256;
ent=htabof(sorttab[ent]) & ((1<<maxbits)-1)) {
stack_top = in_stack(htabof(sorttab[ent]) >> maxbits,
stack_top);
}
stack_top = in_stack(ent, stack_top);
fwrite( &de_stack[stack_top], 1, STACK_SIZE-stack_top, stderr);
stack_top = STACK_SIZE;
}
} else if(!debug) {
for ( i = 0; i < free_ent; i++ ) {
ent = i;
c = tab_suffixof(ent);
if ( isascii(c) && isprint(c) )
fprintf( stderr, "%5d: %5d/'%c'  \"",
ent, tab_prefixof(ent), c );
else
fprintf( stderr, "%5d: %5d/\\%03o \"",
ent, tab_prefixof(ent), c );
de_stack[--stack_top] = '\n';
de_stack[--stack_top] = '"';
for ( ; ent != NULL;
ent = (ent >= FIRST ? tab_prefixof(ent) : NULL) ) {
stack_top = in_stack(tab_suffixof(ent), stack_top);
}
fwrite( &de_stack[stack_top], 1, STACK_SIZE - stack_top, stderr );
stack_top = STACK_SIZE;
}
}
}
int
in_stack(int c, int stack_top)
{
if ( (isascii(c) && isprint(c) && c != '\\') || c == ' ' ) {
de_stack[--stack_top] = c;
} else {
switch( c ) {
case '\n': de_stack[--stack_top] = 'n'; break;
case '\t': de_stack[--stack_top] = 't'; break;
case '\b': de_stack[--stack_top] = 'b'; break;
case '\f': de_stack[--stack_top] = 'f'; break;
case '\r': de_stack[--stack_top] = 'r'; break;
case '\\': de_stack[--stack_top] = '\\'; break;
default:
de_stack[--stack_top] = '0' + c % 8;
de_stack[--stack_top] = '0' + (c / 8) % 8;
de_stack[--stack_top] = '0' + c / 64;
break;
}
de_stack[--stack_top] = '\\';
}
return stack_top;
}
#endif
void
writeerr(void)
{
perror(ofname);
unlink(ofname);
exit(1);
}
void
copystat(ifname, ofname)
char *ifname, *ofname;
{
int mode;
time_t timep[2];
struct stat statbuf;
fclose(stdout);
if (stat(ifname, &statbuf)) {
perror(ifname);
return;
}
if (!S_ISREG(statbuf.st_mode)) {
if (quiet)
fprintf(stderr, "%s: ", ifname);
fprintf(stderr, " -- not a regular file: unchanged");
exit_stat = 1;
} else if (exit_stat == 2 && !force) {
if (!quiet)
fprintf(stderr, " -- file unchanged");
} else {
exit_stat = 0;
mode = statbuf.st_mode & 0777;
if (chmod(ofname, mode))
perror(ofname);
chown(ofname, statbuf.st_uid, statbuf.st_gid);
timep[0] = statbuf.st_atime;
timep[1] = statbuf.st_mtime;
utime(ofname, (struct utimbuf *)timep);
return;
}
if (unlink(ofname))
perror(ofname);
}
int
foreground(void)
{
if(bgnd_flag)
return 0;
else
return isatty(2);
}
void
onintr(int x)
{
USED(x);
unlink(ofname);
exit(1);
}
void
oops(int x)
{
USED(x);
if (do_decomp == 1)
fprintf(stderr, "uncompress: corrupt input\n");
unlink(ofname);
exit(1);
}
void
cl_block(void)
{
long rat;
checkpoint = in_count + CHECK_GAP;
#ifdef DEBUG
if ( debug ) {
fprintf ( stderr, "count: %ld, ratio: ", in_count );
prratio ( stderr, in_count, bytes_out );
fprintf ( stderr, "\n");
}
#endif
if (in_count > 0x007fffff) {
rat = bytes_out >> 8;
if (rat == 0)
rat = 0x7fffffff;
else
rat = in_count / rat;
} else
rat = (in_count << 8) / bytes_out;
if (rat > ratio)
ratio = rat;
else {
ratio = 0;
#ifdef DEBUG
if (verbose)
dump_tab();
#endif
cl_hash((count_int)hsize);
free_ent = FIRST;
clear_flg = 1;
output((code_int)CLEAR);
#ifdef DEBUG
if (debug)
fprintf(stderr, "clear\n");
#endif
}
}
void
cl_hash(count_int hsize)
{
count_int *htab_p = htab+hsize;
long i;
long m1 = -1;
i = hsize - 16;
do {
*(htab_p-16) = m1;
*(htab_p-15) = m1;
*(htab_p-14) = m1;
*(htab_p-13) = m1;
*(htab_p-12) = m1;
*(htab_p-11) = m1;
*(htab_p-10) = m1;
*(htab_p-9) = m1;
*(htab_p-8) = m1;
*(htab_p-7) = m1;
*(htab_p-6) = m1;
*(htab_p-5) = m1;
*(htab_p-4) = m1;
*(htab_p-3) = m1;
*(htab_p-2) = m1;
*(htab_p-1) = m1;
htab_p -= 16;
} while ((i -= 16) >= 0);
for ( i += 16; i > 0; i-- )
*--htab_p = m1;
}
void
prratio(stream, num, den)
FILE *stream;
long num, den;
{
int q;
if(num > 214748L)
q = num / (den / 10000L);
else
q = 10000L * num / den;
if (q < 0) {
putc('-', stream);
q = -q;
}
fprintf(stream, "%d.%02d%%", q / 100, q % 100);
}
void
version(void)
{
fprintf(stderr, "%s\n", rcs_ident);
fprintf(stderr, "Options: ");
#ifdef DEBUG
fprintf(stderr, "DEBUG, ");
#endif
#ifdef BSD4_2
fprintf(stderr, "BSD4_2, ");
#endif
fprintf(stderr, "BITS = %d\n", BITS);
}