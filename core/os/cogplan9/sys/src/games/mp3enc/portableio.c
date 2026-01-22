#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <stdio.h>
#if defined(__riscos__) && defined(FPA10)
#include "ymath.h"
#else
#include <math.h>
#endif
#include "portableio.h"
#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
#ifdef KLEMM_36
signed int ReadByte ( FILE* fp )
{
int result = getc (fp);
return result == EOF ? 0 : (signed char) (result & 0xFF);
}
unsigned int ReadByteUnsigned ( FILE* fp )
{
int result = getc (fp);
return result == EOF ? 0 : (unsigned char) (result & 0xFF);
}
#else
int
ReadByte(FILE *fp)
{
int result;
result = getc(fp) & 0xff;
if (result & 0x80)
result = result - 0x100;
return result;
}
#endif
#ifdef KLEMM_36
int Read16BitsLowHigh ( FILE* fp )
{
int low = ReadByteUnsigned (fp);
int high = ReadByte (fp);
return (high << 8) | low;
}
#else
int
Read16BitsLowHigh(FILE *fp)
{
int first, second, result;
first = 0xff & getc(fp);
second = 0xff & getc(fp);
result = (second << 8) + first;
#ifndef THINK_C42
if (result & 0x8000)
result = result - 0x10000;
#endif
return(result);
}
#endif
#ifdef KLEMM_36
int Read16BitsHighLow ( FILE* fp )
{
int high = ReadByte (fp);
int low = ReadByteUnsigned (fp);
return (high << 8) | low;
}
#else
int
Read16BitsHighLow(FILE *fp)
{
int first, second, result;
first = 0xff & getc(fp);
second = 0xff & getc(fp);
result = (first << 8) + second;
#ifndef THINK_C42
if (result & 0x8000)
result = result - 0x10000;
#endif
return(result);
}
#endif
void
Write8Bits(FILE *fp, int i)
{
putc(i&0xff,fp);
}
void
Write16BitsLowHigh(FILE *fp, int i)
{
putc(i&0xff,fp);
putc((i>>8)&0xff,fp);
}
void
Write16BitsHighLow(FILE *fp, int i)
{
putc((i>>8)&0xff,fp);
putc(i&0xff,fp);
}
#ifdef KLEMM_36
int Read24BitsHighLow ( FILE* fp )
{
int high = ReadByte (fp);
int med = ReadByteUnsigned (fp);
int low = ReadByteUnsigned (fp);
return (high << 16) | (med << 8) | low;
}
#else
int
Read24BitsHighLow(FILE *fp)
{
int first, second, third;
int result;
first = 0xff & getc(fp);
second = 0xff & getc(fp);
third = 0xff & getc(fp);
result = (first << 16) + (second << 8) + third;
if (result & 0x800000)
result = result - 0x1000000;
return(result);
}
#endif
#define Read32BitsLowHigh(f) Read32Bits(f)
#ifdef KLEMM_36
int Read32Bits ( FILE* fp )
{
int low = ReadByteUnsigned (fp);
int medl = ReadByteUnsigned (fp);
int medh = ReadByteUnsigned (fp);
int high = ReadByte (fp);
return (high << 24) | (medh << 16) | (medl << 8) | low;
}
#else
int
Read32Bits(FILE *fp)
{
int first, second, result;
first = 0xffff & Read16BitsLowHigh(fp);
second = 0xffff & Read16BitsLowHigh(fp);
result = (second << 16) + first;
#ifdef CRAY
if (result & 0x80000000)
result = result - 0x100000000;
#endif
return(result);
}
#endif
#ifdef KLEMM_36
int Read32BitsHighLow ( FILE* fp )
{
int high = ReadByte (fp);
int medh = ReadByteUnsigned (fp);
int medl = ReadByteUnsigned (fp);
int low = ReadByteUnsigned (fp);
return (high << 24) | (medh << 16) | (medl << 8) | low;
}
#else
int
Read32BitsHighLow(FILE *fp)
{
int first, second, result;
first = 0xffff & Read16BitsHighLow(fp);
second = 0xffff & Read16BitsHighLow(fp);
result = (first << 16) + second;
#ifdef CRAY
if (result & 0x80000000)
result = result - 0x100000000;
#endif
return(result);
}
#endif
void
Write32Bits(FILE *fp, int i)
{
Write16BitsLowHigh(fp,(int)(i&0xffffL));
Write16BitsLowHigh(fp,(int)((i>>16)&0xffffL));
}
void
Write32BitsLowHigh(FILE *fp, int i)
{
Write16BitsLowHigh(fp,(int)(i&0xffffL));
Write16BitsLowHigh(fp,(int)((i>>16)&0xffffL));
}
void
Write32BitsHighLow(FILE *fp, int i)
{
Write16BitsHighLow(fp,(int)((i>>16)&0xffffL));
Write16BitsHighLow(fp,(int)(i&0xffffL));
}
#ifdef KLEMM_36
void ReadBytes (FILE *fp, char *p, int n)
{
memset ( p, 0, n );
fread ( p, 1, n, fp );
}
#else
void ReadBytes(FILE *fp, char *p, int n)
{
while (!feof(fp) & (n-- > 0))
*p++ = getc(fp);
}
#endif
void ReadBytesSwapped(FILE *fp, char *p, int n)
{
register char *q = p;
while (!feof(fp) & (n-- > 0))
*q++ = getc(fp);
for (q--; p < q; p++, q--){
n = *p;
*p = *q;
*q = n;
}
}
#ifdef KLEMM_36
void WriteBytes(FILE *fp, char *p, int n)
{
fwrite ( p, 1, n, fp );
}
#else
void WriteBytes(FILE *fp, char *p, int n)
{
while (n-- > 0)
putc(*p++, fp);
}
#endif
#ifdef KLEMM_36
void WriteBytesSwapped(FILE *fp, char *p, int n)
{
p += n;
while ( n-- > 0 )
putc ( *--p, fp );
}
#else
void WriteBytesSwapped(FILE *fp, char *p, int n)
{
p += n-1;
while (n-- > 0)
putc(*p--, fp);
}
#endif
#ifdef applec
# define FloatToUnsigned(f) ((unsigned long)(f))
# define UnsignedToFloat(u) ((double)(u))
#else
# define FloatToUnsigned(f) ((unsigned long)(((long)((f) - 2147483648.0)) + 2147483647L + 1))
# define UnsignedToFloat(u) (((double)((long)((u) - 2147483647L - 1))) + 2147483648.0)
#endif
double
ConvertFromIeeeExtended(char* bytes)
{
double f;
long expon;
unsigned long hiMant, loMant;
#ifdef TEST
printf("ConvertFromIEEEExtended(%lx,%lx,%lx,%lx,%lx,%lx,%lx,%lx,%lx,%lx\r",
(long)bytes[0], (long)bytes[1], (long)bytes[2], (long)bytes[3],
(long)bytes[4], (long)bytes[5], (long)bytes[6],
(long)bytes[7], (long)bytes[8], (long)bytes[9]);
#endif
expon = ((bytes[0] & 0x7F) << 8) | (bytes[1] & 0xFF);
hiMant = ((unsigned long)(bytes[2] & 0xFF) << 24)
| ((unsigned long)(bytes[3] & 0xFF) << 16)
| ((unsigned long)(bytes[4] & 0xFF) << 8)
| ((unsigned long)(bytes[5] & 0xFF));
loMant = ((unsigned long)(bytes[6] & 0xFF) << 24)
| ((unsigned long)(bytes[7] & 0xFF) << 16)
| ((unsigned long)(bytes[8] & 0xFF) << 8)
| ((unsigned long)(bytes[9] & 0xFF));
if (expon == 0 && hiMant == 0 && loMant == 0) {
f = 0;
}
else {
if (expon == 0x7FFF) {
f = HUGE_VAL;
}
else {
expon -= 16383;
f = ldexp(UnsignedToFloat(hiMant), (int) (expon -= 31));
f += ldexp(UnsignedToFloat(loMant), (int) (expon -= 32));
}
}
if (bytes[0] & 0x80)
return -f;
else
return f;
}
double
ReadIeeeExtendedHighLow(FILE *fp)
{
char bytes [10];
ReadBytes ( fp, bytes, 10 );
return ConvertFromIeeeExtended ( bytes );
}