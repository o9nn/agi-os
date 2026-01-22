#ifndef LAME_PORTABLEIO_H
#define LAME_PORTABLEIO_H
#include	<stdio.h>
#ifndef	__cplusplus
# define	CLINK
#else
# define	CLINK "C"
#endif
extern CLINK int ReadByte(FILE *fp);
extern CLINK int Read16BitsLowHigh(FILE *fp);
extern CLINK int Read16BitsHighLow(FILE *fp);
extern CLINK void Write8Bits(FILE *fp, int i);
extern CLINK void Write16BitsLowHigh(FILE *fp, int i);
extern CLINK void Write16BitsHighLow(FILE *fp, int i);
extern CLINK int Read24BitsHighLow(FILE *fp);
extern CLINK int Read32Bits(FILE *fp);
extern CLINK int Read32BitsHighLow(FILE *fp);
extern CLINK void Write32Bits(FILE *fp, int i);
extern CLINK void Write32BitsLowHigh(FILE *fp, int i);
extern CLINK void Write32BitsHighLow(FILE *fp, int i);
extern CLINK void ReadBytes(FILE *fp, char *p, int n);
extern CLINK void ReadBytesSwapped(FILE *fp, char *p, int n);
extern CLINK void WriteBytes(FILE *fp, char *p, int n);
extern CLINK void WriteBytesSwapped(FILE *fp, char *p, int n);
extern CLINK double ReadIeeeFloatHighLow(FILE *fp);
extern CLINK double ReadIeeeFloatLowHigh(FILE *fp);
extern CLINK double ReadIeeeDoubleHighLow(FILE *fp);
extern CLINK double ReadIeeeDoubleLowHigh(FILE *fp);
extern CLINK double ReadIeeeExtendedHighLow(FILE *fp);
extern CLINK double ReadIeeeExtendedLowHigh(FILE *fp);
extern CLINK void WriteIeeeFloatLowHigh(FILE *fp, double num);
extern CLINK void WriteIeeeFloatHighLow(FILE *fp, double num);
extern CLINK void WriteIeeeDoubleLowHigh(FILE *fp, double num);
extern CLINK void WriteIeeeDoubleHighLow(FILE *fp, double num);
extern CLINK void WriteIeeeExtendedLowHigh(FILE *fp, double num);
extern CLINK void WriteIeeeExtendedHighLow(FILE *fp, double num);
#define	Read32BitsLowHigh(f)	Read32Bits(f)
#define WriteString(f,s)	fwrite(s,strlen(s),sizeof(char),f)
#endif