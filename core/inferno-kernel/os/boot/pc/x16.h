#define rAX 0
#define rCX 1
#define rDX 2
#define rBX 3
#define rSP 4
#define rBP 5
#define rSI 6
#define rDI 7
#define rAL 0
#define rCL 1
#define rDL 2
#define rBL 3
#define rAH 4
#define rCH 5
#define rDH 6
#define rBH 7
#define rES 0
#define rCS 1
#define rSS 2
#define rDS 3
#define rFS 4
#define rGS 5
#define xSI 4
#define xDI 5
#define xBP 6
#define xBX 7
#define rCR0 0
#define rCR2 2
#define rCR3 3
#define rCR4 4
#define OP(o, m, ro, rm) BYTE $o; \
BYTE $(((m)<<6)|((ro)<<3)|(rm))
#define OPrm(o, r, m) OP(o, 0x00, r, 0x06); \
WORD $m;
#define OPrr(o, r0, r1) OP(o, 0x03, r0, r1);
#define LW(m, rX) OPrm(0x8B, rX, m)
#define LXW(x, rI, r) OP(0x8B, 0x02, r, rI); \
WORD $x
#define LBPW(x, r) OP(0x8B, 0x02, r, xBP); \
WORD $x
#define LB(m, rB) OPrm(0x8A, rB, m)
#define LXB(x, rI, r) OP(0x8A, 0x01, r, rI); \
BYTE $x
#define LBPB(x, r) OP(0x8A, 0x01, r, xBP); \
BYTE $x
#define SW(rX, m) OPrm(0x89, rX, m)
#define SXW(r, x, rI) OP(0x89, 0x02, r, rI); \
WORD $x
#define SBPW(r, x) OP(0x89, 0x02, r, xBP); \
WORD $(x)
#define SBPWI(i, x) OP(0xC7, 0x01, 0, xBP); \
BYTE $(x); WORD $(i)
#define STB(rB, m) OPrm(0x88, rB, m)
#define SXB(r, x, rI) OP(0x88, 0x01, r, rI); \
BYTE $x
#define SBPB(r, x) OP(0x88, 0x01, r, xBP); \
BYTE $x
#define SBPBI(i, x) OP(0xC6, 0x01, 0, xBP); \
BYTE $(x); BYTE $(i)
#define LWI(i, rX) BYTE $(0xB8+rX); \
WORD $i;
#define LBI(i, rB) BYTE $(0xB0+rB); \
BYTE $i
#define MW(r0, r1) OPrr(0x89, r0, r1)
#define MFSR(rS, rX) OPrr(0x8C, rS, rX)
#define MTSR(rX, rS) OPrr(0x8E, rS, rX)
#define MFCR(rC, rX) BYTE $0x0F; \
OP(0x20, 0x03, rC, rX)
#define MTCR(rX, rC) BYTE $0x0F; \
OP(0x22, 0x03, rC, rX)
#define ADC(r0, r1) OPrr(0x11, r0, r1)
#define ADD(r0, r1) OPrr(0x01, r0, r1)
#define ADDI(i, r) OP(0x81, 0x03, 0x00, r); \
WORD $i;
#define AND(r0, r1) OPrr(0x21, r0, r1)
#define ANDI(i, r) OP(0x81, 0x03, 0x04, r); \
WORD $i;
#define CLR(r) OPrr(0x31, r, r)
#define CLRB(r) OPrr(0x30, r, r)
#define CMP(r0, r1) OPrr(0x39, r0, r1)
#define CMPI(i, r) OP(0x81, 0x03, 0x07, r); \
WORD $i;
#define CMPBR(r0, r1) OPrr(0x38, r0, r1)
#define DEC(r) BYTE $(0x48|r)
#define DIV(r) OPrr(0xF7, 0x06, r)
#define INC(r) BYTE $(0x40|r)
#define MUL(r) OPrr(0xF7, 0x04, r)
#define IMUL(r0, r1) BYTE $0x0F; \
OPrr(0xAF, r1, r0)
#define OR(r0, r1) OPrr(0x09, r0, r1)
#define ORB(r0, r1) OPrr(0x08, r0, r1)
#define ORI(i, r) OP(0x81, 0x03, 0x01, r); \
WORD $i;
#define ROLI(i, r) OPrr(0xC1, 0x00, r); \
BYTE $i;
#define SHLI(i, r) OPrr(0xC1, 0x04, r); \
BYTE $i;
#define SHLBI(i, r) OPrr(0xC0, 0x04, r); \
BYTE $i;
#define SHRI(i, r) OPrr(0xC1, 0x05, r); \
BYTE $i;
#define SHRBI(i, r) OPrr(0xC0, 0x05, r); \
BYTE $i;
#define SUB(r0, r1) OPrr(0x29, r0, r1)
#define SUBI(i, r) OP(0x81, 0x03, 0x05, r); \
WORD $i;
#define STOSW STOSL
#define CALL16(f) LWI(f, rDI); \
BYTE $0xFF; \
BYTE $0xD7;
#define FARJUMP16(s, o) BYTE $0xEA; \
WORD $o; WORD $s
#define FARJUMP32(s, o) BYTE $0x66; \
BYTE $0xEA; LONG $o; WORD $s
#define DELAY BYTE $0xEB; \
BYTE $0x00
#define BIOSCALL(b) INT $b
#define PEEKW BYTE $0x26; \
BYTE $0x8B; BYTE $0x07
#define POKEW BYTE $0x26; \
BYTE $0x89; BYTE $0x07
#define OUTPORTB(p, d) LBI(d, rAL); \
BYTE $0xE6; \
BYTE $p; DELAY
#define PUSHA BYTE $0x60
#define PUSHR(r) BYTE $(0x50|r)
#define PUSHS(rS) BYTE $(0x06|((rS)<<3))
#define PUSHI(i) BYTE $0x68; WORD $i;
#define POPA BYTE $0x61
#define POPR(r) BYTE $(0x58|r)
#define POPS(rS) BYTE $(0x07|((rS)<<3))
#define NOP BYTE $0x90
#define LGDT(gdtptr) BYTE $0x0F; \
BYTE $0x01; BYTE $0x16; \
WORD $gdtptr
#define OPSIZE BYTE $0x66