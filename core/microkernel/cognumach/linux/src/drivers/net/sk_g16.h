#ifndef SK_G16_H
#define SK_G16_H
#define CSR0_ERR 0x8000
#define CSR0_BABL 0x4000
#define CSR0_CERR 0x2000
#define CSR0_MISS 0x1000
#define CSR0_MERR 0x0800
#define CSR0_RINT 0x0400
#define CSR0_TINT 0x0200
#define CSR0_IDON 0x0100
#define CSR0_INTR 0x0080
#define CSR0_INEA 0x0040
#define CSR0_RXON 0x0020
#define CSR0_TXON 0x0010
#define CSR0_TDMD 0x0008
#define CSR0_STOP 0x0004
#define CSR0_STRT 0x0002
#define CSR0_INIT 0x0001
#define CSR0_CLRALL 0x7f00
#define CSR3_BSWAP 0x0004
#define CSR3_ACON 0x0002
#define CSR3_BCON 0x0001
#define MODE_PROM 0x8000
#define MODE_INTL 0x0040
#define MODE_DRTY 0x0020
#define MODE_COLL 0x0010
#define MODE_DTCR 0x0008
#define MODE_LOOP 0x0004
#define MODE_DTX 0x0002
#define MODE_DRX 0x0001
#define MODE_NORMAL 0x0000
#define RX_OWN 0x80
#define RX_ERR 0x40
#define RX_FRAM 0x20
#define RX_OFLO 0x10
#define RX_CRC 0x08
#define RX_BUFF 0x04
#define RX_STP 0x02
#define RX_ENP 0x01
#define TX_OWN 0x80
#define TX_ERR 0x40
#define TX_MORE 0x10
#define TX_ONE 0x08
#define TX_DEF 0x04
#define TX_STP 0x02
#define TX_ENP 0x01
#define TX_BUFF 0x8000
#define TX_UFLO 0x4000
#define TX_LCOL 0x1000
#define TX_LCAR 0x0400
#define TX_RTRY 0x0200
#define TX_TDR 0x003f
struct init_block
{
unsigned short mode;
unsigned char paddr[6];
unsigned char laddr[8];
unsigned int rdrp;
unsigned int tdrp;
};
struct rmd
{
union
{
unsigned long buffer;
struct
{
unsigned char unused[3];
unsigned volatile char status;
} s;
} u;
volatile short blen;
unsigned short mlen;
};
struct tmd
{
union
{
unsigned long buffer;
struct
{
unsigned char unused[3];
unsigned volatile char status;
} s;
} u;
unsigned short blen;
unsigned volatile short status2;
};
#endif