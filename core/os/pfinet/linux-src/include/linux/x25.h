#ifndef X25_KERNEL_H
#define X25_KERNEL_H
#define SIOCX25GSUBSCRIP (SIOCPROTOPRIVATE + 0)
#define SIOCX25SSUBSCRIP (SIOCPROTOPRIVATE + 1)
#define SIOCX25GFACILITIES (SIOCPROTOPRIVATE + 2)
#define SIOCX25SFACILITIES (SIOCPROTOPRIVATE + 3)
#define SIOCX25GCALLUSERDATA (SIOCPROTOPRIVATE + 4)
#define SIOCX25SCALLUSERDATA (SIOCPROTOPRIVATE + 5)
#define SIOCX25GCAUSEDIAG (SIOCPROTOPRIVATE + 6)
#define X25_QBITINCL 1
#define X25_PS16 4
#define X25_PS32 5
#define X25_PS64 6
#define X25_PS128 7
#define X25_PS256 8
#define X25_PS512 9
#define X25_PS1024 10
#define X25_PS2048 11
#define X25_PS4096 12
typedef struct {
char x25_addr[16];
} x25_address;
struct sockaddr_x25 {
sa_family_t sx25_family;
x25_address sx25_addr;
};
struct x25_subscrip_struct {
char device[200];
unsigned int extended;
};
struct x25_route_struct {
x25_address address;
unsigned int sigdigits;
char device[200];
};
struct x25_facilities {
unsigned int winsize_in, winsize_out;
unsigned int pacsize_in, pacsize_out;
unsigned int throughput;
unsigned int reverse;
};
struct x25_calluserdata {
unsigned int cudlength;
unsigned char cuddata[128];
};
struct x25_causediag {
unsigned char cause;
unsigned char diagnostic;
};
#endif