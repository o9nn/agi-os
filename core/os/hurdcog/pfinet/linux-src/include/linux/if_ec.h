#ifndef __LINUX_IF_EC
#define __LINUX_IF_EC
struct ec_addr
{
unsigned char station;
unsigned char net;
};
struct sockaddr_ec
{
unsigned short sec_family;
unsigned char port;
unsigned char cb;
unsigned char type;
struct ec_addr addr;
unsigned long cookie;
};
#define ECTYPE_PACKET_RECEIVED 0
#define ECTYPE_TRANSMIT_STATUS 0x10
#define ECTYPE_TRANSMIT_OK 1
#define ECTYPE_TRANSMIT_NOT_LISTENING 2
#define ECTYPE_TRANSMIT_NET_ERROR 3
#define ECTYPE_TRANSMIT_NO_CLOCK 4
#define ECTYPE_TRANSMIT_LINE_JAMMED 5
#define ECTYPE_TRANSMIT_NOT_PRESENT 6
#ifdef __KERNEL__
struct econet_opt
{
unsigned char cb;
unsigned char port;
unsigned char station;
unsigned char net;
};
#endif
#endif