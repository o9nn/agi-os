#ifndef __osockaddr_defined
#define __osockaddr_defined 1
struct osockaddr
{
unsigned short int sa_family;
unsigned char sa_data[14];
};
#endif