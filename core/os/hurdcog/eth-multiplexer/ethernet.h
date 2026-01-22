#ifndef ETHERNET_H
#define ETHERNET_H
#include <mach.h>
#include <net/if_ether.h>
#include <netinet/in.h>
extern mach_port_t ether_port;
extern char ether_address[ETH_ALEN];
int ethernet_open (char *dev_name, device_t master_device,
struct port_bucket *etherport_bucket,
struct port_class *etherreadclass);
int ethernet_close (char *dev_name);
int ethernet_demuxer (mach_msg_header_t *inp,
mach_msg_header_t *outp);
error_t eth_set_clear_flags (int set_flags, int clear_flags);
#endif