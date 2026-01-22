#ifndef OPTIONS_H
#define OPTIONS_H
#include <stdint.h>
#include <string.h>
#include <sys/types.h>
#include <argp.h>
#include <lwip/ip.h>
#include <lwip/netif.h>
#define DEV_NAME_LEN    256
struct parse_interface
{
char dev_name[DEV_NAME_LEN];
ip4_addr_t address, netmask, peer, gateway;
uint32_t addr6[LWIP_IPV6_NUM_ADDRESSES][4];
};
struct parse_hook
{
struct parse_interface *interfaces;
size_t num_interfaces;
struct parse_interface *curint;
};
static const struct argp_option options[] = {
{"interface", 'i', "DEVICE", 0, "Network interface to use", 1},
{0, 0, 0, 0, "These apply to a given interface:", 2},
{"address", 'a', "ADDRESS", OPTION_ARG_OPTIONAL, "Set the network address"},
{"netmask", 'm', "MASK", OPTION_ARG_OPTIONAL, "Set the netmask"},
{"gateway", 'g', "ADDRESS", OPTION_ARG_OPTIONAL, "Set the default gateway"},
{"ipv4", '4', "NAME", 0, "Put active IPv4 translator on NAME"},
{"ipv6", '6', "NAME", 0, "Put active IPv6 translator on NAME"},
{"address6", 'A', "ADDR/LEN", OPTION_ARG_OPTIONAL,
"Set the global IPv6 address"},
{0}
};
static const char doc[] = "Interface-specific options before the first \
interface specification apply to the first following interface; otherwise \
they apply to the previously specified interface.";
extern struct argp lwip_argp;
#endif