#if !defined(One)
#define Zero 0
#define One 1
#endif
#if !defined(TRUE)
#define FALSE 0
#define TRUE 1
#endif
#define Disabled 0
#define Listening 1
#define Learning 2
#define Forwarding 3
#define Blocking 4
#define BR_MAX_PORTS (32)
#if CONFIG_BRIDGE_NUM_PORTS > BR_MAX_PORTS
#undef CONFIG_BRIDGE_NUM_PORTS
#define CONFIG_BRIDGE_NUM_PORTS BR_MAX_PORTS
#endif
#define No_of_ports CONFIG_BRIDGE_NUM_PORTS
#define All_ports (No_of_ports + 1)
#define FDB_TIMEOUT 20
#define BRIDGE_MAX_AGE 20
#define BRIDGE_HELLO_TIME 2
#define BRIDGE_FORWARD_DELAY 15
#define HOLD_TIME 1
#define MAX_MCAST_PER_PERIOD 4
#define MCAST_HOLD_TIME (10*HZ/100)
#define Default_path_cost 10
#define Message_age_increment 1
#define No_port 0
typedef struct {
union {
struct {
unsigned short priority;
unsigned char ula[6];
} p_u;
unsigned int id[2];
} bi;
} bridge_id_t;
#define BRIDGE_PRIORITY bi.p_u.priority
#define BRIDGE_ID_ULA bi.p_u.ula
#define BRIDGE_ID bi.id
#define TOPOLOGY_CHANGE 0x01
#define TOPOLOGY_CHANGE_ACK 0x80
#define BRIDGE_BPDU_8021_CONFIG_SIZE 35
#define BRIDGE_BPDU_8021_CONFIG_FLAG_OFFSET 4
#define BRIDGE_BPDU_8021_PROTOCOL_ID 0
#define BRIDGE_BPDU_8021_PROTOCOL_VERSION_ID 0
#define BRIDGE_LLC1_HS 3
#define BRIDGE_LLC1_DSAP 0x42
#define BRIDGE_LLC1_SSAP 0x42
#define BRIDGE_LLC1_CTRL 0x03
typedef struct {
unsigned short protocol_id;
unsigned char protocol_version_id;
unsigned char type;
bridge_id_t root_id;
unsigned int root_path_cost;
bridge_id_t bridge_id;
unsigned short port_id;
unsigned short message_age;
unsigned short max_age;
unsigned short hello_time;
unsigned short forward_delay;
unsigned char top_change_ack;
unsigned char top_change;
} Config_bpdu;
#ifdef __LITTLE_ENDIAN
#define config_bpdu_hton(config_bpdu) \
(config_bpdu)->root_path_cost = htonl((config_bpdu)->root_path_cost); \
(config_bpdu)->port_id = htons((config_bpdu)->port_id); \
(config_bpdu)->message_age = htons((config_bpdu)->message_age); \
(config_bpdu)->max_age = htons((config_bpdu)->max_age); \
(config_bpdu)->hello_time = htons((config_bpdu)->hello_time); \
(config_bpdu)->forward_delay = htons((config_bpdu)->forward_delay);
#else
#define config_bpdu_hton(config_bpdu)
#endif
#define config_bpdu_ntoh config_bpdu_hton
typedef struct {
unsigned short protocol_id;
unsigned char protocol_version_id;
unsigned char type;
} Tcn_bpdu;
#define BPDU_TYPE_CONFIG 0
#define BPDU_TYPE_TOPO_CHANGE 128
typedef struct {
bridge_id_t designated_root;
unsigned int root_path_cost;
unsigned int root_port;
unsigned short max_age;
unsigned short hello_time;
unsigned short forward_delay;
bridge_id_t bridge_id;
unsigned short bridge_max_age;
unsigned short bridge_hello_time;
unsigned short bridge_forward_delay;
unsigned int top_change_detected;
unsigned int top_change;
unsigned short topology_change_time;
unsigned short hold_time;
unsigned int instance;
} Bridge_data;
typedef struct {
unsigned short port_id;
unsigned int state;
unsigned int path_cost;
bridge_id_t designated_root;
unsigned int designated_cost;
bridge_id_t designated_bridge;
unsigned short designated_port;
unsigned int top_change_ack;
unsigned int config_pending;
bridge_id_t ifmac;
unsigned int admin_state;
char ifname[IFNAMSIZ];
struct device *dev;
struct fdb *fdb;
} Port_data;
typedef struct {
unsigned int active;
unsigned int value;
} Timer;
struct fdb {
unsigned char ula[6];
unsigned char pad[2];
unsigned short port;
unsigned int timer;
unsigned short flags;
#define FDB_ENT_VALID 0x01
unsigned short mcast_count;
unsigned int mcast_timer;
short fdb_avl_height;
struct fdb *fdb_avl_left;
struct fdb *fdb_avl_right;
struct fdb *fdb_next;
};
struct fdb_info {
unsigned char ula[6];
unsigned char port;
unsigned char flags;
unsigned int timer;
};
struct fdb_info_hdr {
int copied;
int not_copied;
int cmd_time;
};
#define IS_BRIDGED 0x2e
#define BR_MAX_PROTOCOLS 32
#define BR_MAX_PROT_STATS BR_MAX_PROTOCOLS
#define BR_ACCEPT 1
#define BR_REJECT 0
typedef struct {
int port_disable_up_stack;
int rcv_bpdu;
int notForwarding;
int forwarding_up_stack;
int unknown_state;
int port_disable;
int port_not_disable;
int local_multicast;
int forwarded_multicast;
int flood_unicast;
int aged_flood_unicast;
int forwarded_unicast;
int forwarded_unicast_up_stack;
int forwarded_ip_up_stack;
int forwarded_ip_up_stack_lie;
int arp_for_local_mac;
int drop_same_port;
int drop_same_port_aged;
int drop_multicast;
} br_stats_counter;
struct br_stat {
unsigned int flags;
Bridge_data bridge_data;
unsigned int policy;
unsigned int exempt_protocols;
unsigned short protocols[BR_MAX_PROTOCOLS];
unsigned short prot_id[BR_MAX_PROT_STATS];
unsigned int prot_counter[BR_MAX_PROT_STATS];
br_stats_counter packet_cnts;
unsigned int num_ports;
Port_data port_data[BR_MAX_PORTS + 1];
};
#define BR_UP 0x0001
#define BR_DEBUG 0x0002
#define BR_PROT_STATS 0x0004
#define BR_STP_DISABLED 0x0008
struct br_cf {
unsigned int cmd;
unsigned int arg1;
unsigned int arg2;
};
#define BRCMD_BRIDGE_ENABLE 1
#define BRCMD_BRIDGE_DISABLE 2
#define BRCMD_PORT_ENABLE 3
#define BRCMD_PORT_DISABLE 4
#define BRCMD_SET_BRIDGE_PRIORITY 5
#define BRCMD_SET_PORT_PRIORITY 6
#define BRCMD_SET_PATH_COST 7
#define BRCMD_DISPLAY_FDB 8
#define BRCMD_ENABLE_DEBUG 9
#define BRCMD_DISABLE_DEBUG 10
#define BRCMD_SET_POLICY 11
#define BRCMD_EXEMPT_PROTOCOL 12
#define BRCMD_ENABLE_PROT_STATS 13
#define BRCMD_DISABLE_PROT_STATS 14
#define BRCMD_ZERO_PROT_STATS 15
#define BRCMD_TOGGLE_STP 16
#define BRCMD_IF_ENABLE 17
#define BRCMD_IF_DISABLE 18
#define BRCMD_SET_IF_PRIORITY 19
#define BRCMD_SET_IF_PATH_COST 20
#ifdef __KERNEL__
void br_init(void);
int br_receive_frame(struct sk_buff *skb);
int br_tx_frame(struct sk_buff *skb);
int brg_init(void);
int br_ioctl(unsigned int cmd, void *arg);
void requeue_fdb(struct fdb *node, int new_port);
struct fdb *br_avl_find_addr(unsigned char addr[6]);
struct fdb *br_avl_insert (struct fdb * new_node);
void sprintf_avl (char **pbuffer, struct fdb * tree, off_t *pos,int* len, off_t offset, int length);
int br_tree_get_info(char *buffer, char **start, off_t offset, int length, int dummy);
void br_avl_delete_by_port(int port);
int br_call_bridge(struct sk_buff *skb, unsigned short type);
void br_spacedevice_register(void);
extern struct br_stat br_stats;
extern Port_data port_info[];
#endif