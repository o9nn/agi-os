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
#define No_of_ports 8
#define All_ports (No_of_ports + 1)
#define FDB_TIMEOUT 300
#define BRIDGE_MAX_AGE 20
#define BRIDGE_HELLO_TIME 2
#define BRIDGE_FORWARD_DELAY 15
#define HOLD_TIME 1
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
typedef struct {
unsigned short protocol_id;
unsigned char protocol_version_id;
unsigned char type;
unsigned char flags;
#define TOPOLOGY_CHANGE 0x01
#define TOPOLOGY_CHANGE_ACK 0x80
bridge_id_t root_id;
unsigned int root_path_cost;
bridge_id_t bridge_id;
unsigned short port_id;
unsigned short message_age;
unsigned short max_age;
unsigned short hello_time;
unsigned short forward_delay;
} Config_bpdu;
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
unsigned int topology_change_detected;
unsigned int topology_change;
unsigned short topology_change_time;
unsigned short hold_time;
unsigned int top_change;
unsigned int top_change_detected;
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
unsigned int flags;
#define FDB_ENT_VALID 0x01
short fdb_avl_height;
struct fdb *fdb_avl_left;
struct fdb *fdb_avl_right;
struct fdb *fdb_next;
};
#define IS_BRIDGED 0x2e
struct br_stat {
unsigned int flags;
Bridge_data bridge_data;
Port_data port_data[No_of_ports];
};
#define BR_UP 0x0001
#define BR_DEBUG 0x0002
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
void transmit_config(int port_no);
int root_bridge(void);
int supersedes_port_info(int port_no, Config_bpdu *config);
void record_config_information(int port_no, Config_bpdu *config);
void record_config_timeout_values(Config_bpdu *config);
void config_bpdu_generation(void);
int designated_port(int port_no);
void reply(int port_no);
void transmit_tcn(void);
void configuration_update(void);
void root_selection(void);
void designated_port_selection(void);
void become_designated_port(int port_no);
void port_state_selection(void);
void make_forwarding(int port_no);
void topology_change_detection(void);
void topology_change_acknowledged(void);
void acknowledge_topology_change(int port_no);
void make_blocking(int port_no);
void set_port_state(int port_no, int state);
void received_config_bpdu(int port_no, Config_bpdu *config);
void received_tcn_bpdu(int port_no, Tcn_bpdu *tcn);
void hello_timer_expiry(void);
void message_age_timer_expiry(int port_no);
void forward_delay_timer_expiry(int port_no);
int designated_for_some_port(void);
void tcn_timer_expiry(void);
void topology_change_timer_expiry(void);
void hold_timer_expiry(int port_no);
void br_init(void);
void br_init_port(int port_no);
void enable_port(int port_no);
void disable_port(int port_no);
void set_bridge_priority(bridge_id_t *new_bridge_id);
void set_port_priority(int port_no, unsigned short new_port_id);
void set_path_cost(int port_no, unsigned short path_cost);
void start_hello_timer(void);
void stop_hello_timer(void);
int hello_timer_expired(void);
void start_tcn_timer(void);
void stop_tcn_timer(void);
int tcn_timer_expired(void);
void start_topology_change_timer(void);
void stop_topology_change_timer(void);
int topology_change_timer_expired(void);
void start_message_age_timer(int port_no, unsigned short message_age);
void stop_message_age_timer(int port_no);
int message_age_timer_expired(int port_no);
void start_forward_delay_timer(int port_no);
void stop_forward_delay_timer(int port_no);
int forward_delay_timer_expired(int port_no);
void start_hold_timer(int port_no);
void stop_hold_timer(int port_no);
int hold_timer_expired(int port_no);
struct fdb *br_avl_find_addr(unsigned char addr[6]);
int br_avl_insert (struct fdb * new_node);
int br_avl_remove (struct fdb * node_to_delete);
int send_tcn_bpdu(int port_no, Tcn_bpdu *bpdu);
int send_config_bpdu(int port_no, Config_bpdu *config_bpdu);
int find_port(struct device *dev);
int br_flood(struct sk_buff *skb, int port);
int br_drop(struct sk_buff *skb);
int br_learn(struct sk_buff *skb, int port);
int br_receive_frame(struct sk_buff *skb);
int br_tx_frame(struct sk_buff *skb);
int br_ioctl(unsigned int cmd, void *arg);
void free_fdb(struct fdb *);
struct fdb *get_fdb(void);
extern struct br_stat br_stats;