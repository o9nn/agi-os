struct rocket_config {
int line;
int flags;
int closing_wait;
int close_delay;
int port;
int reserved[32];
};
struct rocket_ports {
int tty_major;
int callout_major;
int port_bitmap[4];
int reserved[32];
};
#define ROCKET_CALLOUT_NOHUP 0x00000001
#define ROCKET_FORCE_CD 0x00000002
#define ROCKET_HUP_NOTIFY 0x00000004
#define ROCKET_SPLIT_TERMIOS 0x00000008
#define ROCKET_SPD_MASK 0x00000070
#define ROCKET_SPD_HI 0x00000010
#define ROCKET_SPD_VHI 0x00000020
#define ROCKET_SPD_SHI 0x00000030
#define ROCKET_SPD_WARP 0x00000040
#define ROCKET_SAK 0x00000080
#define ROCKET_SESSION_LOCKOUT 0x00000100
#define ROCKET_PGRP_LOCKOUT 0x00000200
#define ROCKET_FLAGS 0x000003FF
#define ROCKET_USR_MASK 0x0071
#define ROCKET_CLOSING_WAIT_NONE 65535
#define ROCKET_CLOSING_WAIT_INF 0
#define RCKP_GET_STRUCT 0x00525001
#define RCKP_GET_CONFIG 0x00525002
#define RCKP_SET_CONFIG 0x00525003
#define RCKP_GET_PORTS 0x00525004