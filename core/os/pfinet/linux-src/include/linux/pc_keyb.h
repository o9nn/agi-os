#undef KBD_REPORT_ERR
#define KBD_REPORT_UNKN
#define KBD_REPORT_TIMEOUTS
#undef KBD_IS_FOCUS_9000
#undef INITIALIZE_MOUSE
#define KBD_INIT_TIMEOUT 1000
#define KBC_TIMEOUT 250
#define KBD_TIMEOUT 1000
extern unsigned char pckbd_read_mask;
extern unsigned char aux_device_present;
#define KBD_STATUS_REG 0x64
#define KBD_CNTL_REG 0x64
#define KBD_DATA_REG 0x60
#define KBD_CCMD_READ_MODE 0x20
#define KBD_CCMD_WRITE_MODE 0x60
#define KBD_CCMD_GET_VERSION 0xA1
#define KBD_CCMD_MOUSE_DISABLE 0xA7
#define KBD_CCMD_MOUSE_ENABLE 0xA8
#define KBD_CCMD_TEST_MOUSE 0xA9
#define KBD_CCMD_SELF_TEST 0xAA
#define KBD_CCMD_KBD_TEST 0xAB
#define KBD_CCMD_KBD_DISABLE 0xAD
#define KBD_CCMD_KBD_ENABLE 0xAE
#define KBD_CCMD_WRITE_AUX_OBUF 0xD3
#define KBD_CCMD_WRITE_MOUSE 0xD4
#define KBD_CMD_SET_LEDS 0xED
#define KBD_CMD_SET_RATE 0xF3
#define KBD_CMD_ENABLE 0xF4
#define KBD_CMD_DISABLE 0xF5
#define KBD_CMD_RESET 0xFF
#define KBD_REPLY_POR 0xAA
#define KBD_REPLY_ACK 0xFA
#define KBD_REPLY_RESEND 0xFE
#define KBD_STAT_OBF 0x01
#define KBD_STAT_IBF 0x02
#define KBD_STAT_SELFTEST 0x04
#define KBD_STAT_CMD 0x08
#define KBD_STAT_UNLOCKED 0x10
#define KBD_STAT_MOUSE_OBF 0x20
#define KBD_STAT_GTO 0x40
#define KBD_STAT_PERR 0x80
#define AUX_STAT_OBF (KBD_STAT_OBF | KBD_STAT_MOUSE_OBF)
#define KBD_MODE_KBD_INT 0x01
#define KBD_MODE_MOUSE_INT 0x02
#define KBD_MODE_SYS 0x04
#define KBD_MODE_NO_KEYLOCK 0x08
#define KBD_MODE_DISABLE_KBD 0x10
#define KBD_MODE_DISABLE_MOUSE 0x20
#define KBD_MODE_KCC 0x40
#define KBD_MODE_RFU 0x80
#define AUX_SET_RES 0xE8
#define AUX_SET_SCALE11 0xE6
#define AUX_SET_SCALE21 0xE7
#define AUX_GET_SCALE 0xE9
#define AUX_SET_STREAM 0xEA
#define AUX_SET_SAMPLE 0xF3
#define AUX_ENABLE_DEV 0xF4
#define AUX_DISABLE_DEV 0xF5
#define AUX_RESET 0xFF
#define AUX_ACK 0xFA
#define AUX_BUF_SIZE 2048
struct aux_queue {
unsigned long head;
unsigned long tail;
struct wait_queue *proc_list;
struct fasync_struct *fasync;
unsigned char buf[AUX_BUF_SIZE];
};