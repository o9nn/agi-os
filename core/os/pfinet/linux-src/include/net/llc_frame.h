#include "linux/if_ether.h"
typedef union {
struct {
unsigned dsap : 8;
unsigned ssap : 8;
unsigned f1 : 1;
unsigned f2 : 1;
unsigned : 6;
unsigned : 8;
} pdu_hdr;
struct {
char dummy1[2];
char byte1;
char byte2;
} pdu_cntl;
struct {
unsigned char dummy2[2];
unsigned : 1;
unsigned ns : 7;
unsigned i_pflag : 1;
unsigned nr : 7;
unsigned char is_info[ ETH_DATA_LEN ];
}  i_hdr;
struct {
unsigned char dummy3[2];
unsigned : 2;
unsigned ss : 2;
unsigned : 4;
unsigned s_pflag : 1;
unsigned nr : 7;
} s_hdr;
struct {
unsigned char dummy4[2];
unsigned : 2;
unsigned mm1 : 2;
unsigned u_pflag : 1;
unsigned mm2 : 3;
unsigned char u_info[ ETH_DATA_LEN-1];
} u_hdr;
struct {
unsigned char dummy5[2];
unsigned : 2;
unsigned mm : 6;
} u_mm;
} frame_type, *frameptr;
#define IS_UFRAME( fr ) ( ( (fr)->pdu_hdr.f1) & ( (fr)->pdu_hdr.f2) )
#define IS_IFRAME( fr ) ( !( (fr)->pdu_hdr.f1) )
#define IS_SFRAME( fr ) ( ( (fr)->pdu_hdr.f1) & !( (fr)->pdu_hdr.f2) )
#define IS_RSP( fr ) ( fr->pdu_hdr.ssap & 0x01 )
#define I_CMD		0
#define RR_CMD		1
#define RNR_CMD		2
#define REJ_CMD		3
#define DISC_CMD	4
#define SABME_CMD	5
#define I_RSP		6
#define RR_RSP		7
#define RNR_RSP		8
#define REJ_RSP		9
#define UA_RSP		10
#define DM_RSP		11
#define FRMR_RSP	12
#define BAD_FRAME	13
#define NO_FRAME	13
#define UI_CMD		14
#define XID_CMD		15
#define TEST_CMD	16
#define XID_RSP		17
#define TEST_RSP	18