#ifndef _DEVICE_BPF_H_
#define _DEVICE_BPF_H_
#define BPF_ALIGNMENT sizeof(int)
#define BPF_WORDALIGN(x) (((x)+(BPF_ALIGNMENT-1))&~(BPF_ALIGNMENT-1))
struct bpf_version {
unsigned short bv_major;
unsigned short bv_minor;
};
#define BPF_MAJOR_VERSION 1
#define BPF_MINOR_VERSION 1
#define DLT_NULL 0
#define DLT_EN10MB 1
#define DLT_EN3MB 2
#define DLT_AX25 3
#define DLT_PRONET 4
#define DLT_CHAOS 5
#define DLT_IEEE802 6
#define DLT_ARCNET 7
#define DLT_SLIP 8
#define DLT_PPP 9
#define DLT_FDDI 10
#define BPF_BEGIN NETF_BPF
#define BPF_IN NETF_IN
#define BPF_OUT NETF_OUT
#define BPF_CLASS(code) ((code) & 0x07)
#define BPF_LD 0x00
#define BPF_LDX 0x01
#define BPF_ST 0x02
#define BPF_STX 0x03
#define BPF_ALU 0x04
#define BPF_JMP 0x05
#define BPF_RET 0x06
#define BPF_MISC 0x07
#define BPF_SIZE(code) ((code) & 0x18)
#define BPF_W 0x00
#define BPF_H 0x08
#define BPF_B 0x10
#define BPF_MODE(code) ((code) & 0xe0)
#define BPF_IMM 0x00
#define BPF_ABS 0x20
#define BPF_IND 0x40
#define BPF_MEM 0x60
#define BPF_LEN 0x80
#define BPF_MSH 0xa0
#define BPF_OP(code) ((code) & 0xf0)
#define BPF_ADD 0x00
#define BPF_SUB 0x10
#define BPF_MUL 0x20
#define BPF_DIV 0x30
#define BPF_OR 0x40
#define BPF_AND 0x50
#define BPF_LSH 0x60
#define BPF_RSH 0x70
#define BPF_NEG 0x80
#define BPF_JA 0x00
#define BPF_JEQ 0x10
#define BPF_JGT 0x20
#define BPF_JGE 0x30
#define BPF_JSET 0x40
#define BPF_CKMATCH_IMM 0x50
#define BPF_SRC(code) ((code) & 0x08)
#define BPF_K 0x00
#define BPF_X 0x08
#define BPF_RVAL(code) ((code) & 0x38)
#define BPF_A 0x10
#define BPF_MATCH_IMM 0x18
#define BPF_MATCH_DATA 0x20
#define BPF_MISCOP(code) ((code) & 0xf8)
#define BPF_TAX 0x00
#define BPF_TXA 0x80
#define BPF_KEY 0x10
#define BPF_REG_DATA 0x18
#define BPF_POSTPONE 0x20
struct bpf_insn {
unsigned short code;
unsigned char jt;
unsigned char jf;
int k;
};
typedef struct bpf_insn *bpf_insn_t;
#define NET_MAX_BPF ((NET_MAX_FILTER*sizeof(filter_t))/sizeof(struct bpf_insn))
#define BPF_STMT(code, k) { (unsigned short)(code), 0, 0, k }
#define BPF_JUMP(code, k, jt, jf) { (unsigned short)(code), jt, jf, k }
#define BPF_RETMATCH(code, k, nkey) { (unsigned short)(code), nkey, 0, k }
#define BPF_INSN_STMT(pc, c, n) \
do { \
(pc)->code = (c); \
(pc)->jt = (pc)->jf = 0; \
(pc)->k = (n); \
(pc)++; \
} while(0)
#define BPF_INSN_JUMP(pc, c, n, jtrue, jfalse) \
do { \
(pc)->code = (c); \
(pc)->jt = (jtrue); \
(pc)->jf = (jfalse); \
(pc)->k = (n); \
(pc)++; \
} while(0)
#define BPF_INSN_RETMATCH(pc, c, n, nkey) \
do { \
(pc)->code = (c); \
(pc)->jt = (nkey); \
(pc)->jf = 0; \
(pc)->k = (n); \
(pc)++; \
} while(0)
#define BPF_MEMWORDS 16
#define BPF_DLBASE (1<<30)
#define BPF_BYTES(n) ((n) * sizeof (struct bpf_insn))
#define BPF_BYTES2LEN(n) ((n) / sizeof (struct bpf_insn))
#define BPF_INSN_EQ(p,q) ((p)->code == (q)->code && \
(p)->jt == (q)->jt && \
(p)->jf == (q)->jf && \
(p)->k == (q)->k)
#endif