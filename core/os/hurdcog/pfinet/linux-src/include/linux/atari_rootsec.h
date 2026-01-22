#ifndef _LINUX_ATARI_ROOTSEC_H
#define _LINUX_ATARI_ROOTSEC_H
struct partition_info
{
u_char flg;
char id[3];
u32 st;
u32 siz;
};
struct rootsector
{
char unused[0x156];
struct partition_info icdpart[8];
char unused2[0xc];
u32 hd_siz;
struct partition_info part[4];
u32 bsl_st;
u32 bsl_cnt;
u16 checksum;
} __attribute__ ((__packed__));
#endif