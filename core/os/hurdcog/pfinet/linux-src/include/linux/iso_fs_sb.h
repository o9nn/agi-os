#ifndef _ISOFS_FS_SB
#define _ISOFS_FS_SB
struct isofs_sb_info {
unsigned long s_ninodes;
unsigned long s_nzones;
unsigned long s_firstdatazone;
unsigned long s_log_zone_size;
unsigned long s_max_size;
unsigned char s_high_sierra;
unsigned char s_mapping;
unsigned char s_rock;
unsigned char s_joliet_level;
unsigned char s_utf8;
unsigned char s_cruft;
unsigned char s_unhide;
unsigned char s_nosuid;
unsigned char s_nodev;
mode_t s_mode;
gid_t s_gid;
uid_t s_uid;
struct nls_table *s_nls_iocharset;
};
#endif