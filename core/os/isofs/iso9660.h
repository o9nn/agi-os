struct voldesc
{
unsigned char type;
unsigned char id[5];
unsigned char version;
unsigned char data[0];
};
#define VOLDESC_PRIMARY 1
#define VOLDESC_END 255
#define ISO_STANDARD_ID "CD001"
struct sblock
{
unsigned char type;
unsigned char id[5];
unsigned char version;
unsigned char skip1;
unsigned char sysid[32];
unsigned char volid[32];
unsigned char skip2[8];
unsigned char vol_sp_size[8];
unsigned char skip[32];
unsigned char vol_set_size[4];
unsigned char vol_seqno[4];
unsigned char blksize[4];
unsigned char ptsize[8];
unsigned char type_l_pt[4];
unsigned char opt_type_l_pt[4];
unsigned char type_m_pt[4];
unsigned char opt_type_m_pt[4];
unsigned char root[34];
unsigned char volset_id[128];
unsigned char pub_id[128];
unsigned char prep_id[128];
unsigned char app_id[128];
unsigned char copyr_id[37];
unsigned char abstr_id[37];
unsigned char biblio_id[37];
unsigned char creation_time[17];
unsigned char mod_time[17];
unsigned char expir_time[17];
unsigned char effect_time[17];
unsigned char file_structure;
unsigned char skip4;
unsigned char appl_data[512];
unsigned char skip5[652];
};
struct dirrect
{
unsigned char len;
unsigned char ext_attr_len;
unsigned char extent[8];
unsigned char size[8];
unsigned char date[7];
unsigned char flags;
unsigned char file_unit_size;
unsigned char ileave;
unsigned char vol_seqno[4];
unsigned char namelen;
unsigned char name[0];
};
#include <endian.h>
static inline unsigned int
isonum_733 (unsigned char *addr)
{
return addr[0] | (addr[1] << 8) | (addr[2] << 16) |
(((unsigned int) addr[3]) << 24);
}
static inline unsigned int
isonum_723 (unsigned char *addr)
{
return addr[0] | (addr[1] << 8);
}