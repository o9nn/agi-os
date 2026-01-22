#ifndef __EFS_VH_H__
#define __EFS_VH_H__
#define VHMAGIC		0xbe5a941
#define NPARTAB		16
#define NVDIR		15
#define BFNAMESIZE	16
#define VDNAMESIZE	8
struct volume_directory {
char	vd_name[VDNAMESIZE];
int	vd_lbn;
int	vd_nbytes;
};
struct partition_table {
int	pt_nblks;
int	pt_firstlbn;
int	pt_type;
};
struct volume_header {
int	vh_magic;
short	vh_rootpt;
short	vh_swappt;
char	vh_bootfile[BFNAMESIZE];
char	pad[48];
struct volume_directory vh_vd[NVDIR];
struct partition_table  vh_pt[NPARTAB];
int	vh_csum;
int	vh_fill;
};
#define SGI_SYSV	0x05
#define SGI_EFS		0x07
#define IS_EFS(x)	(((x) == SGI_EFS) || ((x) == SGI_SYSV))
struct pt_types {
int	pt_type;
char	*pt_name;
} sgi_pt_types[] = {
{0x00,		"SGI vh"},
{0x01,		"SGI trkrepl"},
{0x02,		"SGI secrepl"},
{0x03,		"SGI raw"},
{0x04,		"SGI bsd"},
{SGI_SYSV,	"SGI sysv"},
{0x06,		"SGI vol"},
{SGI_EFS,	"SGI efs"},
{0x08,		"SGI lv"},
{0x09,		"SGI rlv"},
{0x0A,		"SGI xfs"},
{0x0B,		"SGI xfslog"},
{0x0C,		"SGI xlv"},
{0x82,		"Linux swap"},
{0x83,		"Linux native"},
{0,		NULL}
};
#endif