#ifndef _DDB_NLIST_H_
#define _DDB_NLIST_H_
struct	nlist {
union n_un {
char	*n_name;
long	n_strx;
} n_un;
unsigned char n_type;
unsigned char n_other;
short	n_desc;
#if alpha
int	n_pad;
#endif
vm_offset_t n_value;
};
#define	N_UNDF	0
#define	N_ABS	2
#define	N_TEXT	4
#define	N_DATA	6
#define	N_BSS	8
#define	N_FN	0x1f
#define	N_EXT	1
#define	N_TYPE	0x1e
#define	N_STAB	0xe0
#endif