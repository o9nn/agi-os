#ifndef _SYSV_FS_SB
#define _SYSV_FS_SB
struct sysv_sb_info {
int s_type;
unsigned int s_block_size;
unsigned int s_block_size_1;
unsigned int s_block_size_bits;
unsigned int s_block_size_inc_bits;
unsigned int s_block_size_dec_bits;
char s_convert;
char s_kludge_symlinks;
char s_truncate;
nlink_t s_link_max;
unsigned int s_inodes_per_block;
unsigned int s_inodes_per_block_1;
unsigned int s_inodes_per_block_bits;
unsigned int s_ind_per_block;
unsigned int s_ind_per_block_1;
unsigned int s_ind_per_block_bits;
unsigned int s_ind_per_block_2;
unsigned int s_ind_per_block_2_1;
unsigned int s_ind_per_block_2_bits;
unsigned int s_ind_per_block_3;
unsigned int s_ind_per_block_block_size_1;
unsigned int s_ind_per_block_block_size_bits;
unsigned int s_ind_per_block_2_block_size_1;
unsigned int s_ind_per_block_2_block_size_bits;
unsigned int s_ind0_size;
unsigned int s_ind1_size;
unsigned int s_ind2_size;
unsigned int s_toobig_block;
unsigned int s_block_base;
unsigned short s_fic_size;
unsigned short s_flc_size;
struct buffer_head *s_bh1;
struct buffer_head *s_bh2;
char * s_sbd1;
char * s_sbd2;
u16 *s_sb_fic_count;
u16 *s_sb_fic_inodes;
u16 *s_sb_total_free_inodes;
u16 *s_sb_flc_count;
u32 *s_sb_flc_blocks;
u32 *s_sb_total_free_blocks;
u32 *s_sb_time;
u32 *s_sb_state;
u32 s_firstinodezone;
u32 s_firstdatazone;
u32 s_ninodes;
u32 s_ndatazones;
u32 s_nzones;
};
#define sv_type u.sysv_sb.s_type
#define sv_block_size u.sysv_sb.s_block_size
#define sv_block_size_1 u.sysv_sb.s_block_size_1
#define sv_block_size_bits u.sysv_sb.s_block_size_bits
#define sv_block_size_inc_bits u.sysv_sb.s_block_size_inc_bits
#define sv_block_size_dec_bits u.sysv_sb.s_block_size_dec_bits
#define sv_convert u.sysv_sb.s_convert
#define sv_kludge_symlinks u.sysv_sb.s_kludge_symlinks
#define sv_truncate u.sysv_sb.s_truncate
#define sv_link_max u.sysv_sb.s_link_max
#define sv_inodes_per_block u.sysv_sb.s_inodes_per_block
#define sv_inodes_per_block_1 u.sysv_sb.s_inodes_per_block_1
#define sv_inodes_per_block_bits u.sysv_sb.s_inodes_per_block_bits
#define sv_ind_per_block u.sysv_sb.s_ind_per_block
#define sv_ind_per_block_1 u.sysv_sb.s_ind_per_block_1
#define sv_ind_per_block_bits u.sysv_sb.s_ind_per_block_bits
#define sv_ind_per_block_2 u.sysv_sb.s_ind_per_block_2
#define sv_ind_per_block_2_1 u.sysv_sb.s_ind_per_block_2_1
#define sv_ind_per_block_2_bits u.sysv_sb.s_ind_per_block_2_bits
#define sv_ind_per_block_3 u.sysv_sb.s_ind_per_block_3
#define sv_ind_per_block_block_size_1 u.sysv_sb.s_ind_per_block_block_size_1
#define sv_ind_per_block_block_size_bits u.sysv_sb.s_ind_per_block_block_size_bits
#define sv_ind_per_block_2_block_size_1 u.sysv_sb.s_ind_per_block_2_block_size_1
#define sv_ind_per_block_2_block_size_bits u.sysv_sb.s_ind_per_block_2_block_size_bits
#define sv_ind0_size u.sysv_sb.s_ind0_size
#define sv_ind1_size u.sysv_sb.s_ind1_size
#define sv_ind2_size u.sysv_sb.s_ind2_size
#define sv_toobig_block u.sysv_sb.s_toobig_block
#define sv_block_base u.sysv_sb.s_block_base
#define sv_fic_size u.sysv_sb.s_fic_size
#define sv_flc_size u.sysv_sb.s_flc_size
#define sv_bh1 u.sysv_sb.s_bh1
#define sv_bh2 u.sysv_sb.s_bh2
#define sv_sbd1 u.sysv_sb.s_sbd1
#define sv_sbd2 u.sysv_sb.s_sbd2
#define sv_sb_fic_count u.sysv_sb.s_sb_fic_count
#define sv_sb_fic_inodes u.sysv_sb.s_sb_fic_inodes
#define sv_sb_total_free_inodes u.sysv_sb.s_sb_total_free_inodes
#define sv_sb_flc_count u.sysv_sb.s_sb_flc_count
#define sv_sb_flc_blocks u.sysv_sb.s_sb_flc_blocks
#define sv_sb_total_free_blocks u.sysv_sb.s_sb_total_free_blocks
#define sv_sb_time u.sysv_sb.s_sb_time
#define sv_sb_state u.sysv_sb.s_sb_state
#define sv_firstinodezone u.sysv_sb.s_firstinodezone
#define sv_firstdatazone u.sysv_sb.s_firstdatazone
#define sv_ninodes u.sysv_sb.s_ninodes
#define sv_ndatazones u.sysv_sb.s_ndatazones
#define sv_nzones u.sysv_sb.s_nzones
#endif