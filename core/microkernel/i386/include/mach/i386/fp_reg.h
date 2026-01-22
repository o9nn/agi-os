#ifndef	_MACH_I386_FP_REG_H_
#define	_MACH_I386_FP_REG_H_
struct i386_fp_save	{
unsigned short	fp_control;
unsigned short	fp_unused_1;
unsigned short	fp_status;
unsigned short	fp_unused_2;
unsigned short	fp_tag;
unsigned short	fp_unused_3;
unsigned int	fp_eip;
unsigned short	fp_cs;
unsigned short	fp_opcode;
unsigned int	fp_dp;
unsigned short	fp_ds;
unsigned short	fp_unused_4;
};
struct i386_fp_regs {
unsigned short	fp_reg_word[8][5];
};
#define XSAVE_XCOMP_BV_COMPACT (((unsigned long long)1) << 63)
struct i386_xfp_xstate_header {
unsigned long long	xfp_features;
unsigned long long	xcomp_bv;
unsigned long long	reserved[6];
} __attribute__((packed));
struct i386_xfp_save {
unsigned short	fp_control;
unsigned short	fp_status;
unsigned short	fp_tag;
unsigned short	fp_opcode;
unsigned int	fp_eip;
unsigned short	fp_cs;
unsigned short	fp_eip3;
unsigned int	fp_dp;
unsigned short	fp_ds;
unsigned short	fp_dp3;
unsigned int	fp_mxcsr;
unsigned int	fp_mxcsr_mask;
unsigned char	fp_reg_word[8][16];
unsigned char	fp_xreg_word[16][16];
unsigned int	padding[24];
struct i386_xfp_xstate_header header;
unsigned char	extended[0];
} __attribute__((packed, aligned(64)));
#define	FPC_IE		0x0001
#define FPC_IM		FPC_IE
#define	FPC_DE		0x0002
#define FPC_DM		FPC_DE
#define	FPC_ZE		0x0004
#define FPC_ZM		FPC_ZE
#define	FPC_OE		0x0008
#define FPC_OM		FPC_OE
#define	FPC_UE		0x0010
#define	FPC_PE		0x0020
#define	FPC_PC		0x0300
#define	FPC_PC_24	0x0000
#define	FPC_PC_53	0x0200
#define	FPC_PC_64	0x0300
#define	FPC_RC		0x0c00
#define	FPC_RC_RN	0x0000
#define	FPC_RC_RD	0x0400
#define	FPC_RC_RU	0x0800
#define	FPC_RC_CHOP	0x0c00
#define	FPC_IC		0x1000
#define	FPC_IC_PROJ	0x0000
#define	FPC_IC_AFF	0x1000
#define	FPS_IE		0x0001
#define	FPS_DE		0x0002
#define	FPS_ZE		0x0004
#define	FPS_OE		0x0008
#define	FPS_UE		0x0010
#define	FPS_PE		0x0020
#define	FPS_SF		0x0040
#define	FPS_ES		0x0080
#define	FPS_C0		0x0100
#define	FPS_C1		0x0200
#define	FPS_C2		0x0400
#define	FPS_TOS		0x3800
#define	FPS_TOS_SHIFT	11
#define	FPS_C3		0x4000
#define	FPS_BUSY	0x8000
#define	FP_NO		0
#define	FP_SOFT		1
#define	FP_287		2
#define	FP_387		3
#define	FP_387FX	4
#define	FP_387X		5
#endif