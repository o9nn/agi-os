#define	CLKIN	72000000
#define	IMMR		0xf0000000
#define	FLASHMEM	0xfe000000
#define	FLASHSIZE	0x01000000
#define	DSP1BASE		0xfc000000
#define	DSP1SIZE		0x01000000
#define	MEM1BASE	0x00000000
#define	MEM1SIZE	0x04000000
#define	DSP2BASE		0x04000000
#define	DSP2SIZE		0x01000000
#define	MEM2BASE	0x05000000
#define	MEM2SIZE	0
#define	FPGABASE		0xe0000000
#define	FPGASIZE		0x01000000
#define	PLAN9INI		0x00460000
#define	TLBENTRIES	32
#define	PTEVALID		PTE1_M
#define	PTEWRITE		(PTE1_RW|PTE1_C)
#define	PTERONLY	PTE1_RO
#define	PTEUNCACHED	PTE1_I
#define	SMC1PORT	3
#define	SMTXD1		BIT(9)
#define	SMRXD1		BIT(8)
#define	A1txer	 	0x00000004
#define	A1rxdv	 	0x00000010
#define	A1txen		 0x00000008
#define	A1rxer		 0x00000020
#define	A1col		 0x00000001
#define	A1crs		 0x00000002
#define	A1txdat		 0x00003c00
#define	A1rxdat		 0x0003c000
#define	B2txer		 0x00000001
#define	B2rxdv		 0x00000002
#define	B2txen		 0x00000004
#define	B2rxer		 0x00000008
#define	B2col		 0x00000010
#define	B2crs		 0x00000020
#define	B2txdat		 0x000003c0
#define	B2rxdat		 0x00003c00
#define	B3rxdv		 0x00004000
#define	B3rxer		 0x00008000
#define	B3txer		 0x00010000
#define	B3txen		 0x00020000
#define	B3col		 0x00040000
#define	B3crs		 0x00080000
#define	B3txdat		 0x0f000000
#define	B3rxdat		 0x00f00000
#define	A1psor0		 (A1rxdat | A1txdat)
#define	A1psor1		 (A1col | A1crs | A1txer | A1txen | A1rxdv | A1rxer)
#define	A1dir0		 (A1rxdat | A1crs | A1col | A1rxer | A1rxdv)
#define	A1dir1		 (A1txdat | A1txen | A1txer)
#define	B2psor0		 (B2rxdat | B2txdat | B2crs | B2col | B2rxer | B2rxdv | B2txer)
#define	B2psor1		 (B2txen)
#define	B2dir0		 (B2rxdat | B2crs | B2col | B2rxer | B2rxdv)
#define	B2dir1		 (B2txdat | B2txen | B2txer)
#define	B3psor0		 (B3rxdat | B3txdat | B3crs | B3col | B3rxer | B3rxdv | B3txer | B3txen)
#define	B3psor1		 0
#define	B3dir0		 (B3rxdat | B3crs | B3col | B3rxer | B3rxdv)
#define	B3dir1		 (B3txdat | B3txen | B3txer)